library(data.table)
library(ggplot2)
# Keeping only website activity
user_attr <- c(2:9, 11)
activity_data_filtered <- activity_data[, -user_attr]

# Dropping features
drop_features_wa <- c('Orders', 'Units', 'Date_time_col', 'Rev.binary')
activity_data_filtered <- activity_data_filtered[, !colnames(activity_data_filtered) %in% drop_features_wa]

# Aggregate at account/season level
filtered_wa <- as.data.table(activity_data_filtered)
filtered_wa <- filtered_wa[, lapply(.SD, sum), by=c('Retailer.Account.Number..v3...evar3.', 'Season..v8...evar8.')]

# Aggregate at account level
filtered_wa <- filtered_wa[, -2]
filtered_wa <- filtered_wa[, lapply(.SD, mean), by=c('Retailer.Account.Number..v3...evar3.')]

predictors <- setdiff(colnames(filtered_wa), c('Retailer.Account.Number..v3...evar3.', 'Revenue'))
response <- 'Revenue'

# Create two different models
filtered_wa_1 <- filtered_wa[filtered_wa$Revenue < 1e5, ]
filtered_wa_2 <- filtered_wa[filtered_wa$Revenue >= 1e5, ]



#### Small accounts ####

wa_data_1 <- as.h2o(filtered_wa_1)
wa_splits_1 <- h2o.splitFrame(wa_data_1, c(0.7, 0.15))
wa_data_train_1 <- wa_splits_1[[1]]
wa_data_valid_1 <- wa_splits_1[[2]]
wa_data_test_1 <- wa_splits_1[[3]]

### Random Forest ###

small_rf_hyper_params <- list(max_depth = seq(10,50,5))

small_rf_grid <- h2o.grid(
  hyper_params = small_rf_hyper_params,
  search_criteria = list(strategy = 'Cartesian'),
  algorithm = 'randomForest',
  grid_id = 'depth_grid',
  
  #Std parameters
  x = predictors,
  y = response, 
  training_frame = wa_data_train_1,
  validation_frame = wa_data_valid_1,
  ntrees = 500,
  
  # Stopping criteria
  stopping_rounds = 5,
  stopping_tolerance = 0.001,
  stopping_metric = 'RMSE',
  score_tree_interval = 10,
  
  seed = 1234
)

small_rf_grid
sortedGrid <- h2o.getGrid('depth_grid', sort_by = 'residual_deviance'); sortedGrid

for (i in 1:6) {
  small_rf <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.performance(small_rf, newdata = wa_data_test_1))
  # RMSE:  13946.74
  # MAE:  8228.941
}

small_rf <- h2o.getModel(sortedGrid@model_ids[[1]])

small_rf_pred <- h2o.predict(small_rf, newdata = wa_data_test_1)
small_res <- h2o.cbind(small_rf_pred, wa_data_test_1[, 'Revenue'])
head(small_res)

# Median error
zero_rev <- small_res[, 'Revenue'] == 0
h2o.median(h2o.abs(small_res[, 'predict']/(small_res[, 'Revenue'] + 1) - 1))
# [1] 0.8224063

# Keep only important predictors
small_rf_vi <- as.data.frame(h2o.varimp(small_rf))
small_rf_imp <- small_rf_vi[small_rf_vi[, 'percentage'] >= 0.01, 'variable']; small_rf_imp

small_rf_vi$variable <- reorder(small_rf_vi$variable, -small_rf_vi$percentage)
ggplot(small_rf_vi[1:10,], aes(x = variable, y = percentage*100)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))


small_rf_updated <- h2o.randomForest(x = small_rf_imp, y = response,
                                     training_frame = h2o.rbind(wa_data_train_1, wa_data_valid_1, wa_data_test_1),
                                     nfolds = 10, ntrees = 500, max_depth = 20, seed = 1234)
small_rf_updated@model$cross_validation_metrics
# RMSE:  14327.36
# MAE:  8351.531

small_rf_vi_updated <- as.data.frame(h2o.varimp(small_rf_updated))

# Better names
small_rf_vi_updated[, 'variable'] <- 
  c('Visits', 'Cart Views', 'Wrkspc Entered', 'Add Pdt Assrtmt', 'Ord Upld Success',
  'Wrkspc Created', 'Carts', 'Add Assrtmt Cart', 'Rmvd Pdt Assrtmt',
  'Pdt Views', 'Srch Res Retnd', 'Filter Res Retnd', 'Add Sim Clr Pdt',
  'Add Sim Stl Pdt', 'Del Ord', 'Cpy Ord', 'Zero Srch Res Rtrnd',
  'Rmvd Assrtmt Cart', 'Add All Pdt Assrtmt', 'Apply') 

small_rf_vi_updated$variable <- reorder(small_rf_vi_updated$variable, -small_rf_vi_updated$percentage)
ggplot(small_rf_vi_updated[1:10,], aes(x = variable, y = scaled_importance)) +
  geom_bar(stat = 'identity') + labs(x = 'Variable', y = 'Relative Importance') +
  my_plot_theme() +
  theme(axis.text.x = element_text(angle = 90))
    



#### Large accounts ####

wa_data_2 <- as.h2o(filtered_wa_2)
wa_splits_2 <- h2o.splitFrame(wa_data_2, c(0.7, 0.15))
wa_data_train_2 <- wa_splits_2[[1]]
wa_data_valid_2 <- wa_splits_2[[2]]
wa_data_test_2 <- wa_splits_2[[3]]

### Random Forest ###

large_rf_hyper_params <- list(max_depth = seq(10,50,5))

large_rf_grid <- h2o.grid(
  hyper_params = large_rf_hyper_params,
  search_criteria = list(strategy = 'Cartesian'),
  algorithm = 'randomForest',
  grid_id = 'large_depth_grid',
  
  #Std parameters
  x = predictors,
  y = response, 
  training_frame = wa_data_train_2,
  validation_frame = wa_data_valid_2,
  ntrees = 500,
  
  # Stopping criteria
  stopping_rounds = 5,
  stopping_tolerance = 0.001,
  stopping_metric = 'RMSE',
  score_tree_interval = 10,
  
  seed = 1234
)

large_rf_grid
sortedGrid_large <- h2o.getGrid('large_depth_grid', sort_by = 'residual_deviance'); sortedGrid_large

for (i in 1:6) {
  large_rf <- h2o.getModel(sortedGrid_large@model_ids[[i]])
  print(h2o.performance(large_rf, newdata = wa_data_test_2))
  # RMSE:  2250614
  # MAE:  665029.6
}

large_rf <- h2o.getModel(sortedGrid_large@model_ids[[1]])
print(h2o.performance(large_rf, newdata = wa_data_test_2))
# RMSE:  2250614
# MAE:  665029.6

large_rf_pred <- h2o.predict(large_rf, newdata = wa_data_test_2)
head(h2o.cbind(large_rf_pred, wa_data_test_2[, 'Revenue']), 50)

# Keep only important predictors
large_rf_vi <- as.data.frame(h2o.varimp(large_rf))
large_rf_imp <- large_rf_vi[large_rf_vi[, 'percentage'] >= 0.01, 'variable']; large_rf_imp

large_rf_vi$variable <- reorder(large_rf_vi$variable, -large_rf_vi$percentage)
ggplot(large_rf_vi[1:10,], aes(x = variable, y = scaled_importance)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))

large_rf_updated <- h2o.randomForest(x = large_rf_imp, y = response,
                                     training_frame = h2o.rbind(wa_data_train_2, wa_data_valid_2, wa_data_test_2),
                                     nfolds = 10, ntrees = 500, max_depth = 10, seed = 1234)
large_rf_updated@model$cross_validation_metrics
# RMSE:  3768796
# MAE:  790605.1

large_rf_vi_updated <- as.data.frame(h2o.varimp(large_rf_updated))

# Better names
large_rf_vi_updated[, 'variable'] <- 
  c('Ord Upld Success', 'Order Saved', 'Add All Pdt to Assrtmt',
    'Ord Upld Failed', 'Cart Views', 'Wrkspc Created', 'Wrkspc Entered', 'Visits',
    'Add Sim Col Pdt', 'Cpy Ord', 'Carts', 'Add Assrtmt Cart',
    'Pdt Views', 'Del Ord', 'Rmvd Pdt Assrtmt', 'Filter Res Rtrnd',
    'Add Sim Stl Pdt', 'Add Pdt Assrtmt', 'Clear Filters', 'Srch Res Rtrnd',
    'Rem Assrtmt Cart') 

large_rf_vi_updated$variable <- reorder(large_rf_vi_updated$variable, -large_rf_vi_updated$percentage)
ggplot(large_rf_vi_updated[1:10,], aes(x = variable, y = scaled_importance)) +
  geom_bar(stat = 'identity') + labs(x = 'Variable', y = 'Relative Importance') +
  my_plot_theme() +
  theme(axis.text.x = element_text(angle = 90))




###################################################

# <1e4 model

#### Small accounts ####


wa_data_3 <- as.h2o(filtered_wa[filtered_wa$Revenue < 1e4,])
wa_splits_3 <- h2o.splitFrame(wa_data_3, c(0.7, 0.15))
wa_data_train_3 <- wa_splits_3[[1]]
wa_data_valid_3 <- wa_splits_3[[2]]
wa_data_test_3 <- wa_splits_3[[3]]

### Random Forest ###

smal3_rf_hyper_params <- list(max_depth = seq(10,50,5))

smal3_rf_grid <- h2o.grid(
  hyper_params = smal3_rf_hyper_params,
  search_criteria = list(strategy = 'Cartesian'),
  algorithm = 'randomForest',
  grid_id = 'depth_grid3',
  
  #Std parameters
  x = predictors,
  y = response, 
  training_frame = wa_data_train_3,
  validation_frame = wa_data_valid_3,
  ntrees = 500,
  
  # Stopping criteria
  stopping_rounds = 5,
  stopping_tolerance = 0.001,
  stopping_metric = 'RMSE',
  score_tree_interval = 10,
  
  seed = 1234
)

smal3_rf_grid
sort3dGrid <- h2o.getGrid('depth_grid3', sort_by = 'residual_deviance')



smal3_rf <- h2o.getModel(sort3dGrid@model_ids[[3]])
print(h2o.performance(smal3_rf, newdata = wa_data_test_3))
# RMSE:  1886.59
# MAE:  1230.683

smal3_rf_pred <- h2o.predict(smal3_rf, newdata = wa_data_test_3)
tail(h2o.cbind(smal3_rf_pred, wa_data_test_3[, 'Revenue']))


# Keep only important predictors
smal3_rf_vi <- as.data.frame(h2o.varimp(smal3_rf))
smal3_rf_imp <- smal3_rf_vi[smal3_rf_vi[, 'percentage'] >= 0.01, 'variable']; smal3_rf_imp

smal3_rf_vi$variable <- reorder(smal3_rf_vi$variable, -smal3_rf_vi$percentage)
ggplot(smal3_rf_vi[1:10,], aes(x = variable, y = percentage*100)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))


small_rf_updated <- h2o.randomForest(x = small_rf_imp, y = response,
                                     training_frame = h2o.rbind(wa_data_train_1, wa_data_valid_1),
                                     nfolds = 10, ntrees = 500, max_depth = 20, seed = 1234)
small_rf_updated@model$cross_validation_metrics
# RMSE:  14327.36
# MAE:  8351.531

small_rf_vi_updated <- as.data.frame(h2o.varimp(small_rf_updated))

small_rf_vi_updated$variable <- reorder(small_rf_vi_updated$variable, -small_rf_vi_updated$percentage)
ggplot(small_rf_vi_updated[1:10,], aes(x = variable, y = percentage*100)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))






##################################################

### GBM ###
wa_gbm_model <- h2o.gbm(y = 'Revenue', training_frame = wa_data_train[,-1], validation_frame = wa_data_valid[,-1],
                        ntrees = 10000, learn_rate = 0.01, 
                        col_sample_rate = 0.8, stopping_rounds = 5, stopping_metric = 'MSE',
                        seed = 1234, score_tree_interval = 10)

wa_gbm_model@model$validation_metrics
wa_gbm_vi <- as.data.frame(h2o.varimp(wa_gbm_model))
gbm_imp_predictors <- wa_gbm_vi[wa_gbm_vi[, 'percentage'] >= 0.01, 'variable']; gbm_imp_predictors

wa_gbm_pred <- h2o.predict(wa_gbm_model, newdata = wa_data_test[,-1])
rmse <- h2o.sqrt(h2o.mean((wa_gbm_pred - wa_data_test[, 'Revenue'])^2)); rmse
# [1] 481952
mae <- h2o.mean(h2o.abs(wa_gbm_pred - wa_data_test[, 'Revenue'])); mae
# [1] 130815.9

gbm_hyper_params <- list(max_depth = 1:30)
grid <- h2o.grid(
  hyper_params = gbm_hyper_params,
  search_criteria = list(strategy = 'Cartesian'),
  algorithm = 'gbm',
  grid_id = 'depth_grid',
  
  #Std parameters
  x = predictors,
  y = response, 
  training_frame = wa_data_train, validation_frame = wa_data_valid,
  ntrees = 10000,
  learn_rate = 0.05,
  learn_rate_annealing = 0.99,
  col_sample_rate = 0.8,
  
  seed = 1234,
  
  # Stopping criteria
  stopping_rounds = 5,
  stopping_tolerance = 0.001,
  stopping_metric = 'MSE',
  score_tree_interval = 10
)
grid
maxDepth <- 28; minDepth <- 16

gbm_hyper_params <- list(
  max_depth = seq(minDepth, maxDepth, 1),
  sample_rate = seq(0.2, 1, 0.01),
  col_sample_rate = seq(0.2, 1, 0.01),
  col_sample_rate_per_tree = seq(0.2, 1, 0.01),
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  min_rows = 2^seq(0,log2(nrow(wa_data_train))-1,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)

search_criteria <- list(
  strategy = 'RandomDiscrete',
  # max_runtime_secs <- 3600,
  # max_models = 100
  seed = 1234,
  stopping_rounds = 5,
  stopping_metric = "MSE",
  stopping_tolerance = 1e-3
)

grid_final <- h2o.grid(
  hyper_params = gbm_hyper_params,
  search_criteria = search_criteria,
  algorithm = 'gbm',
  grid_id = 'final_grid',
  
  x = predictors,
  y = response,
  training_frame = wa_data_train,
  validation_frame = wa_data_valid,
  ntrees = 10000,
  learn_rate = 0.05,
  learn_rate_annealing = 0.99,

  # max_runtime_secs = 3600,
  seed = 1234,

  # Stopping criteria
  stopping_rounds = 5,
  stopping_tolerance = 0.0001,
  stopping_metric = 'MSE',
  score_tree_interval = 10
)
grid_final


# GGPLOT FORMAT FUNCTION
my_plot_theme = function(){
  font_family = "Helvetica"
  font_face = "bold"
  
  return(theme(
    axis.text.x = element_text(size = 18, 
                               face = font_face, 
                               family = font_family),
    axis.text.y = element_text(size = 18, 
                               face = font_face, 
                               family = font_family),
    axis.title.x = element_text(size = 20, 
                                face = font_face, 
                                family = font_family),
    axis.title.y = element_text(size = 20, face = font_face, family = font_family),
    strip.text.y = element_text(size = 18, face = font_face, family = font_family),
    plot.title = element_text(size = 24, face = font_face, family = font_family),
    legend.position = "top",
    legend.title = element_text(colour = "white", size = 16,
                                face = font_face,
                                family = font_family),
    legend.text = element_text(colour = "white", size = 14,
                               face = font_face,
                               family = font_family)))
}
