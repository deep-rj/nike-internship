# library(data.table)
### 'Data Cleaning_H2O.R' to be run prior to this to setup the data

#########################
### Categorical response
#########################

# Feature engineering
cat_resp_drop_cols <- c("Revenue", "Date_time_col", "Orders", "Units", "Retailer Account Name (v2) (evar2)")
cat_resp_data <- activity_data[, !colnames(activity_data) %in% cat_resp_drop_cols]

# Drop some additional columns
cat_drop_features <- c('Retailer Account Number (v3) (evar3)', 'Season (v8) (evar8)',
                       'Video: 25% Viewed (e27) (event27)', 'Video: 50% Viewed (e28) (event28)',
                       'Video: 75% Viewed (e29) (event29)', 'Video: 95% Viewed (e30) (event30)',
                       'Video Completes (e31) (event31)', 'Video Time Played (e25) (event25)',
                       'Video Views (e26) (event26)', '"Campaign Views', 'Tech Sheet Viewed (e15) (event15)',
                       'Learn More CTA Tout Clicked (e38) (event38)', 'Save Order (e40) (event40)',
                       'Carousel Image Clicked (e49) (event49)', 'Apply (e35) (event35)', 'Clear Filters (e39) (event39)',
                       'Removed Assortment from Cart (e11) (event11)', 'Order Upload Failed (e34) (event34)')
# 'Added Similar Style Product (e6) (event6)','Added Similar Color Product (e5) (event5)',
# 'Retailer Language (v4) (evar4)')
cat_resp_data <- cat_resp_data[, !colnames(cat_resp_data) %in% cat_drop_features]

# Split data into training and test datasets
cat_splits <- h2o.splitFrame(cat_resp_data, c(0.6,0.2), seed=1234)
cat_resp_train <- cat_splits[[1]]
cat_resp_valid <- cat_splits[[2]]
cat_resp_test <- cat_splits[[3]]

# Response variable
cat_resp_var <- "Rev.binary"


### 1. Random Forest

cat_resp_rf <- h2o.randomForest(y = cat_resp_var, training_frame = cat_resp_train, validation_frame = cat_resp_valid,
                                ntrees = 300, min_rows = 5)
summary(cat_resp_rf)
plot(cat_resp_rf)
cat_resp_rf@model$validation_metrics

as.data.frame(h2o.varimp(cat_resp_rf))
h2o.partialPlot(cat_resp_rf, data = cat_resp_train, cols = c('Cart Views'))

cat_resp_rf_pred <- h2o.predict(cat_resp_rf, newdata = cat_resp_test)
h2o.table(cat_resp_rf_pred[, 'predict'], cat_resp_test[, 'Rev.binary'])
# predict Rev.binary Counts
# 1       0          0  75327
# 2       0          1   2180
# 3       1          0   8346
# 4       1          1  11015

cat_resp_rf_test_acc <- (75327 + 11015) / nrow(cat_resp_test); cat_resp_rf_test_acc
# [1] 0.8913367

# Threshold
# h2o.min(cat_resp_rf_pred[cat_resp_rf_pred[,'predict'] == '1', 'p1'])
# [1] 0.3175536


### 2. Gradient Boosting Machine

cat_resp_gbm <- h2o.gbm(y = cat_resp_var, training_frame = h2o.rbind(cat_resp_train, cat_resp_valid),
                        ntrees = 10000, learn_rate = 0.01, stopping_rounds = 10, 
                        col_sample_rate = 0.5, seed = 1234, score_tree_interval = 10)
summary(cat_resp_gbm)

cat_resp_gbm_pred <- h2o.predict(cat_resp_gbm, newdata = cat_resp_test)
h2o.table(cat_resp_gbm_pred[, 'predict'], cat_resp_test[, 'Rev.binary'])
#     predict Rev.binary Counts
# 1       0          0 154077
# 2       0          1   8935
# 3       1          0  12135
# 4       1          1  15323

cat_resp_gbm_test_acc <- (154077 + 15323) / nrow(cat_resp_test)
cat_resp_gbm_test_acc
# [1] 0.8893789

# Threshold
# h2o.min(cat_resp_gbm_pred[cat_resp_gbm_pred[,'predict'] == '1', 'p1'])
# [1] 0.3160609



#####################
### Numeric response
#####################

#######################
## Aggregated features
#######################

# Prepare aggregated data
# Sort the dataset chronologically for each account/season
agg_activity_data <- activity_data
agg_activity_data <- agg_activity_data[, !colnames(agg_activity_data) %in% 'Retailer Account Name (v2) (evar2)']
agg_activity_data <- h2o.arrange(agg_activity_data, 'Retailer Account Number (v3) (evar3)',
                                 'Season (v8) (evar8)', 'Date_time_col')

agg_activity_data[, 'Rev.binary'] <- as.numeric(agg_activity_data[, 'Rev.binary'])

# Initialize sequence column
agg_activity_data[, 'seq'] <- 1

# Create order sequence column

create_order_seq <- function(df) {
  # Proceed only if more than one row
  if (nrow(df) > 1) {
    seq_counter <- 1
    
    for (i in 2:nrow(df)) {
      if (df[i-1, 'Rev.binary'] && !df[i, 'Rev.binary']) {
        # Transitioning from revenue to non-revenue row
        seq_counter <- seq_counter + 1
      }
      
      df[i, 'seq'] <- seq_counter
    }
  }
  
  df
}

agg_activity_data <- as.data.frame(agg_activity_data)
agg_keys <- unique(agg_activity_data[, c('Retailer.Account.Number..v3...evar3.',
                                            'Season..v8...evar8.')])

for (i in 1:nrow(agg_keys)) {
  cat('\r', paste('Processing key', i, 'of', nrow(agg_keys)))
  # Get account and season
  acct <- as.character(agg_keys[i, 'Retailer.Account.Number..v3...evar3.'])
  seas <- as.character(agg_keys[i, 'Season..v8...evar8.'])
  
  mask <- which(agg_activity_data[, 'Retailer.Account.Number..v3...evar3.'] == acct &
    agg_activity_data[, 'Season..v8...evar8.'] == seas)
  
  agg_activity_data[mask, ] <- create_order_seq(agg_activity_data[mask, ])
}

# Scale the revenue
agg_activity_data$Rev.k <- agg_activity_data$Revenue/1000

# Differentiate small and large customers
mil_club <- unique(agg_activity_data$Retailer.Account.Number..v3...evar3.[which(agg_activity_data$Revenue >= 1e6)])
agg_activity_data$mil_dollar_club <- 0
agg_activity_data$mil_dollar_club[which(agg_activity_data$Retailer.Account.Number..v3...evar3. %in% mil_club)] <- 1

# Aggregate columns based on account and sequence
group_by_cols <- c('Retailer.Account.Number..v3...evar3.', 'Channel', 'Country.Name',
                   'Retailer.Language..v4...evar4.', 'mil_dollar_club', "Season..v8...evar8.", 'seq')
num_resp_drop_cols <- c("Rev.binary", "Day.of.Week..v32...evar32.", "Days.Since.Last.Visit..v34...evar34.",
                        "New.Repeat.Visitor..v22...evar22.", "Role..v7...evar7.", "Revenue",
                        "Site.Section.Channel..v16...evar16.", "Date_time_col", "Orders", "Units")

agg_act_data_seq <- as.data.table(agg_activity_data)
agg_act_data_seq <- agg_act_data_seq[, (num_resp_drop_cols) := NULL]
agg_act_data_seq <- agg_act_data_seq[, lapply(.SD, sum), by=group_by_cols]

# Drop some additional columns
drop_features <- c('Retailer.Account.Number..v3...evar3.', 'Season..v8...evar8.',
                   'Video..25..Viewed..e27...event27.', 'Video..50..Viewed..e28...event28.',
                   'Video..75..Viewed..e29...event29.', 'Video..95..Viewed..e30...event30.',
                   'Video.Completes..e31...event31.', 'Video.Time.Played..e25...event25.',
                   'Video.Views..e26...event26.', 'Campaign.Views', 'Tech.Sheet.Viewed..e15...event15.',
                   'Learn.More.CTA.Tout.Clicked..e38...event38.', 'Save.Order..e40...event40.',
                   'Carousel.Image.Clicked..e49...event49.', 'Apply..e35...event35.', 'Clear.Filters..e39...event39.',
                   'Removed.Assortment.from.Cart..e11...event11.', 'Order.Upload.Failed..e34...event34.',
                   'Added.Similar.Style.Product..e6...event6.','Added.Similar.Color.Product..e5...event5.',
                   'Retailer.Language..v4...evar4.')
agg_act_data_seq <- agg_act_data_seq[, (drop_features) := NULL]

# Split data into training and test datasets
agg_act_data_seq <- as.h2o(agg_act_data_seq)
splits <- h2o.splitFrame(agg_act_data_seq, c(0.6,0.2), seed=1234)
num_resp_train <- splits[[1]]
num_resp_valid <- splits[[2]]
num_resp_test <- splits[[3]]

# Response variable
num_resp_var <- "Rev.k"


### 0. Simple mean

rev_mean <- h2o.mean(agg_act_data_seq[, num_resp_var])
rmse <- h2o.sqrt(h2o.mean((rev_mean - num_resp_test[, 'Rev.k'])^2)); rmse
# rmse
# [1] 522142.5
mae <- h2o.mean(h2o.abs(rev_mean - num_resp_test[, 'Rev.k'])); mae
# mae
# [1] 94319.06

### 1. Random Forest

num_resp_rf <- h2o.randomForest(y = num_resp_var, training_frame = num_resp_train, validation_frame = num_resp_valid,
                                ntrees = 300, min_rows = 5)
# summary(num_resp_rf)
num_resp_rf@model$validation_metrics

as.data.frame(h2o.varimp(num_resp_rf))

num_resp_rf_pred <- h2o.predict(num_resp_rf, newdata = num_resp_test)
rmse <- h2o.sqrt(h2o.mean((num_resp_rf_pred - num_resp_test[, 'Rev.k'])^2)); rmse
# [1] 432.7141
mae <- h2o.mean(h2o.abs(num_resp_rf_pred - num_resp_test[, 'Rev.k'])); mae
# [1] 56.99796

# Percentage error
# per_error <- h2o.mean(h2o.abs(1 - (num_resp_rf_pred / num_resp_test[, 'Rev.k']))); per_error
# [1] 6.618777

tail(h2o.cbind(num_resp_rf_pred, num_resp_test[, 'Rev.k']), 25)

### 2. Gradient Boosting Machine

num_resp_gbm <- h2o.gbm(y = num_resp_var, training_frame = num_resp_train, validation_frame = num_resp_valid)
summary(num_resp_gbm)

num_resp_gbm@model$validation_metrics

as.data.frame(h2o.varimp(num_resp_gbm))

num_resp_gbm_pred <- h2o.predict(num_resp_gbm, newdata = num_resp_test)
rmse <- h2o.sqrt(h2o.mean((num_resp_gbm_pred - num_resp_test[, 'Revenue'])^2)); rmse
# [1] 461415.8
mae <- h2o.mean(h2o.abs(num_resp_gbm_pred - num_resp_test[, 'Revenue'])); mae
# [1] 64239.53



num_resp_gbm_xval <- h2o.gbm(y = num_resp_var, training_frame = h2o.rbind(num_resp_train, num_resp_valid),
                             nfolds = 5)

num_resp_gbm_xval@model$cross_validation_metrics

as.data.frame(h2o.varimp(num_resp_gbm_xval))

num_resp_gbm_xval_pred <- h2o.predict(num_resp_gbm_xval, newdata = num_resp_test)
rmse <- h2o.sqrt(h2o.mean((num_resp_gbm_xval_pred - num_resp_test[, 'Revenue'])^2)); rmse
# [1] 449691.9
mae <- h2o.mean(h2o.abs(num_resp_gbm_xval_pred - num_resp_test[, 'Revenue'])); mae
# [1] 63018.93



num_resp_gbm_3 <- h2o.gbm(y = num_resp_var, training_frame = h2o.rbind(num_resp_train, num_resp_valid),
                          ntrees = 10000, learn_rate = 0.01, stopping_rounds = 10, 
                          col_sample_rate = 0.5, seed = 1234, score_tree_interval = 10)

as.data.frame(h2o.varimp(num_resp_gbm_3))

num_resp_gbm_3_pred <- h2o.predict(num_resp_gbm_3, newdata = num_resp_test)
rmse <- h2o.sqrt(h2o.mean((num_resp_gbm_3_pred - num_resp_test[, 'Revenue'])^2)); rmse
# [1] 469902.7
mae <- h2o.mean(h2o.abs(num_resp_gbm_3_pred - num_resp_test[, 'Revenue'])); mae
# [1] 66646.72



### 3. Linear Regression

num_resp_glm <- h2o.glm(y = num_resp_var, training_frame = num_resp_train, validation_frame = num_resp_valid,
                        family = 'gaussian')
h2o.performance(num_resp_glm)
summary(num_resp_glm)

num_resp_glm_pred <- h2o.predict(num_resp_glm, newdata = num_resp_test)
rmse <- h2o.sqrt(h2o.mean((num_resp_glm_pred - num_resp_test[, 'Rev.k'])^2)); rmse
# [1] 493.9613
mae <- h2o.mean(h2o.abs(num_resp_glm_pred - num_resp_test[, 'Rev.k'])); mae
# [1] 83.95081


### 4. Neural Network



######################
# Sales target binary
######################





# IDEAS
# 0. DONE - Check Order upload is correct or not / maybe try removing Country??
# 1. DONE - Remove categorical variables and see
# 2. Cluster activity variables
# 3. DONE - Try only modeling with < 1 mil revenue (maybe 2 or more splits)
# 4. Sales target binary
# 5. Try neural net


# PRESENTATION
# 1. Prediction results - 
#        Challenges - less data, multiple users in one account
# 2. Important variables (partial plots if possible)
# 3. Cluster results
# 4. NLP stuff?
