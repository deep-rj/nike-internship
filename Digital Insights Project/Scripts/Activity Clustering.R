library(archetypes)
# Drop account number from the aggregated dataset
metrics_data <- filtered_wa[, -1]
colnames(metrics_data)

# Drop some columns
cluster_cols_drop <- c('Apply..e35...event35.', 'Clear.Filters..e39...event39.',
                       'Order.Saved..Unsubmitted..e36...event36.', 'Revenue', 'Save.Order..e40...event40.',
                       'Tech.Sheet.Viewed..e15...event15.', 'Workspace.Enter..e17...event17.',
                       'Workspace.Created..e16...event16.', 'Visits', 'Carts', 'Cart.Views', 'Product.Views',
                       'Video..25..Viewed..e27...event27.')
metrics_data[, (cluster_cols_drop) := NULL]

aa <- stepArchetypes(metrics_data, k = 1:6, nrep = 5)
screeplot(aa)
rss(aa)

aa_best <- bestModel(aa[[4]])
round(t(parameters(aa_best)), 3)
aa_best_profile <- coef(aa_best)
aa_best_cluster <- max.col(aa_best_profile)
table(aa_best_cluster)

kcl <- kmeans(metrics_data, 10, nstart = 25, iter.max = 100)
table(kcl$cluster)
t(round(kcl$centers, 3))

std_metrics_data <- scale(metrics_data)

std_aa <- stepArchetypes(std_metrics_data, k = 1:10, nrep = 5)
screeplot(std_aa)
rss(std_aa)

std_aa_best <- bestModel(std_aa[[4]])
round(t(parameters(std_aa_best)), 3)
std_aa_best_profile <- coef(std_aa_best)
std_aa_best_cluster <- max.col(std_aa_best_profile)
table(std_aa_best_cluster)

std_kcl <- kmeans(std_metrics_data, 4, nstart = 25, iter.max = 100)
table(std_kcl$cluster)
t(round(std_kcl$centers, 3))
