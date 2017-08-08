setwd("../Daily Activity Data")
library(caret)

### Prepare and clean datasets

# Enforce correct data types for certain columns
setClass('verboseDate')
setAs('character', 'verboseDate', function(from) as.Date(from, format = '%B %d, %Y'))
column_types <- c('ï..Date'='verboseDate', 'Retailer.Account.Number..v3...evar3.'='character',
                  'Country.Name'='character')

# Load the data from files
fa17_activity_data <- read.csv('Daily_Activity_FA17.csv', header = TRUE, colClasses = column_types)
ho17_activity_data <- rbind(read.csv('Daily_Activity_HO17_1.csv', header = TRUE, colClasses = column_types),
                            read.csv('Daily_Activity_HO17_2.csv', header = TRUE, colClasses = column_types))
sp18_activity_data <- rbind(read.csv('Daily_Activity_SP18_1.csv', header = TRUE, colClasses = column_types),
                            read.csv('Daily_Activity_SP18_2.csv', header = TRUE, colClasses = column_types))

# Merge data for all seasons
activity_data <- rbind(fa17_activity_data, ho17_activity_data, sp18_activity_data)
rm(list = c('fa17_activity_data', 'ho17_activity_data', 'sp18_activity_data'))

# Keep valid account numbers only
activity_data <- activity_data[which(nchar(as.character(activity_data$Retailer.Account.Number..v3...evar3.)) == 10), ]

# Keep seasons relevant to futures activity
valid_seasons <- c('fa2017', 'ho2017', 'sp2018', 'fall 2017')
activity_data <- activity_data[which(activity_data$Season..v8...evar8. %in% valid_seasons), ]

# Remove missing rows
activity_data <- activity_data[which(activity_data$New.Repeat.Visitor..v22...evar22. != ''),]
activity_data <- activity_data[which(activity_data$Days.Since.Last.Visit..v34...evar34. != ''),]
activity_data <- activity_data[which(activity_data$Channel != ''),]
activity_data <- activity_data[which(activity_data$Role..v7...evar7. !=  ''),]

# Make the values consistent
activity_data$Role..v7...evar7.[which(activity_data$Role..v7...evar7. == 'sales_rep')] <- factor('sales rep')
activity_data$Season..v8...evar8.[which(activity_data$Season..v8...evar8. == 'fall 2017')] <- factor('fa2017')

# Compute Day of Week on our own as inconsistencies exist in data
# activity_data$Day.of.Week..v32...evar32.[which(activity_data$Day.of.Week..v32...evar32. == '')] <-
#   weekdays(activity_data$ï..Date[which(activity_data$Day.of.Week..v32...evar32. == '')])
# activity_data$Day.of.Week..v32...evar32. <- factor(activity_data$Day.of.Week..v32...evar32.)
activity_data$Day.of.Week <- factor(weekdays(activity_data$ï..Date))

# Create binary variable for revenue
activity_data$Rev.binary <- 0
activity_data$Rev.binary[which(activity_data$Revenue > 0)] <- 1
activity_data$Rev.binary <- factor(activity_data$Rev.binary)

# Drop certain columns that are not required
columns_to_drop <- c('Weekend.Weekday..v33...evar33.', 'Add.All.Products.to.Cart..e50...event50.',
                     'Day.of.Week..v32...evar32.', 'Retailer.Access.Status..v6...evar6.',
                     'Retailer.Account.Name..v2...evar2.')
activity_data <- activity_data[, !colnames(activity_data) %in% columns_to_drop]

# Feature Selection
features_dropped <- c('ï..Date', 'Revenue', 'Orders', 'Units')
activity_data <- activity_data[, !colnames(activity_data) %in% features_dropped]

# Split data into training and test datasets
sp2018_rows <- which(activity_data$Season..v8...evar8. == 'sp2018')
activity_test <- activity_data[sp2018_rows,]
activity_train <- activity_data[-sp2018_rows,]

summary(activity_data)
gc()

### Predictive Modeling

## Logistic Regression
# logistic_fit <- train(Rev.binary ~ ., data = activity_train, method = 'glm', family = 'binomial',
#                  trControl = trainControl(method = 'cv', number = 10))

temp_train <- activity_train[1:10000, colnames(activity_train) %in% c("Rev.binary", 'Retailer.Account.Number..v3...evar3.',
                                'Channel', 'Days.Since.Last.Visit..v34...evar34.', 'New.Repeat.Visitor..v22...evar22.',
                                'Role..v7...evar7.', 'Added.All.Products.to.Assortment..e8...event8.',
                                'Added.Product.to.Assortment..e7...event7.', 'Added.Assortment.to.Cart..e10...event10.',
                                'Zero.Filter.Results.Returned..e53...event53.', 'Zero.Search.Results.Returned..e3...event3.')]
logistic_fit2 <- glm(Rev.binary ~ ., data = temp_train, family = 'binomial')

## Random Forest
library(randomForest)

rf_grid <- expand.grid(mtry = 3:7)
rf_fit <- train(Rev.binary ~ ., data = temp_train, method = 'rf',
                trControl = trainControl(method = 'oob'), tuneGrid = rf_grid)

rf_fit2 <- randomForest(Rev.binary ~ ., data = temp_train, importance = TRUE, nodesize = 15,
                        ntree = 300)

