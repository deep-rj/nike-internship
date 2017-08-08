library(h2o)
h2o.init(max_mem_size = '8G')

column_names <- c("Date","Retailer Account Number (v3) (evar3)","Channel","Country Name","Day of Week (v32) (evar32)",
                  "Days Since Last Visit (v34) (evar34)","New/Repeat Visitor (v22) (evar22)",
                  "Retailer Access Status (v6) (evar6)","Retailer Account Name (v2) (evar2)",
                  "Retailer Language (v4) (evar4)","Role (v7) (evar7)","Season (v8) (evar8)",
                  "Weekend/Weekday (v33) (evar33)","Site Section/Channel (v16) (evar16)",
                  "Add All Products to Cart (e50) (event50)","Added All Products to Assortment (e8) (event8)",
                  "Added Product to Assortment (e7) (event7)","Added Assortment to Cart (e10) (event10)",
                  "Added Similar Color Product (e5) (event5)","Added Similar Style Product (e6) (event6)",
                  "Apply (e35) (event35)","Carousel Image Clicked (e49) (event49)","Clear Filters (e39) (event39)",
                  "Copy Order (e13) (event13)","Delete Order (e12) (event12)","Filter Performed (e51) (event51)",
                  "Filter Results Returned (e52) (event52)","Learn More CTA Tout Clicked (e38) (event38)",
                  "Order Saved, Unsubmitted (e36) (event36)","Order Upload Attempted (e32) (event32)",
                  "Order Upload Failed (e34) (event34)","Order Upload Successful (e33) (event33)",
                  "Removed Assortment from Cart (e11) (event11)","Removed Product from Assortment (e9) (event9)",
                  "Save Order (e40) (event40)","Search Performed (e1) (event1)","Search Results Returned (e2) (event2)",
                  "Tech Sheet Viewed (e15) (event15)","Video: 25% Viewed (e27) (event27)",
                  "Video: 50% Viewed (e28) (event28)","Video: 75% Viewed (e29) (event29)",
                  "Video: 95% Viewed (e30) (event30)","Video Completes (e31) (event31)",
                  "Video Time Played (e25) (event25)","Video Views (e26) (event26)","Workspace Created (e16) (event16)",
                  "Workspace Enter (e17) (event17)","Zero Filter Results Returned (e53) (event53)",
                  "Zero Search Results Returned (e3) (event3)","Orders","Product Views","Revenue","Units","Visits",
                  "Unique Visitors","Cart Views","Carts","Campaign Views")

col_types <- c(rep("string", 14), rep("numeric", 44))

# Import data
setwd("~/Digital Insights Project/Daily Activity Data/With Section")
# Manually remove header row for each file
fa17_activity_data1 <- h2o.uploadFile('FA2017_Daily_Activity_1.csv',col.names = column_names, col.types = col_types,
                                     header = TRUE, na.strings = '')
fa17_activity_data1 <- fa17_activity_data1[-1,]

fa17_activity_data2 <- h2o.uploadFile('FA2017_Daily_Activity_2.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
fa17_activity_data2 <- fa17_activity_data2[-1,]

ho17_activity_data1 <- h2o.uploadFile('HO2017_Daily_Activity_1.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
ho17_activity_data1 <- ho17_activity_data1[-1,]

ho17_activity_data2 <- h2o.uploadFile('HO2017_Daily_Activity_2.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
ho17_activity_data2 <- ho17_activity_data2[-1,]

ho17_activity_data3 <- h2o.uploadFile('HO2017_Daily_Activity_3.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
ho17_activity_data3 <- ho17_activity_data3[-1,]

sp18_activity_data1 <- h2o.uploadFile('SP2018_Daily_Activity_1.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
sp18_activity_data1 <- sp18_activity_data1[-1,]

sp18_activity_data2 <- h2o.uploadFile('SP2018_Daily_Activity_2.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
sp18_activity_data2 <- sp18_activity_data2[-1,]

sp18_activity_data3 <- h2o.uploadFile('SP2018_Daily_Activity_3.csv',col.names = column_names, col.types = col_types,
                                      header = TRUE, na.strings = '')
sp18_activity_data3 <- sp18_activity_data3[-1,]

# Merge data for all seasons
activity_data <- h2o.rbind(fa17_activity_data1, fa17_activity_data2, ho17_activity_data1, ho17_activity_data2,
                           ho17_activity_data3, sp18_activity_data1, sp18_activity_data2, sp18_activity_data3)

# Keep valid account numbers only
activity_data <- activity_data[h2o.nchar(activity_data[,"Retailer Account Number (v3) (evar3)"]) == 10, ]

# Remove demo accounts
demo_accounts <- c('0000447669', '0000447670', '0000447716', '0000447717', '0005022178', '0005072216', '0005072226', '0000449262', '0000422289', '0000415245', '0000448951', '0000448440', '0000448947', '0000448946', '0000447718', '0000447720', '0000447946', '0000447719', '0000451474', '0000451577', '0000451672', '0000451673', '0000451674', '0000451675', '0000451696', '0000451712', '0000451713', '0000451714', '0000451751', '0000453244', '0000451749', '0000457078', '0000457079', '0000456990', '0000457391', '0000457392', '0000457393', '0000457394', '0000457395', '0000457396', '0000458546', '0000458547', '0000457401', '0000457402', '0000457081', '0000457082', '0000457083', '0000457084', '0000459106', '0000459107', '0000457085', '0000457090', '0000457086', '0000457087', '0000457088', '0000459108', '0000459109', '0000457089', '0000456976', '0000456977', '0000459322', '0000459473', '0000459474', '0000459475', '0000459476', '0000459477', '0000459802', '0000460018', '0000460019', '0000460020', '0000460037', '0000460038', '0000460039', '0000460040', '0000460054', '0000460055', '0000460066', '0000460075', '0000460111', '0000460430', '0000460711', '0000460712', '0000460713', '0000460714', '0000460726', '0000460727', '0000460728', '0000460758', '0000460880', '0000460913', '0000460950', '0000460986', '0000461006', '0000461007', '0000461126', '0000461127', '0000461129')
activity_data <- activity_data[!activity_data[,"Retailer Account Number (v3) (evar3)"] %in% demo_accounts, ]

# Keep desired seasons
activity_data[activity_data[,"Season (v8) (evar8)"] == 'fall 2017', "Season (v8) (evar8)"] <- 'fa2017'
valid_seasons <- c('fa2017', 'ho2017', 'sp2018')
activity_data <- activity_data[activity_data[,"Season (v8) (evar8)"] %in% valid_seasons, ]

# Remove missing rows
activity_data <- activity_data[activity_data[,"New/Repeat Visitor (v22) (evar22)"] != '',]
activity_data <- activity_data[activity_data[,"Days Since Last Visit (v34) (evar34)"] != '',]
activity_data <- activity_data[activity_data[,"Channel"] != '',]
activity_data <- activity_data[activity_data[,"Role (v7) (evar7)"] !=  '',]

# Make values consistent
activity_data[activity_data[,"Role (v7) (evar7)"] == 'sales rep', "Role (v7) (evar7)"] <- 'sales_rep'

# Keep rows relevant to futures activity
activity_data <- activity_data[h2o.grep('futures|learn', activity_data[, 'Site Section/Channel (v16) (evar16)'],
                                        output.logical = TRUE), ]

# Compute Day of Week on our own as inconsistencies exist in data
activity_data[,"Date_time_col"] <- as.Date(activity_data[,"Date"], '%B %d, %Y')
activity_data[,"Day of Week (v32) (evar32)"] <- h2o.dayOfWeek(activity_data[,"Date_time_col"])
activity_data[,"Week"] <- h2o.week(activity_data[,"Date_time_col"])
activity_data[,"Day"] <- h2o.day(activity_data[,"Date_time_col"])

# Calculate day counter
# h2o.hist(round((activity_data[activity_data["Season (v8) (evar8)"] == 'fa2017',"Date_time_col"] - 
#                   +                     +            min(activity_data[activity_data["Season (v8) (evar8)"] == 'fa2017',"Date_time_col"]))/(1000*3600*24)))

# activity_data[activity_data[,"Date_time_col"] == min(activity_data[activity_data["Season (v8) (evar8)"] == 'fa2017',"Date_time_col"]), "Date"]

# activity_data[,"Day_counter"] <- h2o.rbind(
#   round((activity_data[activity_data[,"Season (v8) (evar8)"] == 'fa2017', "Date_time_col"] - 
#            min(activity_data[activity_data[,"Season (v8) (evar8)"] == 'fa2017', "Date_time_col"]))/(1000*3600*24)),
#   round((activity_data[activity_data[,"Season (v8) (evar8)"] == 'ho2017', "Date_time_col"] - 
#            min(activity_data[activity_data[,"Season (v8) (evar8)"] == 'ho2017', "Date_time_col"]))/(1000*3600*24)),
#   round((activity_data[activity_data[,"Season (v8) (evar8)"] == 'sp2018', "Date_time_col"] - 
#            min(activity_data[activity_data[,"Season (v8) (evar8)"] == 'sp2018', "Date_time_col"]))/(1000*3600*24))
# )

# Create binary response variable for revenue
activity_data[,"Rev.binary"] <- 0
activity_data[activity_data[,'Revenue'] > 0, "Rev.binary"] <- 1

# Convert some columns to factors
categorical_cols <- c("Channel", "Day of Week (v32) (evar32)", "Days Since Last Visit (v34) (evar34)",
                      "New/Repeat Visitor (v22) (evar22)", "Role (v7) (evar7)", "Season (v8) (evar8)",
                      "Rev.binary", "Site Section/Channel (v16) (evar16)", "Retailer Account Number (v3) (evar3)",
                      "Retailer Language (v4) (evar4)", "Country Name")
activity_data[, categorical_cols] <- as.factor(activity_data[, categorical_cols])

# Drop certain columns that are not required
columns_to_drop <- c('Weekend/Weekday (v33) (evar33)', 'Add All Products to Cart (e50) (event50)',
                     "Retailer Access Status (v6) (evar6)", "Week", "Day", "Date",
                     "Zero Filter Results Returned (e53) (event53)", "Order Upload Attempted (e32) (event32)",
                     "Filter Performed (e51) (event51)", "Search Performed (e1) (event1)",
                     "Video: 50% Viewed (e28) (event28)", "Video: 75% Viewed (e29) (event29)",
                     "Video: 95% Viewed (e30) (event30)", "Video Time Played (e25) (event25)", "Unique Visitors")
activity_data <- activity_data[, !colnames(activity_data) %in% columns_to_drop]



