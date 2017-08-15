setwd('C:\\Users\\RJha3\\Documents\\Digital First Comms Results\\SP18')

# Read the file
comms_data <- fread('SP18_comms_analysis.csv', header = TRUE)
head(comms_data)

# Filter 2018 campaigns
comms_sp2018 <- comms_data[grep('sp2018', comms_data$`Tracking Code`, ignore.case = TRUE), ]
head(comms_sp2018)
summary(comms_sp2018)

# Clean up the tracking code string
tracking_list <- strsplit(comms_sp2018$`Tracking Code`, '#', fixed = TRUE)
comms_sp2018$`Tracking Code` <- unlist(lapply(tracking_list, function(x) x[1]))

tracking_list <- strsplit(comms_sp2018$`Tracking Code`, '=', fixed = TRUE)
comms_sp2018$`Tracking Code` <- unlist(lapply(tracking_list, function(x) {
                                                                if (length(x) == 2) return(x[2])
                                                                return(x[1])
                                                              } ))

# Split tracking code components to populate other columns
tracking_components <- strsplit(comms_sp2018$`Tracking Code`, '_', fixed = TRUE)
comms_sp2018$Type <- unlist(lapply(tracking_components, function(x) x[3]))
comms_sp2018$Category <- unlist(lapply(tracking_components, function(x) x[4]))
comms_sp2018$Territory <- unlist(lapply(tracking_components, function(x) x[5]))
comms_sp2018$Campaign_Season <- unlist(lapply(tracking_components, function(x) x[6]))
comms_sp2018$Channel <- unlist(lapply(tracking_components, function(x) x[7]))
comms_sp2018$Tier <- unlist(lapply(tracking_components, function(x) x[8]))
comms_sp2018$unique_id <- unlist(lapply(tracking_components, function(x) x[9]))

# Removing few anomalous rows
# 1:         comms_email_futures_xcat_CEE_EN_sp2018_xch_all_introSO 
# 2:  comms_email_futures_xcat_CENEU_GB_CEE_sp2018_xch_all_seasprev 
# 6: comms_email_futures_xcat_CENEU_GB_DIST_sp2018_xch_all_seasprev 
# 11:        comms_email_futures_xcat_DIST_EN_sp2018_xch_all_introSO 
comms_sp2018 <- comms_sp2018[-which(lengths(tracking_components) != 9), ]

# Remove zero revenue rows
comms_sp2018 <- comms_sp2018[which(comms_sp2018$Revenue > 0),]

# Group all futures order types together
comms_sp2018$`Order Type (v43) (evar43)`[grep('futures', comms_sp2018$`Order Type (v43) (evar43)`)] <- 'futures'

# Create Geo column
comms_sp2018$Geo[comms_sp2018$Territory %in% c('GR', 'PL', 'RU', 'IL', 'CENEU', 'TR')] <- 'CEE'
comms_sp2018$Geo[comms_sp2018$Territory %in% c('BR', 'IN', 'KR', 'MX', 'PAC', 'SEA', 'SOCO', 'ZA')] <- 'EM'
comms_sp2018$Geo[comms_sp2018$Territory %in% c('CN', 'TW')] <- 'GC'
comms_sp2018$Geo[comms_sp2018$Territory %in% c('JP')] <- 'JP'
comms_sp2018$Geo[comms_sp2018$Territory %in% c('CA', 'US')] <- 'NA'
comms_sp2018$Geo[comms_sp2018$Territory %in% c('AGS', 'NOEU', 'UKI', 'FR', 'SOEU')] <- 'WE'

head(comms_sp2018)

fwrite(comms_sp2018, file = 'SP18_results.csv')

