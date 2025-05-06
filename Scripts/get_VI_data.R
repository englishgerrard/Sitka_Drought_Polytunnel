source('./Scripts/.FUNCTIONS.R')

yr1 <- read.csv('./Data/Vegetation Index_yr1.csv')

yr1 <- yr1 %>% mutate(status = ifelse(days == 0, 'a_t0', ifelse(days == 57,'c_endDrought' , ifelse(days > 58, 'yr1_recovery', 'b_Drought'))))

yr2 <- read.csv('./Data/Vegetation_Index_yr2.csv')
yr2[yr2 == 368] <- 367

full_index <- data.frame(id = c(yr1$id, yr2$id),
                         date = c(yr1$date, yr2$date),
                         treatment = c(yr1$treatment, yr2$treatment),
                         clone = c(yr1$clone, yr2$clone),
                         clone_num = c(yr1$clone_num, yr2$clone_num),
                         branch = c(yr1$branch, yr2$branch),
                         days = c(yr1$days, yr2$days),
                         status = c(yr1$status, yr2$status),
                         DateTime = c(yr1$DateTime, yr2$DateTime_avg),
                         # VI
                         CCI = c(yr1$CCI, yr2$CCI),
                         CIgr = c(yr1$CIgreen, yr2$CIgr),
                         CIre = c(yr1$CIred_edge , yr2$CIre),
                         NDVI = c(yr1$NDVI , yr2$NDVI),
                         NDVIre = c(yr1$NRred_edge , yr2$NDVIre),
                         SR = c(yr1$SR , yr2$SR),
                         
                         ARI = c(yr1$ARI , yr2$ARI),
                         ARI2 = c(yr1$ARI2 , yr2$ARI2),
                         CRI1 = c(yr1$CRI1 , yr2$CRI1),
                         CRI2 = c(yr1$CRI2 , yr2$CRI2),
                         PRI = c(yr1$PRI , yr2$PRI),
                         
                         MSI = c(yr1$MSI , yr2$MSI),
                         NDWI = c(yr1$NDWI1 , yr2$NDWI),
                         NDWI2= c(yr1$NDWI1640 , yr2$NDWI2),
                         SRWI = c(yr1$SRWI , yr2$SRWI),
                         WI = c(yr1$WI , yr2$WI),
                         WI_NDVI= c(yr1$WI_NDVI , yr2$WI_NDVI)
)


write.csv(full_index, './Data/full_VI.csv')
print('Setup Run :) have a nice day')
