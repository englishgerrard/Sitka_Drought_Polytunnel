source('./Scripts/.PACKAGES.R')

dd <- read.csv('./Data/tidy_averaged_spectra.csv')

dd$wavelength <- as.numeric(gsub('X', '', dd$wavelength))


band_select <- data.frame(R510 = filter(dd,wavelength %in% c(510))$value,
                          R515 = filter(dd,wavelength %in% c(515))$value,
                          R570 = filter(dd,wavelength %in% c(570))$value,
                          R531 = filter(dd,wavelength %in% c(531))$value,
                          R550 = filter(dd,wavelength %in% c(550))$value,
                          R565 = filter(dd,wavelength %in% c(565))$value,
                          R645 = filter(dd,wavelength %in% c(645))$value,
                          R650 = filter(dd,wavelength %in% c(650))$value,
                          R670 = filter(dd,wavelength %in% c(670))$value,
                          R700 = filter(dd,wavelength %in% c(700))$value,
                          R710 = filter(dd,wavelength %in% c(710))$value,
                          R750 = filter(dd,wavelength %in% c(750))$value,
                          R800 = filter(dd,wavelength %in% c(800))$value,
                          R820 = filter(dd,wavelength %in% c(820))$value,
                          R858 = filter(dd,wavelength %in% c(858))$value,
                          R860 = filter(dd,wavelength %in% c(860))$value,
                          R900 = filter(dd,wavelength %in% c(900))$value,
                          R970 = filter(dd,wavelength %in% c(970))$value,
                          R1240 = filter(dd,wavelength %in% c(1240))$value,
                          R1450 = filter(dd,wavelength %in% c(1450))$value,
                          R1600 = filter(dd,wavelength %in% c(1600))$value,
                          R1640 = filter(dd,wavelength %in% c(1640))$value,
                          R2130 = filter(dd,wavelength %in% c(2130))$value)



index <- filter(dd, wavelength %in% c(350))

b <- bind_cols(index,  band_select)

# greeness VI

index$CIgr <- (b$R750/b$R550)-1 # Green chlorophyll index (CIgreen) Gitelson et al 2005
index$CIre <- (b$R750/b$R710)-1 # Red edge chlorophyll index (CIred edge) Gitelson et al 2005
index$NDVI <- (b$R800 - b$R670)/ (b$R800 + b$R670) # Rouse et al 1973
index$NRVIre <- (b$R750 - b$R710)/ (b$R750 + b$R710) # Red edge normalized ratio (NRred edge) Gitelson et al 1996
# index$SR <- (b$R800/ b$R670) # removed to simlify discussion (follwed sim patter to NDVI). used VI outlided in Zhang and Zhou 2019 

# stress pigment VI

index$CCI <- (b$R531 - b$R645)/(b$R531 + b$R645)
index$ARI1 <- (1/b$R550) - (1/b$R700) #Anthocyanin Reflectance Index 1 (ARI1) Gitelson et at 2001
index$ARI2 <- b$R800*((1/b$R550) - (1/b$R700)) # Anthocyanin Reflectance Index 2 (ARI2) Gitelson et at 2001
# index$CRI1 <-(1/b$R510) - (1/b$R550) #Carotenoid Reflectance Index 1 (CRI1) Gitelson et at 2002 # removed as development 
# was based on plants containling no anthycyanin and a tendancy for inter correlation with chlorophyll
# index$CRI2 <-(1/b$R510) - (1/b$R700) #Carotenoid Reflectance Index 2 (CRI2) Gitelson et at 2002 # removed as development 
# was based on plants containling no anthycyanin and a tendancy for inter correlation with chlorophyll
index$PRI <- (b$R531 - b$R570)/(b$R531 + b$R570) ## is this pigment??

# Water VI
index$MSI <- b$R1600/b$R820 # Moisture stress index (MSI) # Hunt et al 1989
index$NDWI1 <- (b$R970 - b$R1450)/ (b$R970 + b$R1450) # Normalized diference water index 1 (NDWI1) 
index$NDWI2 <- (b$R858 - b$R1640)/ (b$R858 + b$R1640) # Normalized diference water index centered at 1640 nm (NDWI) # Chen et al 2005
index$SRWI <- b$R860/b$R1240 # Simple ratio water index (SRWI) # Zarco-Tejada et al 2001
# index$WI <- b$R900/b$R970 # Water index (WI) # Pe?uelas et al 1997 removed to balace out number of VI (perfroms sim to other water VI)
# and not used in Zhang and Zhou 2019 

# combination VI
index$WI_NDVI <- index$WI/ index$NDVI # WI normalised by NDVI # Penuelas et al 1997




index$collection_time <- ymd_hms(index$collection_time)
index$Time <- format(as.POSIXct(index$collection_time), format = '%H:%M:%S')
index$id <- paste(index$treatment, index$clone, index$clone_num)

write.csv(index, './Data/Index.csv')
