#### Converts output of KBA-PA output into SDG reporting output

## Part 1 - read in input files, load libraries, set working directory and fix encoding issues

library(tidyverse)
library(xlsx)

year_run <- format(Sys.Date(), "%Y") #get the current year

folder <- paste("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables", year_run) #set working directory
#folder <- "C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/input tables 2019_someOverrodeDec19" 
setwd(folder)

reg_grp <- read_csv(paste('../regional_groupings_', year_run, '.csv', sep = "")) #load in regional groupings needed for reporting
# fix erroneous country names
#Encoding(tab3$GeoAreaName) <- "UTF-8" #fix character encoding
reg_grp$`Country Name`[reg_grp$`ISO Code` == "CIV"] <- "Cote d'Ivoire"
reg_grp$`Country Name`[reg_grp$`ISO Code` == "CUW"] <- "Curacao (to Netherlands)"
reg_grp$`Country Name`[reg_grp$`ISO Code` == "STP"] <- "Sao Tome e Principe"
reg_grp$`Country Name`[reg_grp$`ISO Code` == "REU"] <- "Reunion (to France)"
reg_grp$`Country Name`[reg_grp$`ISO Code` == "ALA"] <- "Aland Islands"
reg_grp$`Region Name`[reg_grp$`M49 Code(region)` == 747] <- "Western Asia (M49) and Northern Africa (M49)"  

isos <- read.csv(paste('../Iso_countries_', year_run, '.csv', sep = "")) #load iso codes
isos <- select(isos, c('countryname', 'ISO_SDG')) #subset to country name and ISO_SDG code

fls <- dir(pattern = "PA coverage")#delete unwanted files
fls
length(fls)

#ecosys <- c(mar, terr, fw, mnt) #create list of ecosystem outputs to run

## Part 2 - create output files for each system for SDG reporting

for (x in 2:length(fls)){ #don't go from 1 as 1 is all sites, and we only need disaggregations by ecosystem type
  #mar <- select(mar, c('Global/region/country', 'year', 'Mean % area of each KBA covered by PAs (lower CI)', 'Mean % area of each KBA covered by PAs (value)', 'Mean % area of each KBA covered by PAs (upper CI)'))
  fl1 <- fls[x]
  fl1
  tab <- read_csv(fl1)

  tab <- tab[tab$year >= 2000,] #subset to years greater than 2000

  #tab <- gather(tab, Bounds, Value, `Mean % area of each KBA covered by PAs (lower CI)`:`Mean % area of each KBA covered by PAs (upper CI)`, factor_key = T) #restructure data so fit reporting style (wide to long format)
  
  #tab <- select(tab, c('Global/region/country', 'year', 'Value', 'Bounds')) #subset to required columns and reorder columns
  
  tab <- select(tab, c('Global/region/country', 'year', 'Mean % area of each KBA covered by PAs (value)', 'Mean % area of each KBA covered by PAs (upper CI)', 'Mean % area of each KBA covered by PAs (lower CI)')) #select necessary columns
  
  tab <- unique(tab)
  
  #colnames(tab) <- c('GeoAreaName', 'TimePeriod', 'Value', 'Bounds') #change column names to match SDG reporting
  
  colnames(tab) <- c('GeoAreaName', 'TimePeriod', 'Value', 'UpperBound', 'LowerBound') #change column names to match SDG reporting
  
  tab$GeoAreaName[tab$GeoAreaName=='Global'] <- 'World' #replace name global with world to fit report style
  
  #tab$Bounds <- as.character(tab$Bounds)
  #tab$Bounds[tab$Bounds == 'Mean % area of each KBA covered by PAs (lower CI)'] <- 'LB'
  #tab$Bounds[tab$Bounds == 'Mean % area of each KBA covered by PAs (value)'] <- 'MP'  
  #tab$Bounds[tab$Bounds == 'Mean % area of each KBA covered by PAs (upper CI)'] <- 'UB'
  
  #add in necessary columns
  tab$TimeDetail <- tab$TimePeriod
  tab$Nature <- 'C'
  tab$Units <- 'PERCENT'
  tab$`Reporting Type` <- 'G'
  tab$Source <- paste('BirdLife International, IUCN and UNEP-WCMC (', max(tab$TimePeriod), '). Based on spatial overlap between polygons for Key Biodiversity Areas from the World Database of Key Biodiveristy Areas (www.keybiodiversityareas.org) and polygons for protected areas from the World Database on Protected Areas (www.protectedplanet.net)', sep = "")
  tab$FootNote <- ''
  
  
  # tab$GeoAreaName <- as.character(tab$GeoAreaName) #set region column to be a character variable
  # #Encoding(tab$GeoAreaName) <- "UTF-8" #fix character encoding
  # #tab$GeoAreaName[tab$GeoAreaName == "C<f4>te d'Ivoire"] <- "Cote d'Ivoire"
  # #tab$GeoAreaName[tab$GeoAreaName == "Cura<e7>ao (to Netherlands)"] <- "Curacao (to Netherlands)"
  # #tab$GeoAreaName[tab$GeoAreaName == "S<e3>o Tom<e9> e Pr<ed>ncipe"] <- "Sao Tome e Principe"
  # #tab$GeoAreaName[tab$GeoAreaName == "R<e9>union (to France)"] <- "Reunion (to France)"
  # tab$GeoAreaName[tab$GeoAreaName == "Western Asia<U+00A0>(M49) and Northern Africa (M49)"] <- "Western Asia (M49) and Northern Africa (M49)"  
  
  reg_grp <- select(reg_grp, c('M49 Code', 'ISO Code', 'Country Name', 'M49 Code(region)', 'Region Name', 'Reference Area Type [i.e. global, SDG groupings, MDG groupings, etc.]')) #select columns
  
  reg_grpC <- select(reg_grp, c('M49 Code', 'ISO Code', 'Country Name'))
  
  colnames(reg_grpC) <- c('GeoAreaCode', 'ISO Code', 'Country Name')
  
  reg_grpC$RefAreaType_InternalUseOnly <- '3.0-Country' #add country ref
  
  reg_grpR <- select(reg_grp, c('M49 Code(region)', 'Region Name', 'Reference Area Type [i.e. global, SDG groupings, MDG groupings, etc.]'))
  
  colnames(reg_grpR) <- c('GeoAreaCode2', 'Region Name', 'RefAreaType_InternalUseOnly2')
  reg_grpR <- unique(reg_grpR)
  
  tab2 <- merge(tab, reg_grpC, by.x='GeoAreaName', by.y='ISO Code', all.x=T)
  Encoding(tab2$GeoAreaName) <- "UTF-8"
  tab2$GeoAreaName[tab2$GeoAreaName == "Western Asia<U+00A0>(M49) and Northern Africa (M49)"] <- "Western Asia (M49) and Northern Africa (M49)"
  tab2 <- unique(tab2)
  #Encoding(tab2$`Country Name`) <- 'UTF-8'
  
  for (row in 1:nrow(tab2)){
    if(!is.na(tab2$`Country Name`[row])){
      tab2$GeoAreaName[row] <- tab2$`Country Name`[row]
    }
  }
  
  tab3 <- merge(tab2, reg_grpR, by.x='GeoAreaName', by.y='Region Name', all.x=T)
  
  for (row in 1:nrow(tab3)){
    if (is.na(tab3$RefAreaType_InternalUseOnly[row])){
      tab3$GeoAreaCode[row] <- tab3$GeoAreaCode2[row]
      tab3$RefAreaType_InternalUseOnly[row] <- tab3$RefAreaType_InternalUseOnly2[row]
    }
  }
  
  #categories that didn't code up for some reason
  tab3$RefAreaType_InternalUseOnly[tab3$GeoAreaName == 'Western Asia (M49) and Northern Africa (M49)'] <- '2.1-Regional (SDG)'
  tab3$GeoAreaCode[tab3$GeoAreaName == 'Western Asia (M49) and Northern Africa (M49)'] <- '747'
  tab3$RefAreaType_InternalUseOnly[tab3$GeoAreaName == 'LDC'] <- '2.3-Other groupings'
  tab3$GeoAreaCode[tab3$GeoAreaName == 'LDC'] <- '199'
  tab3$RefAreaType_InternalUseOnly[tab3$GeoAreaName == 'LLDC'] <- '2.3-Other groupings'
  tab3$GeoAreaCode[tab3$GeoAreaName == 'LLDC'] <- '432'
  tab3$RefAreaType_InternalUseOnly[tab3$GeoAreaName == 'SIDS'] <- '2.3-Other groupings'
  tab3$GeoAreaCode[tab3$GeoAreaName == 'SIDS'] <- '722'
  tab3$RefAreaType_InternalUseOnly[tab3$GeoAreaName == 'Developing'] <- '2.5-Regional (MDG)'
  tab3$GeoAreaCode[tab3$GeoAreaName == 'Developing'] <- '515'
  
  tab3 <- tab3[!is.na(tab3$RefAreaType_InternalUseOnly), ] #filter out any sites that didn't get ref codes and so shouldn't be in the final output


  
  #tab2 <- merge(tab, isos, by.x = 'GeoAreaName', by.y = 'countryname', all.x = T) #combine with ISO list to help matching to reporting categories
  #tab2 <- tab2[nchar(tab2$GeoAreaName) > 3,] #exclude iso codes
  
  # unique(tab3$GeoAreaName[is.na(tab3$ISO_SDG)]) #identify countries that didn't match iso codes - should only be regions (all countries should have an assigned iso code)
  # 
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Anguilla (to UK)'] <- 'AIA'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Cayman Islands (to UK)'] <- 'CYM'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'China (mainland)'] <- 'CHN'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Congo'] <- 'COG'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Congo, The Democratic Republic of the'] <- 'COD'
  # tab2$ISO_SDG[tab2$GeoAreaName == "Cote d'Ivoire"] <- 'CIV'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Hong Kong (China)'] <- 'HKG'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Iran, Islamic Republic of'] <- 'IRN'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Macedonia, the former Yugoslav Republic of'] <- 'MKD'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Martinique (to France)'] <- 'MTQ'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Mayotte (to France)'] <- 'MYT'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Palestinian Authority Territories'] <-  'PSE'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Puerto Rico (to USA)'] <- 'PRI'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Russia (Central Asian)'] <- 'RUS'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Sao Tome e Principe'] <- 'STP'
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Svalbard and Jan Mayen Islands (to Norway)'] <- 'SJM' #NOR
  # tab2$ISO_SDG[tab2$GeoAreaName == 'Taiwan, China'] <- 'CHN' #TWN
    
    
  head(tab3)
  
  tab <- tab3
  
  if (grepl('Freshwater', fl1)){
    tab$Indicator <- '15.1.2'
    tab$SeriesID <- '2837'
    tab$SeriesCode <- 'ER_PTD_FRWRT'
    tab$SeriesDescription <- 'Mean proportion of freshwater Key Biodiversity Areas (KBAs) covered by protected areas (%)'
  }
  
  if (grepl('marine', fl1)){
    tab$Indicator <- '14.5.1'
    tab$SeriesID <- '2999'
    tab$SeriesCode <- 'ER_MRN_MPA'
    tab$SeriesDescription <- 'Mean proportion of marine Key Biodiversity Areas (KBAs) covered by protected areas (%)'
  }
  
  if (grepl('terrestrial', fl1)){
    tab$Indicator <- '15.1.2'
    tab$SeriesID <- '2839'
    tab$SeriesCode <- 'ER_PTD_TERRS'
    tab$SeriesDescription <- 'Mean proportion of terrestrial Key Biodiversity Areas (KBAs) covered by protected areas (%)'
  }
  
  if (grepl('mountain', fl1)){
    tab$Indicator <- '15.4.1'
    tab$SeriesID <- '2838'
    tab$SeriesCode <- 'ER_PTD_MOTN'
    tab$SeriesDescription <- 'Mean proportion of mountain Key Biodiversity Areas (KBAs) covered by protected areas (%)'
  }
  
  #tab <- select(tab, c('GeoAreaCode', 'GeoAreaName', 'RefAreaType_InternalUseOnly', 'Indicator', 'SeriesID', 'SeriesCode', 'SeriesDescription', 'TimePeriod', 'Value', 'TimeDetail', 'Nature', 'Units', 'Bounds', 'Reporting Type', 'Source', 'FootNote'))
  
  tab <- select(tab, c('Indicator', 'SeriesID', 'SeriesDescription', 'GeoAreaCode', 'GeoAreaName', 'TimePeriod', 'Value', 'TimeDetail', 'UpperBound', 'LowerBound', 'Source', 'FootNote', 'Nature', 'Units', 'Reporting Type', 'SeriesCode', 'RefAreaType_InternalUseOnly'))
  
  colnames(tab)[colnames(tab) == 'TimeDetail'] <- 'Time_Detail'
  
  head(tab)
  colnames(tab)
  
  tab <- tab[
    order(tab[, "RefAreaType_InternalUseOnly"], tab[, "GeoAreaName"], tab[, "TimePeriod"]),
  ]
# , "GeoAreaCode", "TimePeriod"  
  
  if (grepl('Freshwater', fl1)){
    #write.xlsx(tab, '15.1.2_BirdLife_UNEP-WCMC_IUCN_03.12.2019', sheetName = "15.1.2_FreshwaterKBAs%", row.names = F)
    #write.csv(tab, '../15.1.2_BirdLife_UNEP-WCMC_IUCN_03.12.2019_fw.csv', row.names = F)
    write.csv(tab, '../119-15.1.2-2837-ER_PTD_FRWRT-3116.csv', row.names = F)
  }
  
  if (grepl('marine', fl1)){
    #write.csv(tab, '../14.5.1_BirdLife_UNEP-WCMC_IUCN_03.12.2019_.csv', row.names = F)
    write.csv(tab, '../119-14.5.1-2999-ER_MRN_MPA-3781.csv', row.names = F)
  }
  
  if (grepl('terrestrial', fl1)){
    #write.csv(tab, '15.1.2_BirdLife_UNEP-WCMC_IUCN_03.12.2019', sheetName = "15.1.2_TerrestrialKBAs%", row.names = F)
    #write.csv(tab, '../15.1.2_BirdLife_UNEP-WCMC_IUCN_03.12.2019_terr.csv', row.names = F)
    write.csv(tab, '../119-15.1.2-2839-ER_PTD_TERRS-4864.csv', row.names = F)
  }
  
  if (grepl('mountain', fl1)){
    #write.csv(tab, '../15.4.1_BirdLife_UNEP-WCMC_IUCN_03.12.2019.csv', row.names = F)
    write.csv(tab, '../119-15.4.1-2838-ER_PTD_MOTN-3857.csv', row.names = F)
  }
}
