library(tidyverse)

library(sf)

folder = ("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten")  ## set the working directory
finfolder = "C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/files_country_2019_test_june2019" #folder where the files per country will be saved
setwd(folder)

kba_class <- read_csv('kba_class.csv') #read in kba classifications from WBDB (status and criteria report, nothing ticked)
kba_class <- select(kba_class, c('SitRecID', 'SitMarine', 'SitTerrestrial', 'SitFreshwater', 'CtyISO3', 'CtyBirdLifeName'))

#add in mountain class
prev <- read_csv('sdg_class_MikeE.csv') #read in sdg classes from last year with Mike E's additional changes (note these are missing updated mountain information)
prev <- select(prev, c('SitRecID', 'mountain'))

library(foreign)
mtn <- read.dbf('mnt_new_class.dbf') #read new classified marine and terrestrial sites
mtn <- select(mtn, c('siteID', 'MEAN'))

all <- merge(prev, mtn, by.x = 'SitRecID', by.y = 'siteID', all = T)

#classify new mountain sites based on overlap
for (row in 1:nrow(all)){
  if (is.na(all$mountain[row]) | all$mountain[row] == '#N/A'){ #if there is no previous information on whether a site is a mountain kba
    if (!is.na(all$MEAN[row])){ #if there is information on mean coverage of the site by mountains
      if (all$MEAN[row] >= 0.05){ #5% or more was decided as the appropriate threshold of mountain coverage for a site to be classified as a mountain KBA
        all$mountain[row] <- 1 #if 5% or more coverage, classify the site as a mountain kba
      } else if (all$MEAN[row] < 0.05){
        all$mountain[row] <- 0 #if less than 5% coverage, don't classify the site as a mountain kba
      }
    }
  }
}

colnames(all)[colnames(all) == 'mountain'] <- 'SitMountain'

all_class <- merge(kba_class, all, by = 'SitRecID', all.x = T) #combine mountain classification with output of WBDB

#read in KBA layer to subset to necessary sites:

kba <- st_read(dsn = '../../../KBA/KBAsGlobal_2019_September_02', layer = 'KbaGlobal_2019_September_02_POL', stringsAsFactors=F) #read kba shapefile in that will be run
kba <- st_set_geometry(kba, NULL)



all_classified <- all_class[all_class$SitRecID %in% kba$SitRecID,] #subset to KBAs used in this years analyses
all_classified <- select(all_classified, c('SitRecID', 'SitMarine', 'SitTerrestrial', 'SitFreshwater', 'SitMountain', 'CtyISO3', 'CtyBirdLifeName')) #reorder columns
colnames(all_classified) <- c('SitRecID', 'marine', 'terrestrial', 'Freshwater', 'mountain', 'ISO3', 'Country')

all_classified$marine[all_classified$marine == 'Yes'] <- 1
all_classified$marine[all_classified$marine == 'No'] <- 0
all_classified$terrestrial[all_classified$terrestrial == 'Yes'] <- 1
all_classified$terrestrial[all_classified$terrestrial == 'No'] <- 0
all_classified$Freshwater[all_classified$Freshwater == 'Yes'] <- 1
all_classified$Freshwater[all_classified$Freshwater == 'No'] <- 0

write.csv(all_classified, 'classif_kbas_FINAL2019_revised.csv', row.names = F) #write output file for analyses



mnt <- all_classified[all_classified$mountain == '#N/A',] #mountain sites to rerun
write.csv(mnt, 'mnt_sites_rerun.csv', row.names = F) #some still missing after analyses so need to rerun
#couldnt run ovelay with these so reviewed by eye as was only eleven and from this produced classif_kbas_FINAL2019.csv



##################################


## below is filtering work to identify sites to classify as mountain/not


#create list of sites that need to be classified
kba <- st_read(dsn = '../../../KBA/KBAsGlobal_2019_September_02', layer = 'KbaGlobal_2019_September_02_POL', stringsAsFactors=F) #read kba shapefile in that will be run
kba <- st_set_geometry(kba, NULL) #set geometry to null so can just use tabular info

prev <- read_csv('sdg_class_MikeE.csv') #read in sdg classes from last year with Mike E's additional changes (note these are missing updated mountain information)

kba_tr <- merge(kba, prev, by='SitRecID', all.x = T) #subset to sites to run

#missing <- kba_tr[is.na(kba_tr$terrestrial) | kba_tr$mountain == '#N/A',] #subset to missing sites that need to be run
missing <- kba_tr[kba_tr$mountain == '#N/A',] #subset to missing sites that need to be run
write.csv(missing, 'sites_missing_classification.csv', row.names = F)

missing2 <- merge(kba, missing, by = 'SitRecID')

st_write(missing2, dsn = paste(folder,'/sites_missing_classification', sep=''), driver = 'ESRI Shapefile', update = T)

###########################

library(foreign)

#terMar <- read.dbf('kba_n_marine_terr.dbf') #read new classified marine and terrestrial sites
mtn <- read.dbf('mnt_new_class.dbf') #read new classified marine and terrestrial sites
mtn <- select(mtn, c('siteID', 'MEAN'))

#class <- merge(terMar, mtn, by = 'VALUE', all = T)
#class <- select(class, c('VALUE', 'MEAN.x', 'MEAN.y')) #subset to useful columns; mean is mean score across all cells in the site classified as mountain (from 1 being totally montane to 0 being no coverage) and marine (1 being marine to 0 being terrestrial)
#colnames(class) <- c('SitRecID', 'Mean_cover_marine', 'Mean_cover_mountain')

all <- merge(kba_tr, mtn, by.x = 'SitRecID', by.y = 'siteID', all = T) #combine two datasets

# #classify new terrestrial sites based on overlap
# for (row in 1:nrow(all)){
#   if (is.na(all$terrestrial[row])){ #if there is no previous information on whether a site is a mountain kba
#     if (!is.na(all$Mean_cover_marine[row])){ #if there is information on mean coverage of the site by mountains
#       if (all$Mean_cover_marine[row] <= 0.95){ #5% or more was decided as the appropriate threshold of mountain coverage for a site to be classified as a mountain KBA
#         all$terrestrial[row] <- 1 #if 5% or more coverage, classify the site as a mountain kba
#         #all$terrestrial <- 1 #if the site is a mountain kba, it should also be classified as terrestrial so this is just to check that
#       } else if (all$Mean_cover_marine[row] > 0.95){
#         all$terrestrial[row] <- 0 #if less than 5% coverage, don't classify the site as a mountain kba
#       }
#     }
#   }
# }
# 
# #classify new marine sites based on overlap
# for (row in 1:nrow(all)){
#   if (is.na(all$marine[row])){ #if there is no previous information on whether a site is a mountain kba
#     if (!is.na(all$Mean_cover_marine[row])){ #if there is information on mean coverage of the site by mountains
#       if (all$Mean_cover_marine[row] >= 0.05){ #5% or more was decided as the appropriate threshold of mountain coverage for a site to be classified as a mountain KBA
#         all$marine[row] <- 1 #if 5% or more coverage, classify the site as a mountain kba
#         #all$terrestrial <- 1 #if the site is a mountain kba, it should also be classified as terrestrial so this is just to check that
#       } else if (all$Mean_cover_marine[row] < 0.05){
#         all$marine[row] <- 0 #if less than 5% coverage, don't classify the site as a mountain kba
#       }
#     }
#   }
# }

#classify new mountain sites based on overlap
for (row in 1:nrow(all)){
  if (is.na(all$mountain[row]) | all$mountain[row] == '#N/A'){ #if there is no previous information on whether a site is a mountain kba
    if (!is.na(all$MEAN[row])){ #if there is information on mean coverage of the site by mountains
      if (all$MEAN[row] >= 0.05){ #5% or more was decided as the appropriate threshold of mountain coverage for a site to be classified as a mountain KBA
        all$mountain[row] <- 1 #if 5% or more coverage, classify the site as a mountain kba
      } else if (all$MEAN[row] < 0.05){
        all$mountain[row] <- 0 #if less than 5% coverage, don't classify the site as a mountain kba
      }
    }
  }
}

all$terrestrial[all$mountain == 1] <- 1 #if the site is a mountain kba, it should also be classified as terrestrial so this is just to check that

write.csv(all, 'classif_KBAs_final_end2019.csv', row.names = F) #output KBA classification file


