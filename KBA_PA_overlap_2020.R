## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PA - KBA Overlap calculator v2.0
## Ash Simkins & Lizzie Pearmain, June 2019
## based on code by Maria Dias, 2016
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Script to estimate the overlap of PAs and KBAs (giving earliest year of designation)
# In the 'marine' is any site with >=5% of its area as sea, as determined by intersection

### IMPORTANT NOTES
# Given the complexity of the code, it is strongly recommended to copy-paste the code into an R editor (e.g. RStudio)
# The minimum requirement to run the script is a 16 GB RAM machine
# WDPA layer is extremely large, can take several minutes (up to half an hour) to load
# to facilitate the process, the code runs the script and saves a file country by country. This avoids reanalysing the countries already done in case of error
# it might occur an error preventing to calculate which kbas overlap with ibas for the entire country - error in gIntersects step. Examples: Jordan and USA. These situations are easily identifiable in the final csv file (filter by SitRecID=9999, kba=9999). Additional lines of codes should be run if this happens (see codes from "redoiso=F" - around line 260, to "## save files for countries" - around line 450 change redoiso=F to redoiso=T), and the results added to the overall table

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 01. install and load packages ----
kpacks <- c('sf', 'dplyr','tidyverse', 'lwgeom')  ### install automatically the relevant packages
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

### packages set 2 (FOR TESTING ONLY)
# kpacks2 <- c('maptools','rgdal')  ### install automatically the relevant packages
# new.packs <- kpacks2[!(kpacks2 %in% installed.packages()[,"Package"])]
# if(length(new.packs)) install.packages(new.packs)
# lapply(kpacks2, require, character.only=T)
# remove(kpacks2, new.packs)

#### 01.2 Define functions ----
lu <- function (x = x){
  length(unique(x))
  #nrow(unique(x))
}

#### 02. set file locations and working directories ----

## NB.in the KBA layer attribute table, the relevant fields should be "SitRecID" and "Country", not in capitals!
#memory.limit(size=50000)

year_run <- format(Sys.Date(), "%Y")

folder <- ("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten")  ## set the working directory
finfolder <- paste("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/files_country", year_run, sep="_") #folder where the files per country will be saved
setwd(folder)

tabmf <- read.csv(paste("classif_KBAs_FINAL_", year_run, ".csv", sep = ""))   ## file with types of kbas
isos <- read.csv(paste("Iso_countries_", year_run, ".csv", sep = ""))            ## file with ISO codes; should be stored in the wkfolder specified above; no changes in 2019, so 2018 file used

#### 03. Read in shapefiles ----

kbas <- st_read(dsn = '../../../KBA/KBAsGlobal_2019_September_02', layer = 'KbaGlobal_2019_September_02_POL', stringsAsFactors = F) #note field called Shape not geometry
#kbas<-st_read(dsn = '.', layer = 'KBAs_for_2019_SDG', stringsAsFactors = F, geometry_column = T)
kbas <- kbas[!is.na(kbas$SitRecID),] #remove any NAs

#pas<- st_read(dsn = 'WDPA_Dec2018_SDGs.gdb', layer = 'WDPA_Dec_2018_Merge', stringsAsFactors=F) 
pas <- st_read(dsn = '../../../Protected Areas/wdpa_nov2019/wdpa_for_birdlife/wdpa_poly_nov2019_sdg_prep.gdb', layer = 'wdpa_poly_nov2019_sdg_prep', stringsAsFactors = F) 

#### TODO: CHECK GEOMETRY TYPES - continue from here: https://github.com/r-spatial/sf/issues/427
pas <- pas[!is.na(st_dimension(pas)),]
as.character(unique(st_geometry_type(st_geometry(pas)))) ## what geometries are in the dataset

#kbas <- st_make_valid(kbas) #repair any geometry issues
#pas <- st_make_valid(pas) #repair any geometry issues


## convert factors to characters in the dataframes
## PAs dataframe
pas$ISO3 <- as.character(pas$ISO3)
pas$PARENT_ISO <- as.character(pas$PARENT_ISO)
str(pas)


#########################################################################
#### SECTION 1 - DATA CLEANING ----
#########################################################################

## only need to run the following lines until the ISO3 in the kba layer is corrected - 
## this changes the correct #ISO3 in the PA layer to match the wrong ISO in the kba layer. 
## Otherwise these #bas are excluded because the ISO3 in the two layers don't match. 
## When the ISO3 in the kba layer is corrected, these lines should be deleted.

#### 1.01 - fixing issues in ISO codes ----
## TODO: Replace this section with code matching ISO codes from the tabmf table.

# levels(pas$ISO3)[levels(pas$ISO3)=='ALA'] <- 'FIN'
# levels(pas$ISO3)[levels(pas$ISO3)=='ASC'] <- 'SHN'
# levels(pas$ISO3)[levels(pas$ISO3)=='CPT'] <- 'FRA'
# levels(pas$ISO3)[levels(pas$ISO3)=='GGY'] <- 'GBR'
# levels(pas$ISO3)[levels(pas$ISO3)=='IMN'] <- 'GBR'
# levels(pas$ISO3)[levels(pas$ISO3)=='JEY'] <- 'GBR'
# levels(pas$ISO3)[levels(pas$ISO3)=='TAA'] <- 'SHN'
# levels(pas$ISO3)[levels(pas$ISO3)=='WAK'] <- 'UMI'
# levels(pas$ISO3)[levels(pas$ISO3)=='XAD'] <- 'CYP'
# levels(pas$ISO3)[levels(pas$ISO3)=='XKO'] <- 'SRB'
# levels(pas$ISO3)[levels(pas$ISO3)=='XNC'] <- 'CYP'

pas$ISO3[(pas$ISO3)=='ALA'] <- 'FIN'
pas$ISO3[(pas$ISO3)=='ASC'] <- 'SHN'
pas$ISO3[(pas$ISO3)=='CPT'] <- 'FRA'
pas$ISO3[(pas$ISO3)=='GGY'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='IMN'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='JEY'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='TAA'] <- 'SHN'
pas$ISO3[(pas$ISO3)=='WAK'] <- 'UMI'
pas$ISO3[(pas$ISO3)=='XAD'] <- 'CYP'
pas$ISO3[(pas$ISO3)=='XKO'] <- 'SRB'
pas$ISO3[(pas$ISO3)=='XNC'] <- 'CYP'

unassigned_pas <- pas[pas$ISO3 == " " | is.na(pas$ISO3) | pas$ISO3 == '---',]

#### 1.02 - KBAs with no ISO code ----
unique(kbas$ISO3)
unique(kbas$Country[kbas$ISO3 == "---"])
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "High Seas"] <- "ABNJ"
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "Falkland Islands (Malvinas)"] <- "FLK"
#kbas$ISO3[kbas$ISO3 == "---" & kbas$Country != "High Seas"] <- "RUS"

unique(kbas$Country[kbas$ISO3 == " "])
unique(kbas$Country[is.na(kbas$ISO3)])
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Palau"] <- "PLW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Aruba"] <- "ABW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Aruba (to Netherlands)"] <- "ABW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Guadeloupe"] <- "GLP"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Guadeloupe (to France)"] <- "GLP"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Norfolk Island"] <- "NFK"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Norfolk Island (to Australia)"] <- "NFK"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Lao People's Democratic Republic"] <- "LAO"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Laos"] <- "LAO"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "India"] <- "IND"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Cuba"] <- "CUB"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Libya"] <- "LBY"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Belarus"] <- "BLR"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Russian Federation"] <- "RUS"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Russia (Asian)"] <- "RUS"

kbas <- kbas[kbas$Country != 'Disputed',] #remove any sites that cannot be assigned a country as are disputed

unassigned_kbas <- kbas[kbas$ISO3 == " " | is.na(kbas$ISO3) | kbas$ISO3 == '---',] #check if any sites don't have an ISO3 code, if any are missing, add in country name (if non are missing, will have 0 observations)
# site 27335 missing - in Belarus
#kbas$ISO3[kbas$SitRecID == 27335] <- 'BLR'
#kbas$Country[kbas$SitRecID == 27335] <- 'Belarus'


## Fill in the country field for these sites as well
#kbas$Country[kbas$ISO3 == "RUS"] <- "Russian Federation"


kbas_without_names <- kbas[kbas$Country == " ",] #checks if any KBAs are missing country names, should be 0, if not find out which sites are missing country names and add in country name


#### 1.03 Transboundary PAs ----
cnpa <- data.frame(ISO3 = unique(pas$ISO3))
cnpa$nchart <- nchar(as.character(cnpa$ISO3))
cnpa <- cnpa[cnpa$nchart>4, ] #where iso3 codes have more than 4 characters (more than one country per site)
cnpa
transb <- data.frame()
for (g in 1:nrow(cnpa)){ #this loop checks each transboundary pa and splits the iso code and only assigns the site to each country
  #g=2
  cnpa1 <- cnpa[g, ]
  sp <- substr(cnpa1$ISO3, 4, 5)
  if (sp == "; "){
    cnpa2 <- data.frame(ISO3=strsplit(as.character(cnpa1$ISO3), split="; ")[[1]])
    cnpa2$oISO3 <- as.character(cnpa1$ISO3)
    }
  if (sp != "; "){
    cnpa2 <- data.frame(ISO3=strsplit(as.character(cnpa1$ISO3), split=";")[[1]])
    cnpa2$oISO3 <- as.character(cnpa1$ISO3)
    }
  transb <- rbind(transb, cnpa2)
}
transb

#### 1.04 - list of countries ----
# DgProjw <- CRS(proj4string(kbas)) #checks coordinate system - DEPRECATED in sf
kbas #check that in the console output: proj4string:    +proj=longlat +datum=WGS84 +no_defs

kbas <- kbas[!is.na(kbas$SitRecID),] #remove any NAs

listcnts <- as.character(unique(kbas$ISO3))
lu(listcnts)

#for reruns
#listcnts <- as.character(unique(kbas$ISO3[kbas$ISO3 %in% c('FIN', 'SHN', 'FRA', 'GBR', 'UMI', 'CYP', 'SRB')])) #missed countries
#lu(listcnts)

#########################################################################
#### SECTION 2 - SPATIAL ANALYSIS ----
#########################################################################



##### OVERLAP WITH PROTECTED AREAS
### per country

finaltab <- data.frame()
tt <- proc.time()
for (x in 1:length(listcnts)){ #starts loop for all countries
#for (x in 1:10){ #starts loop for all countries
  #x=1
  #x=8
  #x <- which(listcnts==country) #find x based on country name
  country <- listcnts[x]
  
  ## 1. Subset kbas and pas to this country
  kba.c <- kbas[kbas$ISO3 == country, ]
  pa.c <- pas[pas$ISO3 == country, ]  ## protected areas within the country
  
  #finds PA in country for transboundary sites and so includes in pa country list
  if (country %in% transb$ISO3){ 
    iso3 <- c(country, transb$oISO3[country == transb$ISO3])
    iso3
    pa.c <- pas[pas$ISO3 %in% iso3, ]
  }
 
  ## 2. Print country name and ISO3 code to console
  country.n <- kba.c$Country[1]
  cat(x, '\t', country, '\t', country.n, '\n')  
 
  ## 3. Plot map of KBAs and PAs to check ----
  plotit <- F
  if(plotit){
    plot(kba.c$Shape, border=3)#kbas are in green
    plot(pa.c$Shape, border=4, add=T) # pas are in blue
    title(main=paste(country.n, country))
    box()
    axis(1)
    axis(2)
  }
  
  ### TODO get rid of this bit when we've added in some preliminary analysis to 
  ## REMOVE ALL COUNTRIES WITH NO PAs.
  nrow(pa.c) ## number of PAs in the country
  if (nrow(pa.c) == 0){ #finds all kbas with no protected area overlap - sets all output to 0 (0 overlap, no. of pas it overlaps with are 0, etc)
    areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = NA, ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, ISO = country, COUNTRY = country.n) 
  }
  
    # areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = st_area(kba.c$Shape), ovl = 0, year = 0, nPAs = 0, percPA = 0, ISO = country, COUNTRY = country.n) #try to set KBA area even if no overlap
  #}
  
  ## TODO: this will then no longer need an if statement.
  if (nrow(pa.c) > 0){
    ovkba <- NULL
    ovkba <- st_intersects(pa.c$Shape, kba.c$geometry, sparse = FALSE)
    ovkba ## matrix where rows = PAs, and cols = KBAs
    nrow(ovkba)
      
    if (length(ovkba) == 0){ #if there is no matrix produced, this is an error so set all outputs to error i.e. 9999
      areasov <- data.frame(SitRecID = NA, kba = NA, ovl = NA, year = NA, random = F, nPAs = NA, percPA = NA, ISO = country, COUNTRY = country.n)
    }
      
    if (length(ovkba) > 0){ #if there ARE overlaps between kbas and pas:
      areasov <- data.frame()
      #####next bit is new code which re-assigns missing years to a randomly selected year from PAs in the respective country # should be in data cleaning
      pa.c$random <- F
      if (min(pa.c$STATUS_YR) == 0){
        for (row in 1:nrow(pa.c)){
          if (pa.c$STATUS_YR[row] == 0){ #create a new column to identify any site that has had a year randomly allocated
            pa.c$random[row]  <- T
          }
        }
        ryears <- pa.c$STATUS_YR[pa.c$STATUS_YR > 0] #select all years where the status year isn't 0
        if (length(ryears) == 0){ #if all status years are 0
          ryears <- pas$STATUS_YR[pas$STATUS_YR > 1986] #then use range of status years for all protected areas (not just in this country) later than 1986
        }
        if (length(ryears) == 1){ #if only one year that is not 0
          ryears <- c(ryears, ryears)
        }
        pa.c$STATUS_YR[pa.c$STATUS_YR == 0] <- base::sample(ryears, nrow(pa.c[pa.c$STATUS_YR == 0, ]), replace = T) ## selects a year randomly from the pool of possible years
      }
        ########### end of pa year randomisation

      for (z in 1:nrow(kba.c)){ ## starts loop for all kbas in the country (changed to nrow as was looking at columns rather than rows)
      #for (z in 1:length(kba.c)){ ## starts loop for all kbas in the country
        #z = 1
        #z=which(kbac$SitRecID=="23937")
        #z=which(kba.c$SitRecID=="26878")
        #print(z)
        kbaz <- kba.c[z, ]
        head(kbaz)
        akba <- NA #set to 9999 to incase next steps don't run
        #akba <- suppressWarnings(tryCatch({gArea(kbaz, byid=FALSE)}, error=function(e){}))
        akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbaz$geometry, byid = FALSE)}, error=function(e){})))
        akba
        
        #length(which(ovkba[z, ] == T)) # number of KBAs overlapping with protected area z  
        length(which(ovkba[ ,z] == T)) # find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
          
        if (length(which(ovkba[ ,z] == T)) > 0){  ### when at least 1 pa overlaps with the kba
            #win.graph()
          
          #pacz <- pa.c[which(ovkba[z, ]==T), ] #seems to be incorrect index 
          pacz <- pa.c[which(ovkba[ ,z] == T), ] #subset to pas that overlap this kba
          nrow(pacz)
          pacz
          
          #pacz2 <- st_make_valid((pacz)) #repair geometry of pacz if necessary

          if (plotit){
            plot(kbaz$Shape)
            plot(pacz$Shape, col=rgb(0,0,.8,0.2), border=0, add=T)
          }

          yearspacz <- pacz$STATUS_YR #years of pas in kba z
          ovf <- NULL
            
          #ovf <- tryCatch({gIntersection(pacz, kbaz, byid=T)}, error=function(e){}) ## spatial intersection kba and all pas overlapping
          ovf <- tryCatch({st_intersection(pacz, kbaz)}, error = function(e){}) ## spatial intersection kba and all pas overlapping, results in polygon output for each overlap (in sf/dataframe)
          #TODO this line doesn't always run if there is interesting geometry within the PA layer.
          nrow(ovf)
          class(ovf)
          
          # if (class(ovf)=="SpatialCollections"){
          #   ovf <- ovf@polyobj
          #   rnames <- paste(row.names(pacz), row.names(kbaz))
          #   yearspacz <- yearspacz[which(rnames %in% row.names(ovf))]
          # } # don't think this is needed - exports polygons from spatial collections?
          # 
          # if (class(ovf) != "SpatialPolygons"){
          #   ovf=NULL
          # }
            
          if ("sf" %in% class(ovf) & length(yearspacz) > 0){
          #if (class(ovf) == "SpatialPolygons" & length(yearspacz) > 0){
            #ovfpol <- SpatialPolygonsDataFrame(ovf, data=data.frame(id=row.names(ovf), row.names=row.names(ovf), year=yearspacz)) ## converts ovf into a spatial polygon data frame
            ovfpol <- ovf #not needed but avoiding having to rename subsequent dataframes
            years <- sort(unique(ovfpol$STATUS_YR))
            years
              
            year1 <- min(years)
            ovf1 <- ovfpol[ovfpol$STATUS_YR == year1, ]
            nrow(ovf1) #changed from length
            #ovf11 <- NULL
            #ovf11 <- tryCatch({unionSpatialPolygons(ovf1, ovf1$year)}, error=function(e){}) #combines all overlap of kba z by pas in year 1
            ovf11 <- tryCatch({st_union(ovf1, by_feature = F)}, error=function(e){})
            #nrow(ovf11)
            #if(plotit) plot(ovf11$Shape, add=T, col=2)
            if(plotit) plot(ovf11, col = 2)
              
            ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf11, byid = FALSE)}, error=function(e){})))
            #ovlz <- suppressWarnings(tryCatch({gArea(ovf11, byid=FALSE)}, error=function(e){}))
            if (length(ovlz) == 0){ #if there was an error, assign overlap to be 9999 (signifying an error)
              ovlz <- NA
            }
            ovlz
            
            random0 <- pacz$random[pacz$STATUS_YR == year1] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
            
            random1 <- FALSE #sets random to false by default - i.e. PA designation has a given year
            random1[TRUE %in% random0] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
              
            areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year1, random = random1, nPAs=nrow(ovf1)) #creates row in output table with this site overlap area and associated information within it #sets numbers to numeric not units (removes m^2)
            areasov1
              
            if (length(years) > 1){
              for (w in 2:length(years)){
              #w=2
              #w=4
              #w=6
              
              rema <- 1-(sum(areasov1$ovl[!is.na(areasov1$ovl)])/akba)  ## to see if there is still any area left by the pas of year 1
              rema
              if (rema > 0.02){ #assuming 2% error in delineation of kbas compared to pas
                year2 <- years[w]
                year2
                ovf2 <- ovfpol[ovfpol$STATUS_YR == year2, ]
                nrow(ovf2)
                ovf22 <- NULL
                #ovf22 <- tryCatch({unionSpatialPolygons(ovf2, ovf2$year)}, error=function(e){})
                #length(ovf22)
                ovf22 <- tryCatch({st_union(ovf2, by_feature = F)}, error=function(e){})
                #if(plotit) plot(ovf22$Shape, add=T, col=w+1)
                if(plotit){
                  plot(ovf22, add=T, col=w+1)
                }
                
                ovfprev <- ovfpol[ovfpol$STATUS_YR < year2, ]
                #ovfprev1 <- sf::st_buffer(st_as_sf(ovfprev), dist=0)
                #ovfprev2 <- sf::as_Spatial(ovfprev1)
                #ovfprev3 <- tryCatch({unionSpatialPolygons(ovfprev2,ovfprev2$new)}, error=function(e){}) #merge all polygons from previous years
                #ovfprev3 <- tryCatch({st_union(ovfprev2, ovfprev2$new)}, error=function(e){}) #merge all polygons from previous years
                ovfprev3 <- tryCatch({st_union(ovfprev, by_feature = FALSE)}, error=function(e){}) #merge all polygons from previous years
                if(plotit){
                  plot(ovfprev3, add=T, col=w+2)
                }

                ovf23 <- NULL

                #ovf23 <- tryCatch({gDifference(ovf22, ovfprev3)}, error=function(e){}) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                
                ovf23 <- tryCatch({st_difference(ovf22, ovfprev3)}, error = function(e){}) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                                
                #ovf23 <- st_difference(ovf2, ovfprev)
                
                if(plotit){
                  plot(ovf23, add=T, col="grey")
                }
                ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf23, byid = FALSE)}, error = function(e){})))
                
                if (length(ovlz)==0){
                  ovlz <- NA
                }
                ovlz
                
                random2 <- pacz$random[pacz$STATUS_YR == year2] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
                
                random3 <- FALSE #sets random to false by default - i.e. PA designation has a given year
                random3[TRUE %in% random2] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
                
                areasov1 <- rbind(areasov1,data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year2, random = random3, nPAs=nrow(ovf2)))
                areasov1
                }
              }
            }
          areasov1
          }  # ends loop for    class(ovf)=="SpatialPolygons"
        #if (nrow(ovf) == 0 | ovf == NULL | !"sf" %in% class(ovf)){
        if (is.null(ovf) | !"sf" %in% class(ovf)){
          areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=NA, year=0, random=F, nPAs=0)  ## error in spatial overlap
        }
        }  ## ends loop for PAs overlapping with the KBA
      if (length(which(ovkba[ ,z] == T)) == 0){
        areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=0, year=0, random=F, nPAs=0)   ## if there are NO (zero/none) pas overlapping the kba
      }
      areasov <- rbind(areasov,areasov1)
      }  ## ends loop for all kbas in the country

    areasov$percPA <- areasov$ovl/areasov$kba
    areasov
    max(areasov$percPA)
    areasov$ISO <- country
    areasov$COUNTRY <- country.n

    } # ends loop for ovlkba>0
  }  ## ends loop for length(pac)>1
  
  finaltab <- rbind(finaltab,areasov)

  tname <- paste(finfolder,"/",country.n, ".csv", sep="")
  tname
  write.csv(areasov, tname, row.names=F)

}
(proc.time()-tt)[1]/60 ## time in minutes

head(finaltab)
str(finaltab)
lu(finaltab$x) #not sure what suppposed to do

finaltab <- unique(finaltab)

write.csv(finaltab, "finaltab_2020.csv", row.names = F)
### end here
#########################################################################################


isofiles = T ## if we need to bind all country files in a single table

if (isofiles){
  fls <- dir(finfolder)
  final1 <- data.frame()
  for (y in 1:lu(fls)) {
    final1 <- rbind(final1, read.csv(paste(finfolder, fls[y], sep = '/')))
  }
##my edits as cannot load lu
#fls2=list.files(finfolder)
#final2 <- Reduce(rbind, lapply(fls2, read.csv))
}


#write.csv(final1, "finaltab_2020.csv", row.names = F) #only run if didn't run finaltab writing above (i.e. if compiling all countries later)

lu(final1$ISO)

if (isofiles == F){
  final1 <- finaltab
}

final1 <- read_csv("finaltab_2020.csv") #can restart from here
final <- final1[!is.na(final1$ovl),]
lu(final1$SitRecID) - lu(final$SitRecID) # number of KBAs with problems of geometry





##################################### below is work around for reclassifying random years - no longer need this
# ## add some code to reclassify random years as 0 so can be set to be 3000
# #TODO need to improve for next years run
# #pas_ryr <- st_set_geometry(pas, NULL) #create tabular record of pas
# pas_ryr <- pas[pas$STATUS_YR == 0,] #identify protected areas that had missing years
# country2 <- unique(pas_ryr$ISO3) #create a list of countries that contain protected areas with missing years
# #pas_ryr2 <- pas[pas$ISO3 %in% country2,] #subset protected areas list to contain all sites within countries that contain missing years
# kbas_ryr <- kbas[kbas$ISO3 %in% country2,]
# 
# 
# yr3000 <- data.frame() #intersect all kbas in country with 0 years for their protected area designation, to identify kbas that ovelap with protected areas with unknown year of designation
# for (i in 1:length(country2)){
#   #i = 2
#   kbasC <- kbas_ryr[kbas_ryr$ISO == country2[i],]
#   pasC <- pas_ryr[pas_ryr$ISO3 == country2[i],]
#   int <- suppressWarnings(tryCatch({st_intersects(kbasC$geometry, pasC$Shape, sparse = FALSE)})) #creates matrix
#   
#   #length(int2[int2==T]) #number of true records
#   if (!is.null(int)){
#     int2 <- as.data.frame(int) #convert matrix to dataframe
#     colnames(int2) <- pasC$WDPA_PID #set column names to wdpaID
#     int2$kbaID <- kbasC$SitRecID #add in KBA SitRecID
#     int2 <- gather(int2, WDPAID, overlap, colnames(int2[1:(ncol(int2)-1)])) #converts matrix to list of kbas and pas that overlap
#     
#     int2$ISO3 <- country2[i] #add country name
#     int2 <- int2[int2$overlap == TRUE,] #filters to only sites that overlap
#     
#     yr3000 <- rbind(yr3000, int2) #adds to table of kba-pa overlaps with random years
#   }
# }
# 
# yr3000_2 <- data.frame() #intersect kbas in country thave have overlap pas with missing years with all pas in that country to get a full list of kba protected area overlap and years
# for (i in 1:length(country2)){
#   #i = 1
#   kbasC <- kbas_ryr[kbas_ryr$ISO == country2[i],] 
#   kbasC2 <- kbasC[kbasC$SitRecID %in% unique(yr3000$kbaID), ] #subset to kbas that do overlap with protected areas with missing years
#   
#   pasC <- pas[pas$ISO3 == country2[i],] #subset to all protected areas in a country
#   int <- suppressWarnings(tryCatch({st_intersects(kbasC2$geometry, pasC$Shape, sparse = FALSE)})) #creates matrix
#   
#   #length(int2[int2==T]) #number of true records
#   if (!is.null(int)){
#     int2 <- as.data.frame(int) #convert matrix to dataframe
#     colnames(int2) <- pasC$WDPA_PID #set column names to wdpaID
#     int2$kbaID <- kbasC2$SitRecID #add in KBA SitRecID
#     int2 <- gather(int2, WDPAID, overlap, colnames(int2[1:(ncol(int2)-1)])) #converts matrix to list of kbas and pas that overlap
#     
#     int2$ISO3 <- country2[i] #add country name
#     int2 <- int2[int2$overlap == TRUE,] #filters to only sites that overlap
#     pasC2 <- select(pasC, c('WDPA_PID', 'STATUS_YR'))
#     pasC2 <- st_set_geometry(pasC2, NULL)
#     int3 <- merge(int2, pasC2, by.x = 'WDPAID', by.y = 'WDPA_PID', all.x = T)
#     
#     yr3000_2 <- rbind(yr3000_2, int3) #adds to table of kba-pa overlaps with random years
#   }
# }
# 
# 
# ########## use above to create shadow list and then run it at site (rather than country) level for if years exist
# 
# #paYr <- st_set_geometry(pas, NULL) #create version of pa layer without geometry
# #paYr <- pas[ ,c(19, 28)] #subset to status year and iso3
# 
# final_ry <- final[final$SitRecID %in% yr3000$kbaID, ] #subset country overlap table to sites where random years have been included
# 
# final_ry$year2 <- final_ry$year #create a new year column to change to 3000 if random year
# 
# kba_ry <- yr3000$kbaID #create list of kbas with random years
# 
# final_ry3000 <- data.frame()
# for (n in 1:length(kba_ry)){ #loop through kbas with random years
#   #n <- 1
#   final_ryS <- final[final$SitRecID == kba_ry[n], ] #subset country overlap table to the kba in the loop where random years have been included
#   final_ryS$year2 <- final_ryS$year #create a new year column to change to 3000 if random year
#   
#   if (nrow(final_ryS) > 0){
#     
#     for (row in 1:nrow(final_ryS)){
#       if (!(final_ryS$year[row] %in% yr3000_2$STATUS_YR[yr3000_2$kbaID == kba_ry[n]])){ #subset out years that weren't in the status years of the protected areas each kba overlaps with, and hence theese are the random years
#         final_ryS$year2[row] <- 3000
#       }
#     }
#     final_ry3000 <- rbind(final_ry3000, final_ryS)
#   }
# }
# 
# tabu <- unique(final_ry3000[final_ry3000$year2 == 3000,]) #create file of kba years that were randomised
# tabu <- select(tabu, c('SitRecID', 'year')) #create tabu file with years to override
# 
# pkba <- unique(tabu$SitRecID)
# for (g in 1:length(pkba)){ #for the identified random years, this reclassifies them as 3000 so they can undergo randomisation in the next section of the code
#   #g = 1
#   kba4 <- pkba[g]
#   kba4
#   pkba1 <- unique(tabu$year[tabu$SitRecID == kba4])
#   ppa1 <- unique(final$year[final$SitRecID == kba4])
#   
#   if (T %in% !ppa1 %in% pkba1){
#     final$year[final$SitRecID == kba4 &! final$year %in% pkba1] <- 3000
#   }
# }
# 
# write.csv(final, 'finaltab2.csv', row.names = F)

# country2 <- unique(paYr$ISO3[paYr$STATUS_YR == 0]) #create a list of countries that have missing years
# 
# tabff_ryr2 <- data.frame()
# for (n in 1:length(country2)){ #loop through countries that have random years
#   #n <- 1
#   country2[n] #note need to run protected area updates from start if running from here are some sites such as Jersey (JEY) need to be changed (to GBR in this example)
#   tabff_ryrC <- tabff[tabff$ISO == country2[n], ] #create sub records for each country
#   paYrC <- paYr[paYr$ISO3 == country2[n], ]
# 
#   if (country2[n] != 'ESH'){ #ESH iso caused problems in loop
#     for (row in 1:nrow(tabff_ryrC)){
#       if (!(tabff_ryrC$year[row] %in% paYrC$STATUS_YR)){
#         tabff_ryrC$year[row] <- 3000
#       }
#     }
#   }
#   tabff_ryr2 <- rbind(tabff_ryr2, tabff_ryrC) #combine countries into a new dataframe
# }



#####################################################

str(final)
str(tabmf)

tabf <- merge(final, tabmf, by = "SitRecID")
str(tabf)
head(tabf)
max(tabf$percPA)
tabf$percPA[tabf$percPA > 1] <- 1

max(tabf$percPA)
resf <- with(tabf, aggregate(percPA, list(SitRecID = SitRecID), sum))
max(resf$x)
nrow(resf[resf$x > 1,]) # number of kbas for which the sum of the overlap of the multiple years is more than 1 (can be due to geometry oversimplification in gDifference step)

kbas2fix <- unique(resf$SitRecID[resf$x>1])
lu(kbas2fix)
#rescale kbas perc prot
if (length(kbas2fix) > 0){
  for (k in 1:length(kbas2fix)){
    tabf[tabf$SitRecID == kbas2fix[k], ]$percPA = tabf[tabf$SitRecID == kbas2fix[k], ]$percPA/sum(tabf[tabf$SitRecID == kbas2fix[k], ]$percPA)
  }
}

#check if was fixed (result should be 1)
max(with(tabf, aggregate(percPA, list(SitRecID = SitRecID), sum))$x)


#tabf=read.csv("tabf_propa.csv")
## subset for confirmed ibas


## link regions
isos$ISO <- isos$ISO3

str(tabf)
tabff <- merge(tabf, isos, by = "ISO")
str(tabff)



unique(tabff$year)
tabff$year[tabff$year == 0 & tabff$ovl != 0 & !is.na(tabff$ovl)] <- 3000  ## PAs with no year of designation; assume 3000 for posterior randomization (NOTE that changed in 2017; random process was added before the spatial overlap)

#unique(tabff$ovl[tabff$year == 3000]) ## should be NULL; only the kbas with 0 overlap with PAs have year=0, because random year was added to the codes before the overlapping step (modified in 2017)

#write.csv(tabff, "tabff.csv", row.names=F)
#tabff=read.csv("tabff.csv")

#tables to export

fields <- c("global", "COUNTRY", "countryname","Developing","LDC","LLDC_SIDS","IPBES_region", "region", "SDG_Region","ISO_BL","ISO_SDG","SDG_Subregion") #Country and Region added back following output of last years list, add in "World_bank_class", "CMS_region", etc as appropriate


# tfname <- "Input data for R global KBAs.csv"
# tfname
# tab2export <- with(tabff, data.frame(siteid = SitRecID, country = COUNTRY, perprotected = percPA*100, year = year, random_year = random))
# str(tab2export)
# head(tab2export)
# tail(tab2export)
# write.csv(tab2export, tfname, row.names = F)

for (f in 1:length(fields)){
  #f=7
  ff <- as.character(fields[f])
  ff
  
  tfname <- paste("Input data for R", ff, "KBAs.csv", sep=" ")
  tfname
  if (ff == "global"){
    tab2export <- with(tabff, data.frame(siteid = SitRecID, region = ff, country = COUNTRY, perprotected = percPA*100, year = year, random_year = random))
  }
  if (ff != "global"){
    tab2export <- with(tabff, data.frame(siteid = SitRecID, region = tabff[,which(names(tabff) == ff)], country = COUNTRY, perprotected = percPA*100, year = year, random_year = random))
  }
  str(tab2export)
  head(tab2export)
  tail(tab2export)
  write.csv(tab2export, paste("input tables", format(Sys.Date(), " %Y/"), tfname, sep=""), row.names = F)
}

