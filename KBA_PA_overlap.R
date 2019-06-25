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
kpacks <- c('sf', 'dplyr')  ### install automatically the relevant packages
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

### packages set 2 (FOR TESTING ONLY)
kpacks2 <- c('maptools','rgdal')  ### install automatically the relevant packages
new.packs <- kpacks2[!(kpacks2 %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks2, require, character.only=T)
remove(kpacks2, new.packs)

#### 01.2 Define functions ----
lu=function (x=x){
  length(unique(x))
}

#### 02. set file locations and working directories ----

## NB.in the KBA layer attribute table, the relevant fields should be "SitRecID" and "Country", not in capitals!
#memory.limit(size=50000)

folder=("C:\\Users\\eliza\\OneDrive\\Documents\\BirdLife\\SDG\\KBA-PA_overlap\\1_Overlap")  ## set the working directory
finfolder="C:\\Users\\eliza\\OneDrive\\Documents\\BirdLife\\SDG\\KBA-PA_overlap\\1_Overlap\\filespcountry_2019_test" #folder where the files per country will be saved
setwd(folder)

tabmf=read.csv("classif_kbas_FINAL2019_revised.csv")   ## file with types of kbas
isos=read.csv("Iso_countries_2018.csv")            ## file with ISO codes; should be stored in the wkfolder specified above; no changes in 2019, so 2018 file used

#### 03. Read in shapefiles ----

ogr.kbas <- readOGR("../PERU", layer = "SGD_KBA_PERU")    ### loads the kba layer from the geodatabase specified; might take several minutes to load
ogr.pas <- readOGR("../PERU", layer = "PERU_SDG_PA", dropNULLGeometries = T, require_geomType = 'wkbPolygon') 

kbas <- st_read("../PERU", layer = "SGD_KBA_PERU")
pas <- st_read("../PERU", layer = "PERU_SDG_PA")
## remove null (empty) geometries (NB. I'm not sure if this will work)

#### TODO: CHECK GEOMETRY TYPES - continue from here: https://github.com/r-spatial/sf/issues/427
pas <- pas[!is.na(st_dimension(pas)),] ## still got an error of full WDPA dataset
as.character(unique(st_geometry_type(st_geometry(pas)))) ## what geometries are in the dataset


## convert factors to characters in the dataframes
## PAs dataframe
pas$ISO3 <- as.character(pas$ISO3)
pas$PARENT_ISO <- as.character(pas$PARENT_ISO)
str(pas)
## KBAs dataframe
i <- sapply(kbas, is.factor)
kbas[i] <- lapply(kbas[i], as.character)
str(kbas)

#########################################################################
#### SECTION 1 - DATA CLEANING ----
#########################################################################

## only need to run the following lines until the ISO3 in the kba layer is corrected - 
## this changes the correct #ISO3 in the PA layer to match the wrong ISO in the kba layer. 
## Otherwise these #bas are excluded because the ISO3 in the two layers don't match. 
## When the ISO3 in the kba layer is corrected, these lines should be deleted.

#### 1.01 - fixing issues in ISO codes ----
## TODO: Replace this section with code matching ISO codes from the tabmf table.
levels(pas$ISO3)[levels(pas$ISO3)=='ALA'] <- 'FIN'
levels(pas$ISO3)[levels(pas$ISO3)=='ASC'] <- 'SHN'
levels(pas$ISO3)[levels(pas$ISO3)=='CPT'] <- 'FRA'
levels(pas$ISO3)[levels(pas$ISO3)=='GGY'] <- 'GBR'
levels(pas$ISO3)[levels(pas$ISO3)=='IMN'] <- 'GBR'
levels(pas$ISO3)[levels(pas$ISO3)=='JEY'] <- 'GBR'
levels(pas$ISO3)[levels(pas$ISO3)=='TAA'] <- 'SHN'
levels(pas$ISO3)[levels(pas$ISO3)=='WAK'] <- 'UMI'
levels(pas$ISO3)[levels(pas$ISO3)=='XAD'] <- 'CYP'
levels(pas$ISO3)[levels(pas$ISO3)=='XKO'] <- 'SRB'
levels(pas$ISO3)[levels(pas$ISO3)=='XNC'] <- 'CYP'


#### 1.02 - KBAs with no ISO code ----
unique(kbas$ISO3)
unique(kbas$Country[kbas$ISO3 == "---"])
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "High Seas"] <- "ABNJ"
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country != "High Seas"] <- "RUS"
## Fill in the country field for these sites as well
kbas$Country[kbas$ISO3 == "RUS"] <- "Russia"

#### 1.03 Transboundary PAs ----
cnpa <- data.frame(ISO3 = unique(pas@data$ISO3))
cnpa$nchart <- nchar(as.character(cnpa$ISO3))
cnpa <- cnpa[cnpa$nchart>4, ] #where iso3 codes have more than 4 characters (more than one country per site)
cnpa
transb <- data.frame()
for (g in 1:nrow(cnpa)){ #this loop checks each transboundary pa and splits the iso code and only assigns the site to the first country
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
listcnts <- as.character(unique(kbas$ISO3))
lu(listcnts)


#########################################################################
#### SECTION 2 - SPATIAL ANALYSIS ----
#########################################################################



##### OVERLAP WITH PROTECTED AREAS
### per country

finaltab <- data.frame()
tt <- proc.time()
for (x in 1:length(listcnts)){ #starts loop for all countries
  x=1
  #country <- 'PER'
  #x <- which(listcnts==country) #find x based on country name
  country <- listcnts[x]
  
  ## 1. Subset kbas and pas to this country
  kba.c <- kbas[kbas$ISO3 == country, ]
  pa.c <- pas[pas$ISO3 == country, ]  ## protected areas within the country
  
  ## 2. Print country name and ISO3 code to console
  country.n <- kba.c$Country[1]
  cat(x, '\t', country, '\t', country.n, '\n')
  
  # if (country %in% transb$ISO3){ #finds PA in country for transboundary sites
  #   iso3 <- c(country, transb$oISO3[country == transb$ISO3])
  #   iso3
  #   pac <- pas[pas$ISO3 %in% iso3, ]
  # }
  
  ## 3. Plot map of KBAs and PAs to check ----
  plotit <- T
  if(plotit){
    plot(kba.c$geometry, border=3, xlim=c(min(kba.c$SitLong), max(kba.c$SitLong)), ylim=c(min(kba.c$SitLat), max(kba.c$SitLat))) #kbas are in green
    plot(pa.c$geometry, border=4, add=T) # pas are in blue
    title(main=paste(country.n, country))
    box()
    axis(1)
    axis(2)
  }
  
  ### TODO get rid of this bit when we've added in some preliminary analysis to 
  ## REMOVE ALL COUNTRIES WITH NO PAs.
  nrow(pa.c) ## number of PAs in the country
  if (nrow(pa.c) == 0){ #finds all kbas with no protected area overlap - sets all output to 0 (0 overlap, no. of pas it overlaps with are 0, etc)
    areasov <- data.frame(SitRecID = bac$SitRecID, kba = 9999, ovl = 0, year = 0, nPAs = 0, percPA = 0, ISO = country, COUNTRY = countryn) 
  }
  
  ## TODO: this will then no longer need an if statement.
  if (length(pac) > 0){
    ovkba <- NULL
    ovkba <- st_intersects(pa.c, kba.c, sparse = FALSE)
    ovkba ## matrix where rows = PAs, and cols = KBAs
      
    if (length(ovkba) == 0){
      areasov=data.frame(SitRecID=9999, kba=9999, ovl=9999, year=9999, nPAs=9999, percPA=9999,ISO=country,COUNTRY=countryn)
    }
      
    if (length(ovkba) > 0){ #if there ARE overlaps between kbas and pas:
      areasov <- data.frame()
      #####next bit is new code which re-assigns missing years to a randomly selected year from PAs in the respective country # should be in data cleaning
      if (min(pac@data$STATUS_YR) == 0){
        ryears=pac@data$STATUS_YR[pac@data$STATUS_YR > 0]
        if (length(ryears)==0){
          ryears=pas@data$STATUS_YR[pas@data$STATUS_YR > 1986]
        }
        if (length(ryears)==1){
          ryears=c(ryears, ryears)
        }
          
        pac@data$STATUS_YR[pac@data$STATUS_YR==0] <- sample(ryears, nrow(pac@data[pac@data$STATUS_YR==0, ]), replace = T) ## selects a year randomly from the pool of possible years
      }
        ########### end of pa year randomisation

      for (z in 1:length(kbac)){ ## starts loop for all kbas in the country
        #z=2
        #z=which(kbac$SitRecID=="23937")
        #print(z)
        kbaz <- kbac[z, ]
        head(kbaz@data)
        akba <- 9999 #set to 9999 to incase next steps don't run
        akba <- suppressWarnings(tryCatch({gArea(kbaz, byid=FALSE)}, error=function(e){}))
        akba
          
        length(which(ovkba[z,]==T)) #find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
          
        if (length(which(ovkba[z,]==T))>0)  ### when at least 1 pa overlaps with the kba
          {
            #win.graph()
            
          pacz <- pac[which(ovkba[z, ]==T), ]
          length(pacz)
          pacz@data

          if (plotit){
            plot(kbaz)
            plot(pacz, col=rgb(0,0,.8,0.2), border=0, add=T)
          }

          yearspacz <- pacz@data$STATUS_YR #years of pas in kba z
          ovf <- NULL
            
          ovf <- tryCatch({gIntersection(pacz, kbaz, byid=T)}, error=function(e){}) ## spatial intersection kba and all pas overlapping
          length(ovf)
          class(ovf)
          
          if (class(ovf)=="SpatialCollections"){
            ovf <- ovf@polyobj
            rnames <- paste(row.names(pacz@data), row.names(kbaz@data))
            yearspacz <- yearspacz[which(rnames %in% row.names(ovf))]
          } # don't think this is needed - exports polygons from spatial collections?

          if (class(ovf) != "SpatialPolygons"){
            ovf=NULL
          }
            
          if (class(ovf) == "SpatialPolygons" & length(yearspacz) > 0){
            ovfpol <- SpatialPolygonsDataFrame(ovf, data=data.frame(id=row.names(ovf), row.names=row.names(ovf), year=yearspacz)) ## converts ovf into a spatial polygon data frame
            ovfpol@data
            years <- sort(unique(ovfpol$year))
            years
              
            year1 <- min(years)
            ovf1 <- ovfpol[ovfpol$year==year1, ]
            length(ovf1)
            ovf11 <- NULL
            ovf11 <- tryCatch({unionSpatialPolygons(ovf1, ovf1@data$year)}, error=function(e){}) #combines all overlap of kba z by pas in year 1
            length(ovf11)
            if(plotit) plot(ovf11, add=T, col=2)
              
            ovlz <- suppressWarnings(tryCatch({gArea(ovf11, byid=FALSE)}, error=function(e){}))
            if (length(ovlz)==0) ovlz=9999
            ovlz
              
            areasov1 <- data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=ovlz, year=year1, nPAs=length(ovf1))
            areasov1
              
            if (length(years) > 1){
              for (w in 2:length(years)){
              w=4
              rema <- 1-(sum(areasov1$ovl[areasov1$ovl < 9999])/akba)  ## to see if there is still any area left by the pas of year 1
              rema
              if (rema > 0.02){ #assuming 2% error in delineation of kbas compared to pas
                year2 <- years[w]
                year2
                ovf2 <- ovfpol[ovfpol$year==year2, ]
                length(ovf2)
                ovf22 <- NULL
                ovf22 <- tryCatch({unionSpatialPolygons(ovf2, ovf2@data$year)}, error=function(e){})
                length(ovf22)
                if(plotit) plot(ovf22, add=T, col=w+1)
                ovfprev <- ovfpol[ovfpol$year<year2, ]
                ovfprev$new <- 1 #create new column to identify which polygons to merge - i.e. merge all polygons from previous years
                ovfprev1 <- sf::st_buffer(st_as_sf(ovfprev), dist=0)
                ovfprev2 <- sf::as_Spatial(ovfprev1)
                ovfprev3 <- tryCatch({unionSpatialPolygons(ovfprev2,ovfprev2@data$new)}, error=function(e){}) #merge all polygons from previous years
                ovfprev3 <- unionSpatialPolygons(ovfprev2,ovfprev2@data$new) #merge all polygons from previous years
                plot(ovfprev3, add=T, border=2)

                ovf23=NULL

#### original code written by maria - this wouldn't work for the 2019 analyses on some machines (the gdifference caused R to throw a fatal error and close the program if within the tryCatch(), or to do a standard error if used without the tryCatch()), so if this is the case, comment out (using '#') the following three lines of code and then uncomment the three lines of code that are contained between the line of #'s                  
                ovf23 <- tryCatch({gDifference(ovf22, ovfprev3)}, error=function(e){}) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                                
                ovf23 <- gDifference(ovf22, ovfprev3)
                
                
                if(plotit) plot(ovf23, add=T, col="grey")
                ovlz=suppressWarnings(tryCatch({gArea(ovf23, byid=FALSE)}, error=function(e){}))
                
#######################################################################################################                
#this and the following two lines are to replace the creation of teh ovf23 polygon (to identify the differences between the polygon and the previous year), and then calculate the area change.
                
                #ovf22a<-suppressWarnings(gArea(ovf22))  
                #ovfpreva<-suppressWarnings(gArea(ovfprev))
                #ovlz<-ovf22a-ovfpreva #take the difference in area between the two polygons and hence identify the increase in area
########################################################################################################
                
                if (length(ovlz)==0){
                  ovlz=9999
                }
                ovlz
                
                areasov1=rbind(areasov1,data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=ovlz, year=year2, nPAs=length(ovf2)))
                areasov1
                }
              }
            }
          areasov1

          }  # ends loop for    class(ovf)=="SpatialPolygons"
        if (length(ovf)==0|class(ovf)!="SpatialPolygons") areasov1=data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=9999, year=0, nPAs=0)  ## error in spatial overlap
        }  ## ends loop for PAs overlapping with the KBA
      if (length(which(ovkba[z,]==T))==0) areasov1=data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=0, year=0, nPAs=0)   ## no pas overlapping the kba
      areasov=rbind(areasov,areasov1)
      }  ## ends loop for all kbas in the country

    areasov$percPA=areasov$ovl/areasov$kba
    areasov
    max(areasov$perc)
    areasov$ISO=country
    areasov$COUNTRY=countryn

    } # ends loop for ovlkba>0
  }  ## ends loop for length(pac)>1

  finaltab=rbind(finaltab,areasov)
  
  tname=paste(finfolder,"/",countryn, ".csv", sep="")
  tname
  write.csv(areasov, tname, row.names=F)

}
(proc.time()-tt)[1]/60 ## time in minutes

head(finaltab)
str(finaltab)
lu(finaltab$x)
write.csv(finaltab, "finaltab.csv", row.names=F)
### end here
#########################################################################################





##################### countries with severe geometry problems (preventing gIntersects to run)

### USA x=226      ### Jordan (JOR) x=100
redoiso=T
if (redoiso)
  {
  #x=which(listcnts=="USA")
  #x=which(listcnts=="JOR")
  x=which(listcnts=="PER")
  
  
  
  country=as.character(listcnts[x])
  print(country)
  
  kbac=kbas[kbas$ISO3==country,]
  pac=pas[pas$ISO3==country,]
  
  if (country%in%transb$ISO3)
    {
    iso3=c(country,transb$oISO3[country==transb$ISO3])
    iso3
    pac=pas[pas$ISO3%in%iso3,]
    }
  
  countryn=kbac@data$Country[1]
  print(countryn)
  
  plotit=F
    if(plotit){
    plot(cntc, border=2, lwd=2)
    plot(kbac, border=3,add=T)
    plot(pac, add=T)
    title(main=paste(countryn, country))
    box()
    axis(1)
    axis(2)
    }
  
  length(pac)
  
  validpas=data.frame(WDPAID=pac@data$WDPAID, valid=0)
  for (l in 1:length(pac))
    {
    valid=tryCatch({gIsValid(pac[l,])}, error=function(e){})
    if (length(valid)>0) validpas$valid[l]=valid
    }
  #write.csv(validpas, "validpas_USA.csv", row.names=F)
  #validpas=read.csv("validpas_USA.csv")
  validpas1=which(validpas$valid==1)
  pac=pac[validpas1,]
  length(pac)
  sum(validpas$valid)
  
  
  ovkba=NULL
  ovkba=tryCatch({gIntersects(pac,kbac, byid=T)}, error=function(e){})
  length(ovkba)
  
  if (length(ovkba)==0) areasov=data.frame(SitRecID=9999, kba=9999, ovl=9999, year=9999, nPAs=9999, percPA=9999,ISO=country,Country=countryn, x=x)
  
  if (length(ovkba)>0)
    {
    areasov=data.frame()
    
    #####next bit is new code which re-assigns missing years to a randomly selected year from PAs in the respective country
    if (min(pac@data$STATUS_YR)==0)
      {
      ryears=pac@data$STATUS_YR[pac@data$STATUS_YR>0]
      if (length(ryears)==0) ryears=pas@data$STATUS_YR[pas@data$STATUS_YR>1986]
      if (length(ryears)==1) ryears=c(ryears, ryears)
      pac@data$STATUS_YR[pac@data$STATUS_YR==0]= sample(ryears,nrow(pac@data[pac@data$STATUS_YR==0,]), replace=T) ## selects a year randomly from the pool of possible years
      }
    
    for (z in 1:length(kbac))    #length(kbac)
      { ## starts loop for all kbas in the country
      #z=11
      #z=which(kbac$SitRecID=="25154")
      #print(z)
      kbaz=kbac[z,]
      head(kbaz@data)
      akba=9999
      akba=suppressWarnings(tryCatch({gArea(kbaz, byid=FALSE)}, error=function(e){}))
      akba
      
      length(which(ovkba[z,]==T))
      
      if (length(which(ovkba[z,]==T))>0)  ### when at least 1 pa overlaps with the kba
        {
        #win.graph()
        
        pacz=pac[which(ovkba[z,]==T),]
        length(pacz)
        pacz@data
        
          if (plotit){
          plot(kbaz)
          plot(pacz, col=rgb(0,0,.8,0.2), border=0, add=T)
          }
        
      yearspacz=pacz@data$STATUS_YR
      ovf=NULL
      
      ovf=tryCatch({gIntersection(pacz,kbaz, byid=T)}, error=function(e){}) ## spatial intersection kba and all pas overlapping
      length(ovf)
      class(ovf)
      
      if (class(ovf)=="SpatialCollections")
        {
        ovf=ovf@polyobj
        rnames=paste(row.names(pacz@data),row.names(kbaz@data))
        yearspacz=yearspacz[which(rnames%in%row.names(ovf))]
        }
      
      if (class(ovf)!="SpatialPolygons")  ovf=NULL
      
      if (class(ovf)=="SpatialPolygons" & length(yearspacz)>0)
        {
        
        ovfpol=SpatialPolygonsDataFrame(ovf, data=data.frame(id=row.names(ovf), row.names=row.names(ovf), year=yearspacz)) ## converts  ovf in a spatial polygon data frame
        ovfpol@data
        years=sort(unique(ovfpol$year))
        years
        
        year1=min(years)
        ovf1=ovfpol[ovfpol$year==year1,]
        length(ovf1)
        ovf11=NULL
        ovf11=tryCatch({unionSpatialPolygons(ovf1, ovf1@data$year)}, error=function(e){})
        length(ovf11)
        if(plotit) plot(ovf11, add=T, col=1)
        
        ovlz=suppressWarnings(tryCatch({gArea(ovf11, byid=FALSE)}, error=function(e){}))
        if (length(ovlz)==0) ovlz=9999
        ovlz
        
        areasov1=data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=ovlz, year=year1, nPAs=length(ovf1))
        areasov1
        
         if (length(years)>1) #if record for more than 1 year
           {
           for (w in 2:length(years))
             {
             #w=2
             rema=1-(sum(areasov1$ovl[areasov1$ovl<9999])/akba)  ## to see if there is still any area left
             rema
              if (rema>0.02)
                {
                year2=years[w]
                year2
                ovf2=ovfpol[ovfpol$year==year2,]
                length(ovf2)
                ovf22=NULL
                ovf22=tryCatch({unionSpatialPolygons(ovf2, ovf2@data$year)}, error=function(e){})
                length(ovf22)
                if(plotit) plot(ovf22, add=T, col=w+1)
                ovfprev=ovfpol[ovfpol$year<year2,] #original code
                #ovfprev=ovfpol[ovfpol$year==years[w-1],] #potential change in code
                if(plotit) plot(ovfprev, add=T, col=1) 
                
                ovf23=NULL

#### original code written by maria - this wouldn't work for the 2019 analyses on some machines (the gdifference caused R to throw a fatal error and close the program if within the tryCatch(), or to do a standard error if used without the tryCatch()), so if this is the case, comment out (using '#') the following three lines of code and then uncomment the three lines of code that are contained between the line of #'s                  
                
                #ovf23=tryCatch({gDifference(ovf22, ovfprev)}, error=function(e){}) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                
                if(plotit) plot(ovf23, add=T, col="grey")
                ovlz=suppressWarnings(tryCatch({gArea(ovf23, byid=FALSE)}, error=function(e){}))
                
#######################################################################################################                
#this and the following two lines are to replace the creation of teh ovf23 polygon (to identify the differences between the polygon and the previous year), and then calculate the area change.
                
                # ovf22a<-suppressWarnings(gArea(ovf22))
                # ovf22a
                # ovfpreva<-suppressWarnings(gArea(ovfprev))
                # ovfpreva
                # ovlz<-ovf22a-ovfpreva #take the difference in area between the two polygons and hence identify the increase in area
                ovlz 
########################################################################################################
                
                 
                 if (length(ovlz)==0) ovlz=9999
                 #if (length(ovlz)<=0) ovlz=9999 #to ignore negative values
                 ovlz 
                 
                 areasov1=rbind(areasov1,data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=ovlz, year=year2, nPAs=length(ovf2))) 
                 areasov1
                }
             }
           }
        areasov1
        
        }  # ends loop for    class(ovf)=="SpatialPolygons"
      if (length(ovf)==0|class(ovf)!="SpatialPolygons") areasov1=data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=9999, year=0, nPAs=0)  ## error in spatial overlap
      }  ## ends loop for PAs overlapping with the KBA
    if (length(which(ovkba[z,]==T))==0) areasov1=data.frame(SitRecID=kbaz@data$SitRecID, kba=akba, ovl=0, year=0, nPAs=0)   ## no pas overlapping the kba
    areasov=rbind(areasov,areasov1)
    }  ## ends loop for all kbas in the country
  
  areasov$percPA=areasov$ovl/areasov$kba
  areasov
  max(areasov$perc)
  areasov$ISO=country
  areasov$COUNTRY=countryn
  
  } # ends loop for ovlkba>0

tname=paste(finfolder,"/",countryn, ".csv", sep="")
tname
write.csv(areasov, tname, row.names=F)  ## save files for countries that needed to be reanalysed
}
####################################






##############################################

isofiles=T ## if we need to bind all country files in a single table

if (isofiles)
{
fls=dir(finfolder)
final1=data.frame()
for (y in 1:lu(fls)) {
  final1=rbind(final1, read.csv(paste(finfolder, fls[y], sep= '/')))
}

##my edits as cannot load lu
#fls2=list.files(finfolder)
#final2 <- Reduce(rbind, lapply(fls2, read.csv))
}

write.csv(final1, "finaltab.csv", row.names=F)



lu(final1$ISO)

if (isofiles==F) final1=finaltab
final1=read.csv("finaltab.csv")
final=final1[final1$ovl!=9999,]
lu(final1$SitRecID)-lu(final$SitRecID) # number of KBAs with problems of geometry

str(final)
str(tabmf)

tabf=merge(final, tabmf, by="SitRecID")
str(tabf)
head(tabf)
max(tabf$percPA)
tabf$percPA[tabf$percPA>1]=1

max(tabf$percPA)
resf=with(tabf, aggregate(percPA, list(SitRecID=SitRecID), sum))
max(resf$x)
nrow(resf[resf$x>1,]) # number of kbas for which the sum of the overlap of the multiple years is more than 1 (can be due to geometry oversimplification in gDifference step

kbas2fix=unique(resf$SitRecID[resf$x>1])
lu(kbas2fix)
#rescale kbas perc prot
if (length(kbas2fix)>0) for (k in 1:length(kbas2fix)) tabf[tabf$SitRecID==kbas2fix[k],]$percPA=tabf[tabf$SitRecID==kbas2fix[k],]$percPA/sum(tabf[tabf$SitRecID==kbas2fix[k],]$percPA)

#check if was fixed (result should be 1)
max(with(tabf, aggregate(percPA, list(SitRecID=SitRecID), sum))$x)


#tabf=read.csv("tabf_propa.csv")
## subset for confirmed ibas


## link regions
isos$ISO<-isos$ISO3

str(tabf)
tabff=merge(tabf, isos, by="ISO")
str(tabff)

unique(tabff$year)
tabff$year[tabff$year==0&tabff$ovl!=0]=3000  ## PAs with no year of designation; assume 3000 for posterior randomization (NOTE that changed in 2017; random process was added before the spatial overlap)

unique(tabff$ovl[tabff$year==3000]) ## should be NULL; only the kbas with 0 overlap with PAs have year=0, because random year was added to the codes before the overlapping step (modified in 2017)

#write.csv(tabff, "tabff.csv", row.names=F)
#tabff=read.csv("tabff.csv")

#tables to export

fields=c("COUNTRY", "countryname","Developing","LDC","LLDC_SIDS","IPBES_region", "region", "SDG_Region","ISO_BL","ISO_SDG","SDG_Subregion", "World_bank_class") #Country and Region added back following output of last years list


tfname="Input data for R global KBAs.csv"
tfname
tab2export=with(tabff, data.frame(siteid=SitRecID, country=COUNTRY, perprotected=percPA*100, year=year))
str(tab2export)
head(tab2export)
tail(tab2export)
write.csv(tab2export,tfname, row.names=F)

for (f in 1:length(fields))
{
#f=7
ff=as.character(fields[f])
ff

tfname=paste("Input data for R ", ff, "KBAs.csv")
tfname
tab2export=with(tabff, data.frame(siteid=SitRecID, region=tabff[,which(names(tabff)==ff)], country=COUNTRY, perprotected=percPA*100, year=year))
str(tab2export)
head(tab2export)
tail(tab2export)
write.csv(tab2export,tfname, row.names=F)
}
