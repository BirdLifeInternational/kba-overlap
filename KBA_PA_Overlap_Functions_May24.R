##Functions required to complete the Protected Areas/OECMs overlap of Key Biodiversity Areas. 
#Developed by BirdLife International, May 2024 
#Version 3.0
#load_packages----
load_packages <- function(){
  kpacks <- c("sf","tidyverse","lwgeom","lemon","cli")  #List of required packages
  new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]  #Checks if any of these packages are not yet installed
  if(length(new.packs)) install.packages(new.packs)  #Installs any missing packages
  lapply(kpacks, require, character.only=T)  
  remove(kpacks, new.packs)
  sf_use_s2(FALSE) #Sets sf to assume planar geometry - as this analysis looks for ratio of two areas (total KBA area vs KBA area covered by PA) this is usable
  assign("sf_use_s2",sf_use_s2, envir = .GlobalEnv)
  cli_alert_success("Packages successfully loaded.")
}

#build_directory----
build_directory <- function(){
  if (!(file.exists(paste(folder, "/files_country_", year_run, sep = "")))){ #create folder to save output csvs of kba-pa overlaps for each country within working directory
    dir.create(paste(folder, "/files_country_", year_run, sep = ""))}
  if (!(file.exists(paste(folder, "/input tables ", year_run, sep = "")))){ #create tables folder
    dir.create(paste(folder, "/input tables ", year_run, sep = ""))}
  if (!(file.exists(paste(folder, "/output tables ", year_run, sep = "")))){ #create tables folder
    dir.create(paste(folder, "/output tables ", year_run, sep = ""))}
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "")))){ #create tables folder
    dir.create(paste(folder, "/final graphs ", year_run, sep = ""))}
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "", "/all")))){ #create tables subfolder
    dir.create(paste(folder, "/final graphs ", year_run, sep = "", "/all"))}
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "", "/Freshwater")))){ #create tables subfolder
    dir.create(paste(folder, "/final graphs ", year_run, sep = "", "/Freshwater"))}  
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "", "/marine")))){ #create tables subfolder
    dir.create(paste(folder, "/final graphs ", year_run, sep = "", "/marine"))}    
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "", "/mountain")))){ #create tables subfolder
    dir.create(paste(folder, "/final graphs ", year_run, sep = "", "/mountain"))} 
  if (!(file.exists(paste(folder, "/final graphs ", year_run, sep = "", "/terrestrial")))){ #create tables subfolder
    dir.create(paste(folder, "/final graphs ", year_run, sep = "", "/terrestrial"))} 
  if (!(file.exists(paste(folder, "/IBATFig5 ", year_run, sep = "")))){ #create tables folder
    dir.create(paste(folder, "/IBATFig5 ", year_run, sep = ""))}
  if (!(file.exists(paste(folder, "/IBATFig5 ", year_run, sep = "", "/all")))){ #create tables subfolder
    dir.create(paste(folder, "/IBATFig5 ", year_run, sep = "","/all"))}
  if (!(file.exists(paste(folder, "/IBATFig5 ", year_run, sep = "", "/aze")))){ #create tables subfolder
    dir.create(paste(folder, "/IBATFig5 ", year_run, sep = "","/aze"))}
  if (!(file.exists(paste(folder, "/IBATFig5 ", year_run, sep = "", "/iba")))){ #create tables subfolder
    dir.create(paste(folder, "/IBATFig5 ", year_run, sep = "","/iba"))}
  cli_alert_success("Full file directory built successfully.")
}

#kba_clean----
kba_clean <- function(){
  kbas <- st_make_valid(kbas) #repair any geometry issues
  
  if(nrow(filter(kbas, ISO3 == "---" & Country == "High Seas"))){
  kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "High Seas"] <- "ABNJ" } #Add high seas ISO code
  
  kbas <- kbas[!is.na(kbas$SitRecID),] #remove any NAs - should not be needed but just in case
  
  kbas <- kbas[kbas$Country != 'Disputed',] #remove any sites that cannot be assigned a country as are disputed - very uncommon
  kbas$ISO3[kbas$SitRecID == 2707] <- 'GGY' #Adding Additional ISOs to KBA layer
  kbas$ISO3[kbas$SitRecID == 2706] <- 'JEY' #Adding Additional ISOs to KBA layer
  kbas$ISO3[(kbas$SitRecID) %in% c(2701,2702,2703,2704)] <- 'IMN' #Adding Additional ISOs to KBA layer
  kbas$Country[kbas$SitRecID == 2707] <- 'Guernsey' #Adding Additional ISOs to KBA layer
  kbas$Country[kbas$SitRecID == 2706] <- 'Jersey' #Adding Additional ISOs to KBA layer
  kbas$Country[kbas$SitRecID %in% c(2701,2702,2703,2704)] <- 'Isle of Man' #Adding Additional ISOs to KBA layer
  
  cli_alert_success("KBA layer cleaned successfully.")
  
  if(nrow(filter(kbas, is.na(ISO3))) > 0){
    cli_alert_warning(paste("Warning:",nrow(filter(kbas, is.na(ISO3))),"KBAs have been removed from the analysis as they are missing ISO3 codes.\n These sites have the following SitRecIDs:\n",
                         paste(filter(kbas,is.na(ISO3))$SitRecID, collapse = ", ")))}
  
  kbas <- kbas[!is.na(kbas$ISO3),] #remove any transboundary
  
  kbas_iso3 <- kbas$ISO3 #Extract the values in the ISO3 column of kbas dataframe
  pas_iso <- pas$ISO #Extract the values in the ISO column of pas dataframe
  iso3_not_in_pas <- unique(kbas_iso3[!kbas_iso3 %in% pas_iso]) #Find the values in kbas_iso3 that are not in pas_iso
  if(length(iso3_not_in_pas)>0){
    cli_alert_warning(paste("Warning: The following ISO3 codes exist within the KBA but not PA layer, as the script uses these codes to select \n potentially overlapping sites, KBAs with these ISO codes will be reported as having 0 coverage:","\n", paste(iso3_not_in_pas, collapse = ", "))) #Print the vector to the console
  }
    
  assign("kbas",kbas, envir = .GlobalEnv)  
}

#transboundary_pas----
transboundary_pas <- function(){
  cnpa <- data.frame(ISO3 = unique(pas$ISO3))
  cnpa$nchart <- nchar(as.character(cnpa$ISO3))
  cnpa <- cnpa[cnpa$nchart > 4, ] #where iso3 codes have more than 4 characters (more than one country per site)
  transb <- data.frame()
  if(length(cnpa > 0)){
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
  cli_alert_success("Transboundary PAs split successfully.")
  }else{cli_alert_success("No transboundary PAs found in dataset.")}
  assign("transb", transb, envir = .GlobalEnv)
}

#pa_oecm_split----
pa_oecm_split <- function(){
  if(OECM_split){
    OECMs <- filter(pas, PA_DEF == 0)
    PAs <- filter(pas, PA_DEF == 1)
    
    PAs_to_clip <- PAs[OECMs,]
    
    OECMs_clip <- st_difference(OECMs,st_union(PAs_to_clip)) #Removing any parts of OECMs which overlap with PAs
    
    pas <- rbind(PAs,OECMs_clip)
    
    OECMcnts <- unique(OECMs$ISO3) #list of countries for which there are OECMs
    
    #### 2.6 - define PA/OECM/Combined combinations to use
    OECMs_list <- pas %>% 
      filter(PA_DEF == 0) %>% 
      mutate(Site_Type = "OECMs") %>% 
      select(c("Site_Type","WDPAID")) %>% 
      st_drop_geometry()
    
    PAs_list <- pas %>% 
      filter(PA_DEF == 1) %>% 
      mutate(Site_Type = "Protected Areas") %>% 
      select(c("Site_Type","WDPAID")) %>% 
      st_drop_geometry()
    
    All_list <- pas %>% 
      mutate(Site_Type = "Protected Areas and OECMs") %>% 
      select(c("Site_Type","WDPAID")) %>% 
      st_drop_geometry()
    
    PAlist <- rbind(OECMs_list,PAs_list,All_list)
    
    patypes <- unique(PAlist$Site_Type)
    
    rm(OECMs_list,PAs_list,All_list)
    
    cli_alert_success("PAs and OECMs successfully split.")
  }
  
  #########If not splitting OECMs out from PAs
  if(!OECM_split){
    All_list <- pas %>% 
      mutate(Site_Type = "Protected Areas and OECMs") %>% 
      select(c("Site_Type","WDPAID")) %>% 
      st_drop_geometry()
    
    PAlist <- All_list
    
    patypes <- unique(All_list$Site_Type)
    
    rm(All_list)
    cli_alert_success("Step complete, OECM coverage will not be disaggregated from that of PAs.")
    
    OECMcnts <- data.frame() #creates empty data frame 
    
  }
  assign("OECMcnts",OECMcnts, envir = .GlobalEnv)
  assign("pas",pas, envir =  .GlobalEnv)
  assign("PAlist", PAlist, envir = .GlobalEnv)
  assign("patypes", patypes, envir = .GlobalEnv)
  
}

#complete_overlap----
complete_overlap <- function(){
  # Defines the names of the two geometry columns in the datasets (assuming they are final columns) - these are used in this function and means that they do not need to be user specified
  kba_geom <- names(kbas)[length(names(kbas))]
  pas_geom <- names(pas)[length(names(pas))]
  
  ##### OVERLAP WITH PROTECTED AREAS and OECMs per country
  
  finaltab <- data.frame()
  tt <- proc.time()
  for (x in 1:length(listcnts)){ #starts loop for all countries
    #x=8 #to do one specific country
    #x <- which(listcnts==country) #find x based on country name
    country <- listcnts[x]
    #creates dataframe to eventually include combinations of OECMs, PAs and All combined
    areasovfinal <- data.frame()
    
    ## 1. Subset kbas and pas to this country
    kba.c <- kbas[kbas$ISO3 == country, ]
    pa.c <- pas[pas$ISO3 == country, ]  ## protected areas within the country
    
    country.n <- kba.c$Country[1]
    cli_alert_info(paste("Analysing coverage for ",country.n," (",nrow(filter(kbas, ISO3 == country))," KBAs).", sep = ""))
    
    ######################
    
    #finds PA in country for transboundary sites and so includes in pa country list
    if (country %in% transb$ISO3){ 
      iso3 <- c(country, transb$oISO3[country == transb$ISO3])
      pa.c <- pas[pas$ISO3 %in% iso3, ]
    }
    
    if(country %in% OECMcnts){
      ##Does for OECMS, PAs and then the two combined
      for(p in 1:length(patypes)){ 
        focal_pas <- patypes[p]
        
        focal_pas_list <- filter(PAlist, Site_Type == paste(focal_pas))
        
        pa.c.filt <- filter(pa.c, WDPAID %in% focal_pas_list$WDPAID)
        
        ## 3. Plot map of KBAs and PAs to check ----
        plotit <- F
        if(plotit){
          plot(kba.c[,kba_geom], border=3)#kbas are in green
          plot(pa.c.filt[,pas_geom], border=4, add=T) # pas are in blue
          title(main=paste(country.n, country))
          box()
          axis(1)
          axis(2)
        }
        
        ### could refine by removing this bit when we've added in some preliminary analysis to REMOVE ALL COUNTRIES WITH NO PAs.
        nrow(pa.c.filt) ## number of PAs in the country
        if (nrow(pa.c.filt) == 0){ #finds all kbas with no protected area overlap - sets all output to 0 (0 overlap, no. of pas it overlaps with are 0, etc)
          areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = NA, WDPA_type = paste(focal_pas),ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, ISO = country, country = country.n) 
        }
        
        ## this may then no longer need an if statement.
        if (nrow(pa.c.filt) > 0){
          ovkba <- NULL
          ovkba <- suppressMessages(st_intersects(pa.c.filt[,pas_geom], kba.c[,kba_geom], sparse = FALSE))
          
          if (length(ovkba) == 0){ #if there is no matrix produced, this is an error so set all outputs to error i.e. 9999
            areasov <- data.frame(SitRecID = NA, kba = NA, WDPA_type = paste(focal_pas), ovl = NA, year = NA, random = F, nPAs = NA, percPA = NA, ISO = country, COUNTRY = country.n)
          }
          
          if (length(ovkba) > 0){ #if there ARE overlaps between kbas and pas:
            areasov <- data.frame()
            #####next bit is new code which re-assigns missing years to a randomly selected year from PAs in the respective country # should be in data cleaning
            pa.c.filt$random <- F
            if (min(pa.c.filt$STATUS_YR) == 0){
              for (row in 1:nrow(pa.c.filt)){
                if (pa.c.filt$STATUS_YR[row] == 0){ #create a new column to identify any site that has had a year randomly allocated
                  pa.c.filt$random[row]  <- T
                }
              }
              ryears <- pa.c.filt$STATUS_YR[pa.c.filt$STATUS_YR > 0] #select all years where the status year isn't 0
              if (length(ryears) == 0){ #if all status years are 0
                ryears <- pas$STATUS_YR[pas$STATUS_YR > 1986] #then use range of status years for all protected areas (not just in this country) later than 1986
              }
              if (length(ryears) == 1){ #if only one year that is not 0
                ryears <- c(ryears, ryears)
              }
              pa.c.filt$STATUS_YR[pa.c.filt$STATUS_YR == 0] <- base::sample(ryears, nrow(pa.c.filt[pa.c.filt$STATUS_YR == 0, ]), replace = T) ## selects a year randomly from the pool of possible years
            }
            ########### end of pa year randomisation
            
            for (z in 1:nrow(kba.c)){ ## starts loop for all kbas in the country (changed to nrow as was looking at columns rather than rows)
              #for (z in 1:length(kba.c)){ ## starts loop for all kbas in the country
              #z=which(kba.c$SitRecID=="19780")
              kbaz <- kba.c[z, ]
              akba <- NA #set to NA to be replaced further on
              akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbaz[,kba_geom])}, error=function(e){})))
              
              length(which(ovkba[ ,z] == T)) # find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
              
              if (length(which(ovkba[ ,z] == T)) > 0){  ### when at least 1 pa overlaps with the kba
                
                pacz <- pa.c.filt[which(ovkba[ ,z] == T), ] #subset to pas that overlap this kba
                pacz <- st_make_valid(pacz)
                
                
                if (plotit){
                  plot(kbaz$geometry)
                  plot(pacz$Shape, col=rgb(0,0,.8,0.2), border=0, add=T)
                }
                
                yearspacz <- pacz$STATUS_YR #years of pas in kba z
                ovf <- NULL
                
                ovf <- suppressMessages(tryCatch({st_intersection(pacz, kbaz)}, error = function(e){}) )## spatial intersection kba and all pas overlapping, results in polygon output for each overlap (in sf/dataframe)
                #TODO this line doesn't always run if there is interesting geometry within the PA layer.
                class(ovf)
                
                if ("sf" %in% class(ovf)& length(yearspacz) > 0){  
                  
                  ovfpol <- ovf #not needed but avoiding having to rename subsequent dataframes
                  years <- sort(unique(ovfpol$STATUS_YR))
                  
                  year1 <- min(years)
                  ovf1 <- ovfpol[ovfpol$STATUS_YR == year1, ]
                  ovf11 <- NULL
                  
                  if(!is.null(ovf1)){
                    ovf1 <- st_make_valid(ovf1)}
                  
                  ovf11 <- suppressMessages(tryCatch({st_union(ovf1, by_feature = F)}, error=function(e){}))
                  if(plotit) plot(ovf11, col = 2)
                  
                  if(!is.null(ovf11)){
                    ovf11 <- st_make_valid(ovf11)}
                  
                  ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf11)}, error=function(e){})))
                  
                  
                  if (length(ovlz) == 0){ 
                    ovlz <- 0
                  }
                  
                  random0 <- pacz$random[pacz$STATUS_YR == year1] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
                  
                  random1 <- FALSE #sets random to false by default - i.e. PA designation has a given year
                  random1[TRUE %in% random0] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
                  
                  areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=ovlz, year=year1, random = random1, nPAs=nrow(ovf1)) #creates row in output table with this site overlap area and associated information within it #sets numbers to numeric not units (removes m^2)
                  
                  if (length(years) > 1){
                    for (w in 2:length(years)){
                      
                      rema <- 1-(sum(areasov1$ovl[!is.na(areasov1$ovl)])/akba)  ## to see if there is still any area left by the pas of year 1
                      if (rema > 0.02){ #assuming 2% error in delineation of kbas compared to pas
                        year2 <- years[w]
                        ovf2 <- ovfpol[ovfpol$STATUS_YR == year2, ]
                        ovf22 <- NULL
                        
                        ovf22 <- suppressMessages(tryCatch({st_union(ovf2, by_feature = F)}, error=function(e){}))
                        
                        if(plotit){
                          plot(ovf22, add=T, col=w+1)
                        }
                        
                        ovfprev <- ovfpol[ovfpol$STATUS_YR < year2, ]
                        
                        if(!is.null(ovfprev)){
                          ovfprev <- st_make_valid(ovfprev)}
                        
                        ovfprev3 <- suppressMessages(tryCatch({st_union(ovfprev, by_feature = F)}, error=function(e){})) #merge all polygons from previous years
                        
                        if(plotit){
                          plot(ovfprev3, add=T, col=w+2)
                        }
                        
                        ovf23 <- NULL
                        
                        if(!is.null(ovfprev3)){
                          ovfprev3 <- st_make_valid(ovfprev3)}
                        
                        if(!is.null(ovf22)){
                          ovf22 <- st_make_valid(ovf22)}
                        
                        ovf23 <- suppressMessages(tryCatch({st_difference(st_geometry(ovf22), st_geometry(ovfprev3))}, error = function(e){})) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                        
                        if(plotit){
                          plot(ovf23, add=T, col=c("grey"))
                        }
                        if(!is.null(ovf23)){
                          ovf23 <- st_make_valid(ovf23)}
                        
                        if(length(ovf23) > 1){
                          ovf23 <- suppressMessages(tryCatch({st_union(ovf23)}, error = function(e){}) )
                        }
                        
                        #This removes any non-polygon elements from collapsed polygons eg multilines
                        if ("sfc_GEOMETRYCOLLECTION" %in% class(ovf23)){
                          ovf23 <- st_collection_extract(ovf23, "POLYGON", warn = F)}
                        ####
                        
                        if(length(ovf23) > 1){
                          ovf23 <- suppressMessages(tryCatch({st_union(ovf23)}, error = function(e){}) )#union didnt work so tried combine to collapse all polygons
                        }
                        
                        if(!is.null(ovf23)){
                          ovf23 <- st_make_valid(ovf23)}
                        
                        
                        ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf23)}, error = function(e){})))
                        
                        if (length(ovlz)==0){ #if there is no overlap (no new overlap by the PA?), set value to NA (should change to 0?###HAVE NOW CHANGED THIS TO 0)
                          ovlz <- 0
                        }
                        
                        random2 <- pacz$random[pacz$STATUS_YR == year2] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
                        
                        random3 <- FALSE #sets random to false by default - i.e. PA designation has a given year
                        random3[TRUE %in% random2] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
                        
                        areasov1 <- rbind(areasov1,data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=ovlz, year=year2, random = random3, nPAs=nrow(ovf2)))
                        
                      }
                    }
                  }
                  
                }  # ends loop for    class(ovf)=="SpatialPolygons"
                
                if (is.null(ovf)){
                  areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba,WDPA_type = paste(focal_pas), ovl=0, year=year1, random=F, nPAs=0)}
              }  ## ends loop for PAs overlapping with the KBA
              if (length(which(ovkba[ ,z] == T)) == 0){
                areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=0, year=0, random=F, nPAs=0)   ## if there are NO (zero/none) pas overlapping the kba
              }
              areasov <- rbind(areasov,areasov1)
            }  ## ends loop for all kbas in the country
            
            areasov$percPA <- areasov$ovl/areasov$kba
            
            areasov$ISO <- country
            
            areasov$country <- country.n
            
          } # ends loop for ovlkba>0
        }  ## ends loop for length(pac)>1
        
        areasovfinal <- rbind(areasovfinal, areasov)
        
      }
    }else{ #performs this if not a country with OECMs
      
      focal_pas <- "Protected Areas and OECMs"
      
      focal_pas_list <- filter(PAlist, Site_Type == paste(focal_pas))
      
      pa.c.filt <- filter(pa.c, WDPAID %in% focal_pas_list$WDPAID)
      
      ## 3. Plot map of KBAs and PAs to check ----
      plotit <- F
      if(plotit){
        plot(kba.c[,kba_geom], border=3)#kbas are in green
        plot(pa.c.filt[,pas_geom], border=4, add=T) # pas are in blue
        title(main=paste(country.n, country))
        box()
        axis(1)
        axis(2)
      }
      
      ### could refine by removing this bit when we've added in some preliminary analysis to REMOVE ALL COUNTRIES WITH NO PAs.
      nrow(pa.c.filt) ## number of PAs in the country
      if (nrow(pa.c.filt) == 0){ #finds all kbas with no protected area overlap - sets all output to 0 (0 overlap, no. of pas it overlaps with are 0, etc)
        areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = NA, WDPA_type = paste(focal_pas),ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, ISO = country, country = country.n) 
      }
      
      ## this may then no longer need an if statement.
      if (nrow(pa.c.filt) > 0){
        ovkba <- NULL
        ovkba <- suppressMessages(st_intersects(pa.c.filt[,pas_geom], kba.c[,kba_geom], sparse = FALSE))
        
        if (length(ovkba) == 0){ #if there is no matrix produced, this is an error so set all outputs to error i.e. 9999
          areasov <- data.frame(SitRecID = NA, kba = NA, WDPA_type = paste(focal_pas), ovl = NA, year = NA, random = F, nPAs = NA, percPA = NA, ISO = country, COUNTRY = country.n)
        }
        
        if (length(ovkba) > 0){ #if there ARE overlaps between kbas and pas:
          areasov <- data.frame()
          #####next bit is new code which re-assigns missing years to a randomly selected year from PAs in the respective country # should be in data cleaning
          pa.c.filt$random <- F
          if (min(pa.c.filt$STATUS_YR) == 0){
            for (row in 1:nrow(pa.c.filt)){
              if (pa.c.filt$STATUS_YR[row] == 0){ #create a new column to identify any site that has had a year randomly allocated
                pa.c.filt$random[row]  <- T
              }
            }
            ryears <- pa.c.filt$STATUS_YR[pa.c.filt$STATUS_YR > 0] #select all years where the status year isn't 0
            if (length(ryears) == 0){ #if all status years are 0
              ryears <- pas$STATUS_YR[pas$STATUS_YR > 1986] #then use range of status years for all protected areas (not just in this country) later than 1986
            }
            if (length(ryears) == 1){ #if only one year that is not 0
              ryears <- c(ryears, ryears)
            }
            pa.c.filt$STATUS_YR[pa.c.filt$STATUS_YR == 0] <- base::sample(ryears, nrow(pa.c.filt[pa.c.filt$STATUS_YR == 0, ]), replace = T) ## selects a year randomly from the pool of possible years
          }
          ########### end of pa year randomisation
          
          for (z in 1:nrow(kba.c)){ ## starts loop for all kbas in the country (changed to nrow as was looking at columns rather than rows)
            #for (z in 1:length(kba.c)){ ## starts loop for all kbas in the country
            #z=which(kba.c$SitRecID=="19780")
            kbaz <- kba.c[z, ]
            akba <- NA #set to NA to be replaced further on
            akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbaz[,kba_geom])}, error=function(e){})))
            
            length(which(ovkba[ ,z] == T)) # find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
            
            if (length(which(ovkba[ ,z] == T)) > 0){  ### when at least 1 pa overlaps with the kba
              
              pacz <- pa.c.filt[which(ovkba[ ,z] == T), ] #subset to pas that overlap this kba
              pacz <- st_make_valid(pacz)
              
              
              if (plotit){
                plot(kbaz$geometry)
                plot(pacz$Shape, col=rgb(0,0,.8,0.2), border=0, add=T)
              }
              
              yearspacz <- pacz$STATUS_YR #years of pas in kba z
              ovf <- NULL
              
              ovf <- suppressMessages(tryCatch({st_intersection(pacz, kbaz)}, error = function(e){}) )## spatial intersection kba and all pas overlapping, results in polygon output for each overlap (in sf/dataframe)
              #TODO this line doesn't always run if there is interesting geometry within the PA layer.
              class(ovf)
              
              if ("sf" %in% class(ovf)& length(yearspacz) > 0){  
                
                ovfpol <- ovf #not needed but avoiding having to rename subsequent dataframes
                years <- sort(unique(ovfpol$STATUS_YR))
                
                year1 <- min(years)
                ovf1 <- ovfpol[ovfpol$STATUS_YR == year1, ]
                ovf11 <- NULL
                
                if(!is.null(ovf1)){
                  ovf1 <- st_make_valid(ovf1)}
                
                ovf11 <- suppressMessages(tryCatch({st_union(ovf1, by_feature = F)}, error=function(e){}))
                if(plotit) plot(ovf11, col = 2)
                
                if(!is.null(ovf11)){
                  ovf11 <- st_make_valid(ovf11)}
                
                ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf11)}, error=function(e){})))
                
                
                if (length(ovlz) == 0){ 
                  ovlz <- 0
                }
                
                random0 <- pacz$random[pacz$STATUS_YR == year1] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
                
                random1 <- FALSE #sets random to false by default - i.e. PA designation has a given year
                random1[TRUE %in% random0] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
                
                areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=ovlz, year=year1, random = random1, nPAs=nrow(ovf1)) #creates row in output table with this site overlap area and associated information within it #sets numbers to numeric not units (removes m^2)
                
                if (length(years) > 1){
                  for (w in 2:length(years)){
                    
                    rema <- 1-(sum(areasov1$ovl[!is.na(areasov1$ovl)])/akba)  ## to see if there is still any area left by the pas of year 1
                    if (rema > 0.02){ #assuming 2% error in delineation of kbas compared to pas
                      year2 <- years[w]
                      ovf2 <- ovfpol[ovfpol$STATUS_YR == year2, ]
                      ovf22 <- NULL
                      
                      ovf22 <- suppressMessages(tryCatch({st_union(ovf2, by_feature = F)}, error=function(e){}))
                      
                      if(plotit){
                        plot(ovf22, add=T, col=w+1)
                      }
                      
                      ovfprev <- ovfpol[ovfpol$STATUS_YR < year2, ]
                      
                      if(!is.null(ovfprev)){
                        ovfprev <- st_make_valid(ovfprev)}
                      
                      ovfprev3 <- suppressMessages(tryCatch({st_union(ovfprev, by_feature = F)}, error=function(e){})) #merge all polygons from previous years
                      
                      if(plotit){
                        plot(ovfprev3, add=T, col=w+2)
                      }
                      
                      ovf23 <- NULL
                      
                      if(!is.null(ovfprev3)){
                        ovfprev3 <- st_make_valid(ovfprev3)}
                      
                      if(!is.null(ovf22)){
                        ovf22 <- st_make_valid(ovf22)}
                      
                      ovf23 <- suppressMessages(tryCatch({st_difference(st_geometry(ovf22), st_geometry(ovfprev3))}, error = function(e){})) #to determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                      
                      if(plotit){
                        plot(ovf23, add=T, col=c("grey"))
                      }
                      if(!is.null(ovf23)){
                        ovf23 <- st_make_valid(ovf23)}
                      
                      if(length(ovf23) > 1){
                        ovf23 <- suppressMessages(tryCatch({st_union(ovf23)}, error = function(e){}) )
                      }
                      
                      #This removes any non-polygon elements from collapsed polygons eg multilines
                      if ("sfc_GEOMETRYCOLLECTION" %in% class(ovf23)){
                        ovf23 <- st_collection_extract(ovf23, "POLYGON", warn = F)}
                      ####
                      
                      if(length(ovf23) > 1){
                        ovf23 <- suppressMessages(tryCatch({st_union(ovf23)}, error = function(e){}) )#union didnt work so tried combine to collapse all polygons
                      }
                      
                      if(!is.null(ovf23)){
                        ovf23 <- st_make_valid(ovf23)}
                      
                      
                      ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf23)}, error = function(e){})))
                      
                      if (length(ovlz)==0){ #if there is no overlap (no new overlap by the PA?), set value to NA (should change to 0?###HAVE NOW CHANGED THIS TO 0)
                        ovlz <- 0
                      }
                      
                      random2 <- pacz$random[pacz$STATUS_YR == year2] #creates vector of whether each protected area overlap within a given year was randomly sampled or if it was true designation year
                      
                      random3 <- FALSE #sets random to false by default - i.e. PA designation has a given year
                      random3[TRUE %in% random2] <- TRUE #assign random as true if any protected areas in a given year were randomly allocated a year
                      
                      areasov1 <- rbind(areasov1,data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=ovlz, year=year2, random = random3, nPAs=nrow(ovf2)))
                      
                    }
                  }
                }
                
              }  # ends loop for    class(ovf)=="SpatialPolygons"
              
              if (is.null(ovf)){
                areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba,WDPA_type = paste(focal_pas), ovl=0, year=year1, random=F, nPAs=0)}
            }  ## ends loop for PAs overlapping with the KBA
            if (length(which(ovkba[ ,z] == T)) == 0){
              areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, WDPA_type = paste(focal_pas),ovl=0, year=0, random=F, nPAs=0)   ## if there are NO (zero/none) pas overlapping the kba
            }
            areasov <- rbind(areasov,areasov1)
          }  ## ends loop for all kbas in the country
          
          areasov$percPA <- areasov$ovl/areasov$kba
          
          areasov$ISO <- country
          
          areasov$country <- country.n
          
        } # ends loop for ovlkba>0
      }  ## ends loop for length(pac)>1
      
      areasovfinal <- rbind(areasovfinal, areasov)
      
      #As coverage of Protected Areas alone will be the same as for Protected Areas and OECMs (there are no OECMs) - this just copies the previous table and changes the WDPA type
      areasov <- mutate(areasov, WDPA_type = "Protected Areas")
      areasovfinal <- rbind(areasovfinal, areasov)
      
      
      #These countries have no OECMs so this adds 0 coverage OECM data
      areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = NA, WDPA_type = "OECMs",ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, ISO = country, country = country.n) 
      areasovfinal <- rbind(areasovfinal, areasov)
      
    }
    
    finaltab <- rbind(finaltab,areasovfinal)
    
    tname <- paste(country.n, ".csv", sep="")
    write.csv(areasovfinal, paste("files_country_",year_run,"/",tname,sep = ""), row.names=F)
    cli_alert_success(paste("Analysis for",country.n,"complete,", length(unique(finaltab$ISO)), "of", length(listcnts), "countries analysed."))
  }
  (proc.time()-tt)[1]/60 ## time in minutes
  
  finaltab <- unique(finaltab)
  
  write.csv(finaltab, paste("finaltab_", year_run, ".csv", sep=""), row.names = F)
  assign("finaltab", finaltab, envir = .GlobalEnv)
  ### end here
}

#rescale_results ----
rescale_results <- function(){
  final1 <- read.csv(paste("finaltab_", year_run, ".csv", sep=""))
  final <- final1[!is.na(final1$ovl),]
  tabf <- merge(final, tabmf, by = "SitRecID")
  tabf$percPA[tabf$percPA > 1] <- 1
  resf <- with(tabf, aggregate(percPA, list(SitRecID = SitRecID, WDPA_type = WDPA_type), sum))
  kbas2fix <- unique(resf$SitRecID[resf$x>1])
  if(nrow(resf[resf$x > 1,]) > 0){
    cli_alert_info(paste0(length(unique(resf$SitRecID[resf$x>1]))," KBAs have total coverage by PAs and OECM values of > 1. This is often due to geometry Oversimplification, rescaling these now."))
  }
  
  if (length(kbas2fix) > 0){
    tabf <- tabf %>% 
      group_by(SitRecID, WDPA_type) %>% 
      mutate(percPA = case_when(SitRecID %in% kbas2fix ~ percPA/sum(percPA),
                                TRUE ~ percPA)) %>% 
      mutate(percPA = case_when(is.na(percPA) ~ 0,
                                TRUE ~ percPA)) %>% 
      ungroup()
  }
  assign("tabf", tabf, envir = .GlobalEnv)
}

#build_inout ----
build_inout <- function(){
  inout <- as.data.frame(fields) #create a table with the region codes
  colnames(inout)[colnames(inout) == 'fields'] <- 'code' #rename fields as code
  
  for (row in 1:nrow(inout)){ #loop through to generate the required input and output file names, and assign global as a distinct region (or not)
    inout$inputfile[row] <- paste("Input data for R", inout$code[row], "KBAs", sep=" ")
    inout$global[row][inout$code[row] == 'global'] <- 1
    inout$global[row][inout$code[row] != 'global'] <- 0
    inout$outputfile1[row] <- paste("Output data for R", inout$code[row], "KBAs", sep=" ")
  }
  
  write.csv(inout, paste("in_out_files", year_run, '.csv', sep=""), row.names = F) #write in_out file to csv so that it can be used for the randomisation analyses
  assign("inout_full",inout, envir = .GlobalEnv)
}

#input_tables ----
input_tables <- function(){
  isos$ISO <- isos$ISO3
  tabff <- merge(tabf, isos, by = "ISO")
  
  for (f in 1:length(fields2use)){
    ff <- as.character(fields2use[f])
    tfname <- paste("Input data for R", ff, "KBAs.csv", sep=" ")
    if (ff == "global"){
      tab2export <- with(tabff, data.frame(siteid = SitRecID,WDPA_type = WDPA_type, region = ff, country = country, perprotected = percPA*100, year = year, random_year = random)) #removed year = year - 1 (not sure why this is in there)
    }
    if (ff != "global"){
      tab2export <- with(tabff, data.frame(siteid = SitRecID,WDPA_type = WDPA_type, region = tabff[,which(names(tabff) == ff)], country = country, perprotected = percPA*100, year = year, random_year = random))
    }
    write.csv(tab2export, paste("input tables ", year_run, "/", tfname, sep=""), row.names = F)
  }
  cli_alert_success("Input tables successfully created.")
}


#random_functions ----
random_function <- function(){
  # correct sampling function, as standard R function not correct
  resample <- function(x, size, ...){
    if(length(x) <= 1){
      if(!missing(size) && size == 0){
        x[FALSE]
      } else {
        x
      }
    } else {
      sample(x, size, ...)
    }
  }
  assign("resample",resample, envir = .GlobalEnv)
  
  # random allocation of year protected for those with missing years, asign from all if none found in country where x = year, y = country and z = %protected, zx = random_year
  ranyear <- function(x, y, z, zx) {
    out <- as.vector(c())
    for (i in 1:length(y)) {
      if(zx[i] == F) {
        out <- append(out, x[i], after = length(out)) #if year designated was not random, send the table straight to the output of the function (don't need to assign a year)
      } else {
        if(length(as.numeric(x[y == y[i] & zx != TRUE & z > 0])) > 1){
          out <- append(out, resample(as.numeric(x[y == y[i] & zx != TRUE & z > 0]), 1, replace = T), after = length(out)) #for kbas with random years, if there are more than 1 year reported, use one of these for the overlap year
        } else {
          if (glb){
            out <- append(out, resample(as.numeric(x[y == y & zx != TRUE & z > 0]), 1, replace = T), after = length(out)) #use full protected area designation year list if <2 protected area years reported in the country
          } else if (reg){
            out = append(out, resample(poolryears, 1, replace = T), after = length(out))
          }
        }
      }
    }
    return(out)
  }
  assign("ranyear",ranyear, envir = .GlobalEnv)
  
  # cummulative number of sites that are totally protected
  # where x = protected and y = year and id = siteid
  stattotp <-function(x, y, id) {
    #yrs = seq(1900, 2016, by = 1)
    out <- as.vector(c())
    for (i in 1:length(yrs)){
      temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum)
      out = append(out, length(temp[temp >= 98]), after = length(out))
    }
    return(out)
  }
  assign("stattotp",stattotp, envir = .GlobalEnv)
  
  
  # cummulative mean percentage of sites that are protected
  # where x = protected and y = year and id = siteid
  statavep <- function(x, y, id) {
    out <- as.vector(c())
    for (i in 1:length(yrs)){
      temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum) #sums % coverages within each  sites up to year i
      if (glb){
        out = append(out, sum(temp/100), after = length(out)) #sums % coverage across all sites in this country/region up to year i
      }
      if (reg){
        if (length(temp) > 0){
          out = append(out, sum(temp/100), after = length(out))
        }
        if (length(temp) == 0){
          out = append(out, 0, after = length(out))
        }
      }  
    }
    return(out)
  }
  assign("statavep",statavep, envir = .GlobalEnv)
  
  
  #randomisation
  mkdata <- function(x, y, z, xz, zz, j) {
    # where X = %protected, y=country, z=year, xz = whether year was randomly generated, zz=siteID j=number of iterations
    # results are written into an array with row, column, dimension representing samples, years from 1900-2016, and count of KBAs with 100% protection & mean area protected
    # res[,,1] will give counts of IBAs with 100% protection, and res[,,2] will give the means
    res <- matrix(data = NA, length(x), j)
    rownames (res) <- zz # write the KBA ID number to rowname
    for (i in 1:j) {
      ry = ranyear(z, y, x, xz)
      #st = stattotp (rp, ry)
      #sm = statmeanp(rp, ry)
      res[,i] <- ry
    }
    return(res)
  }
  assign("mkdata",mkdata, envir = .GlobalEnv)
  
  calcvals = function(x,y) {
    res <- matrix(data = NA, j, length(yrs))
    for (i in 1:j) {
      res[i,] <- stattotp(x, y[,i], rownames(y)) #total coverage
    }
    return(res)
  }
  assign("calcvals",calcvals, envir = .GlobalEnv)
  
  calcvals2 = function(x,y) {
    res <- matrix(data = NA, j, length(yrs))
    for (i in 1:j) {
      res[i,] <- statavep(x, y[,i], rownames(y)) #mean coverage
    }
    return(res)
  }
  assign("calcvals2",calcvals2, envir = .GlobalEnv)
}

#randomisation ----
randomisation <- function(){
  for (w in 1:nrow(inout)){ #loops through input files (using file names from inout csv file)

    infile = paste("input tables ", year_run,"/",as.character(inout$inputfile[w]), ".csv", sep = "")  #"Input data for R global Region KBAs.csv"
    
    glb = F
    reg = T
    if (inout$global[w] == 1){ #if global
      glb <- T
      reg <- F
    }
    assign("glb",glb, envir = .GlobalEnv)
    assign("reg",reg, envir = .GlobalEnv)
    

    # NOTE: Outfiles will be overwritten, including those for regions.
    ####################
    
    ibadat1 <-  read.csv(infile, header = TRUE, sep = ",", quote="\"", dec=".", comment.char="", as.is = TRUE) # read in data
    
    if (sdg_run){ #if TRUE this retains only values for the combined PA+OECM coverage, these are the only ones required for SDG reporting which saves time here
      ibadat1 <- filter(ibadat1, WDPA_type == "Protected Areas and OECMs")
    }
    
    outputfile1 <- as.character(inout$outputfile1[w])
    
    poolryears <- ibadat1$year[ibadat1$random_year == FALSE & ibadat1$year > 0] #create a list of years that were not randomly assigned, to use if a country has no random years #note this uses ibadat1 as needs to not only look at the system subset, but from the protected areas in the country as a whole
    assign("poolryears",poolryears, envir = .GlobalEnv)
    
    
    for (k in 1:length(subsets)){
      sset <- subsets[k]
      if (sset == 'all') { 
        ibadat <- ibadat1
        outfile <- paste(inout$outputfile1[w], "_", sset, ".csv", sep = "")
      }

      if (sset != 'all') { 
        kbasi <- tabmf$SitRecID[tabmf[,which(names(tabmf) == sset)] == "Y"]
        ibadat <- ibadat1[ibadat1$siteid %in% kbasi,]
        outfile <- paste(inout$outputfile1[w], "_", sset, ".csv", sep = "")
      }

      if (infile == "Input data for R ISO_SDG KBAs.csv" & file.exists(gsub("SDG", "BL", outfile))){ # if running analysis for ISO_SDG, need to check if ISO_BL already run and so output from it already exists
        bl_file_name <- gsub("SDG", "BL", outfile) #create ISO_BL output file name
        iso_bl <- read.csv(paste("input tables ", year_run,"/",bl_file_name, ".csv", sep = "")) #read in ISO_BL output file
        
        ibadat <- ibadat[ibadat$region == "GBR" | ibadat$region == "GGY" | ibadat$region == "IMN" | ibadat$region == "JEY" | ibadat$region == "CHN",] #if using ISO_SDG, only need to run ISOs that differ compared to ISO BL - i.e. Britain and it's near islands (Guernsey, Isle of Man and Jersey), and China and Taiwan
      }
      
      regs <- unique(ibadat$region)    

      regres <- data.frame()
      for (z in 1:length(regs)){
        #regs[z]
        ibadat2 <- ibadat[ibadat$region == regs[z],]

        ### Splitting here also by OECM
        patypes <- unique(ibadat2$WDPA_type)
        res3 <- data.frame()
        
        for(p in 1:length(patypes)){
          focal_pas <- patypes[p]
          
          ibadat3 <- filter(ibadat2, WDPA_type == paste(focal_pas))
          
          n <- length(ibadat3$siteid)
          ibaname <- unique(ibadat3$siteid)
          m <-  length(unique(ibadat3$siteid))
          print(paste("Number of KBAs being analysed ", m," with ", n, " sites ",focal_pas,".", sep = ""))

          ###############################
          # start data randomization
          ##############################
          
          output <- mkdata(ibadat3$perprotected, ibadat3$country, ibadat3$year, ibadat3$random_year, ibadat3$siteid, j) #creates a list of years to make the error bars from (j repetitions so in this case a list of 100 possible years) #rows are the siteIDs, columns are the repetitions
          
          # calculate the results   ## % of PAs fully covered
          alldata <- calcvals(ibadat3$perprotected, output) #creates matrix of possible values for all repetitions for all years, where columns are different years and rows are the different repetitions based on the years randomly determined above - this is now country level data
          
          res <- matrix(data = NA, ncol = 7, nrow = length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")))
          res[,1] <- yrs
          for (i in 1:length(yrs)){
            res[i,2] <- quantile(alldata[,i], probs = c(0.025))
            res[i,3] <- quantile(alldata[,i], probs = c(0.5))
            res[i,4] <- quantile(alldata[,i], probs = c(0.975))
          }
          res[,5] <- res[,2]/m *100
          res[,6] <- res[,3]/m *100
          res[,7] <- res[,4]/m *100
          
          res2 <- data.frame(res)
          names(res2) = c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")
          
          # calculate the results   ## % of KBAs covered
          alldata <- calcvals2(ibadat3$perprotected, output)
          
          res <- matrix(data = NA, ncol = 7, nrow = length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage_Area", "95CImidPercentage_Area", "95CIhiPercentage_Area")))
          res[,1] <- yrs
          for (i in 1:length(yrs)){
            res[i,2] <- quantile(alldata[,i], probs = c(0.025))
            res[i,3] <- quantile(alldata[,i], probs = c(0.5))
            res[i,4] <- quantile(alldata[,i], probs = c(0.975))
          }
          res[,5] <- res[,2]/m *100
          res[,6] <- res[,3]/m *100
          res[,7] <- res[,4]/m *100
          
          res2$CI95lowPercentage_Area <- res[,5]
          res2$CI95midPercentage_Area <- res[,6]
          res2$CI95hiPercentage_Area <- res[,7]
          
          res2$region <- regs[z]
          res2$code <- inout$code[w]
          res2$sset <- sset
          res2$WDPA_type <- focal_pas
          
          res3 <- rbind(res3,res2)
        }
        
        if (glb){
          write.csv(res3, paste("output tables ", year_run,"/", outfile, sep = ""), row.names = F)
        }
        
        if (reg){
          regres <- rbind(regres, res3)
        }
        
      } #ends loop of regs
      
      if (reg){

        if (infile == "Input data for R ISO_SDG KBAs.csv" & exists("iso_bl")){ # if running ISO_SDG data, use data from ISO_BL to save time where ISO code is the same (i.e. for most countries/territories/islands)
          
          iso_bl <- iso_bl[iso_bl$region != "GBR" & iso_bl$region != "GGY" & iso_bl$region != "IMN" & iso_bl$region != "JEY" & iso_bl$region != "CHN",] #remove isos that differ between ISO_BL and ISO_SDG
          
          regres <- rbind(iso_bl, regres) #combine tables
          
          regres$code <- "ISO_SDG" #correct code to SDG
          
          regres <- regres[order(regres$region),] #reorder alphabetically by iso
        }
        
        write.table(regres, file = paste("output tables ", year_run,"/", outfile, sep = ""), append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE)
      } 
      
    } #ends loop of subsets (marine, Freshwater, etc)
  } ## ends the loop of input files
  ##############
}


#summary_per_kba ----
summary_per_kba <- function(){
  
  ovl <- read.csv(paste("input tables ", year_run,"/Input data for R ISO_SDG KBAs", ".csv", sep = "")) #read in data to summarise
  
  ovl2 <- ovl %>% #sum across years and so determine total perc coverage of each kba by protected areas and/or OECMs
    group_by(siteid, country, region, WDPA_type) %>%
    summarise(percPA = sum(perprotected))
  
  kba_class <- merge(ovl2, tabmf, by.x = "siteid", by.y = "SitRecID", all = F)
  
  kba_class[kba_class == "Y"] <- 1
  kba_class[kba_class == "N"] <- 0
  
  colnames(kba_class) <- c("SitRecID",	"COUNTRY",	"ISO","WDPA_type",	"percPA", "terrestrial", "Freshwater", "marine", "mountain") #TODO check in right order for when renamed
  
  kba_class$`PA.Coverage`[kba_class$percPA >= 98] <- "complete"
  kba_class$`PA.Coverage`[kba_class$percPA > 2 & kba_class$percPA < 98] <- "partial"
  kba_class$`PA.Coverage`[kba_class$percPA <= 2] <- "none"
  
  write.csv(kba_class, "pa_coverage_per_kba.csv", row.names = F)
  
  assign("kba_class", kba_class, envir = .GlobalEnv)
  assign("ovl", ovl, envir = .GlobalEnv)
}
#summary_per_country ----
summary_per_country <- function(){
  
  fls <- dir(path = paste("output tables ", year_run,"/",sep = ""),pattern = "Output data for R ISO_SDG KBAs")
  
  tab2 <- data.frame()
  
  for (x in 1:length(fls)){ 
    fl1 <- fls[x]
    
    tab <- read.csv(paste("output tables ", year_run,"/",fl1,sep = ""))
    
    tab <- filter(tab, year %in% c(seq(1980,2020, by = 10), 2020:year_run))
    
    tab <- select(tab, c('region', 'CI95lowPercentage_Area', 'CI95midPercentage_Area', 'CI95hiPercentage_Area', 'sset', 'year', 'WDPA_type')) #select necessary columns
    
    colnames(tab) <- c("iso3",	"lower_confidence_interval",	"mean_perc_pa_coverage",	"upper_confidence_interval",	"kba_type",	"year",'WDPA_type')
    
    tab2 <- rbind(tab2, tab)
  }

  country_list <- unique(select(ovl, c("region", "country", "WDPA_type")))
  
  country_kba_pa <- merge(tab2, country_list, by.x = c("iso3","WDPA_type"), by.y = c("region","WDPA_type"), all = F)
  country_kba_pa <- select(country_kba_pa, c("country", "iso3",	"lower_confidence_interval",	"mean_perc_pa_coverage",	"upper_confidence_interval",	"kba_type",	"year","WDPA_type"))
  
  country_kba_pa <- country_kba_pa[with(country_kba_pa, order(country, kba_type, year,WDPA_type)),]
  
  write.csv(country_kba_pa, "percentage_coverage_by_country_data.csv", row.names = F)
  
  }


#summary_global ----
summary_global <- function(){
  
  summary2022 <- kba_class
  
  summary2022 <- summary2022 %>% 
    mutate(count = 1,
           All= 1) %>% 
    group_by(WDPA_type)
  
  realmlist <- c("All","terrestrial","Freshwater","marine","mountain")
  summary <- data.frame()
  
  for(r in 1:length(realmlist)){
    realm <- realmlist[r]
    
    dat2sum <- filter(summary2022, !!as.symbol(realm)== 1)
    
    sum <- summarise(dat2sum,
                     Total_KBAs = sum(count),
                     Mean_perc_area_coverage = mean(percPA))
    
    sum <- group_by(sum, WDPA_type)
    
    
    nvalues <- dat2sum %>% 
      count(PA.Coverage) %>% 
      mutate(perc_coverage_class = (n/sum(n))*100)
    
    ##Adding in Zeros for missing values so split works correctly 
    catch <- data.frame(WDPA_type = c("OECMs","Protected Areas", "Protected Areas and OECMs","OECMs","Protected Areas", "Protected Areas and OECMs","OECMs","Protected Areas", "Protected Areas and OECMs"),
                        PA.Coverage = c("complete", "complete","complete", "partial","partial","partial","none","none","none"))
    
    nvalues <- merge(nvalues, catch, by = c("WDPA_type","PA.Coverage"), all = T)
    
    nvalues <- nvalues %>% replace(is.na(.), 0) 
    
    nvalues <- data.frame(split(nvalues, nvalues$PA.Coverage))
    
    nvalues <- select(nvalues, c("complete.WDPA_type","complete.n","complete.perc_coverage_class","partial.n","partial.perc_coverage_class","none.n","none.perc_coverage_class"))
    
    sum <- merge(sum,nvalues, by.x = "WDPA_type", by.y = "complete.WDPA_type", all = T)
    
    
    sum <- mutate(sum,
                  Classification = realm)
    
    summary <- rbind(summary,sum)
  }
  
  finalsum <- select(summary, c("Classification","Total_KBAs","WDPA_type","Mean_perc_area_coverage","complete.n","partial.n","none.n","complete.perc_coverage_class","partial.perc_coverage_class","none.perc_coverage_class"))
  
  finalsum <- finalsum %>% 
    rename("Mean % Area Coverage" = "Mean_perc_area_coverage",
           "KBAs with complete coverage (n)" = "complete.n",
           "KBAs with partial coverage (n)" = "partial.n",
           "KBAs with no coverage (n)" = "none.n",
           "Percentage KBAs with complete coverage (%)" = "complete.perc_coverage_class",
           "Percentage KBAs with partial coverage (%)" = "partial.perc_coverage_class",
           "Percentage KBAs with no coverage (%)" = "none.perc_coverage_class"
    ) %>% 
    arrange(desc(WDPA_type))
  
  write.csv(finalsum, "percentage_coverage_by_country_data_global_summary.csv", row.names = F)
  
}

#ibat_fig6_plot ----
ibat_fig6_plot <- function(){
  
  cname <- "KBAs" #define for use in filenames later      
  
  #TODO - set pdf_run to true (T) if want to produce pdf outputs of the graphs, likewise for pngs, if you don't want to produce them, set them to false (F)
  pdf_run <- F #make PDF graphs
  png_run <- T #make PNG graphs
  
  fls <- dir(path = paste("output tables ", year_run,"/",sep = ""),pattern = "Output data for R ISO_SDG KBAs") #only selects the randomisation output files in the folder

  for (w in 1:length(fls)){ #for all data
    fl1 <- fls[w]
    
    tab <- read.csv(paste("output tables ", year_run,"/",fl1,sep = "")) #read in input data

    sset <- tab$sset[1]
    desc <- tab$code[1]

    #check and create folder for that system in which to store graphs
    if (!(file.exists(paste("final graphs ",year_run,"/", sset,"/",desc,  sep = "")))){ #if subfolder does not already exist, create it
      dir.create(file.path(paste("final graphs ",year_run,"/", sset,"/", desc, sep = "")))
    }
  
    ### Set up graphs
    
    varis <- c("percKBAcov") #removing "% sites completely covered by PAs" figures c("percKBAcov","percKBATot").
    
    for (y in 1:length(varis)){
      vari <- varis[y]
      
      if (vari == "percKBATot"){
        tab$mid <- tab$X95CImidPercentage
        tab$qn05 <- tab$X95CIlowPercentage
        tab$qn95 <- tab$X95CIhiPercentage
        nfile <- "PA coverage of "
        lbgr <- "% sites completely covered by protected areas"
      }
      
      if (vari == "percKBAcov"){
        tab$mid <- tab$CI95midPercentage_Area
        tab$qn05 <- tab$CI95lowPercentage_Area
        tab$qn95 <- tab$CI95hiPercentage_Area
        nfile <- "Mean perc area protected"
        lbgr <- "Mean Percentage Area Coverage (%)"
      }
      
      cinz <- grey(0.7)
      min(tab$qn95 - tab$qn05)
      max(tab$qn95 - tab$qn05)

      cts <- unique(tab$region[tab$region != "0"])
      
      ##### produce single PDF with graphs for all countries/regions UPDATED TO GGPLOT2
      if ((length(cts) > 1) & pdf_run){
        
        pdfallname <- paste("final graphs ",year_run,"/", sset,"/", desc, "/", nfile, cname, " all plots ", desc, "_", sset, ".pdf", sep = "")
        plot_list = list()
        
        for (i in 1:length(cts)){
          ct1 <- as.character(cts[i])
          tabi <- tab[tab$region == ct1 & tab$year >= 1980,]
          head(tabi)
          
          basy <- 0
          topy <- 100
          baseyr <- 1980
          if (max(tabi$qn95) < 20){
            topy <- 50
          }
          
          legendneed <- "legend"
          confidencebands <- "Protected Areas and OECMs"
          
          if(max(filter(tabi, WDPA_type == "OECMs")$mid)==0){
            tabi <- filter(tabi, WDPA_type != "OECMs")
            confidencebands <- "Protected Areas"
            legendneed <- "none"
          }
          
          p <- ggplot(tabi, aes(x = year, y = mid))+
            geom_area(data = . %>% filter(WDPA_type != "Protected Areas and OECMs"), alpha =0.4, aes(fill = WDPA_type), colour= NA)+
            geom_ribbon(data = . %>% filter(WDPA_type == confidencebands),aes(ymin = qn05,ymax = qn95), alpha = 0.2)+
            scale_x_continuous(limits = c(1980, (max(tab$year) + 1)), expand = c(0,0))+
            labs(x = "Year", y = "Mean Percetage Area Coverage (%)")+
            scale_y_continuous(limits = c(0,100), expand = c(0,0))+
            #scale_fill_viridis_d(option = "E", direction = 1)+
            scale_fill_manual("WDPA_type", values = c("OECMs"= "#6DCD59FF", "Protected Areas" = "#FDE725FF"))+
            theme(legend.position = "bottom",
                  text=element_text(size=20),
                  legend.title = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(colour = "black", vjust = 0),
                  axis.text.y = element_text(colour = "black"))+
            guides(fill = legendneed)
          
          plot_list[[i]] = p
        }
        
        pdf(file = pdfallname, width = 12, height = 10)
        for(y in 1:length(plot_list)){
          print(plot_list[[i]])
        }
        dev.off()
      } #end loop one pdf for all countries
      
      
      #### one graph per country
      
      if (length(cts) == 0){
        ct1 <- "global"
        tabi <- tab
        head(tabi)
        basy <- 0
        topy <- 100
        baseyr <- 1980
        if (max(tabi$qn95) < 20){
          topy <- 50
        }
        
        legendneed <- "legend"
        confidencebands <- "Protected Areas and OECMs"
        
        if(max(filter(tabi, WDPA_type == "OECMs")$mid)==0){
          tabi <- filter(tabi, WDPA_type != "OECMs")
          confidencebands <- "Protected Areas"
          legendneed <- "none"
        }
        
        if (png_run){
          png1name <- paste("final graphs ",year_run,"/", sset,"/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".png", sep = "")
          png(file <- png1name, width = 12, height = 10, units = "in", res = 600)
          
          print(ggplot(tabi, aes(x = year, y = mid))+
                  geom_area(data = . %>% filter(WDPA_type != "Protected Areas and OECMs"), alpha =0.4, aes(fill = WDPA_type), colour= NA)+
                  geom_ribbon(data = . %>% filter(WDPA_type == confidencebands),aes(ymin = qn05,ymax = qn95), alpha = 0.2)+
                  scale_x_continuous(limits = c(1980, (max(tab$year) + 1)), expand = c(0,0))+
                  labs(x = "Year", y = "Mean Percetage Area Coverage (%)")+
                  scale_y_continuous(limits = c(0,100), expand = c(0,0))+
                  #scale_fill_viridis_d(option = "E", direction = 1)+
                  scale_fill_manual("WDPA_type", values = c("OECMs"= "#6DCD59FF", "Protected Areas" = "#FDE725FF"))+
                  theme(legend.position = "bottom",
                        text=element_text(size=20),
                        legend.title = element_blank(),
                        panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        axis.text.x = element_text(colour = "black", vjust = 0),
                        axis.text.y = element_text(colour = "black"))+
                  guides(fill = legendneed))
          dev.off()
        }
        
        if (pdf_run){
          pdf1name <- paste("final graphs ",year_run,"/", sset,"/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".pdf", sep = "")
          pdf(file <- pdf1name, width = 12, height = 10)
          
          print(ggplot(tabi, aes(x = year, y = mid))+
                  geom_area(data = . %>% filter(WDPA_type != "Protected Areas and OECMs"), alpha =0.4, aes(fill = WDPA_type), colour= NA)+
                  geom_ribbon(data = . %>% filter(WDPA_type == "Protected Areas and OECMs"),aes(ymin = qn05,ymax = qn95), alpha = 0.2)+
                  scale_x_continuous(limits = c(1980, (max(tab$year) + 1)), expand = c(0,0))+
                  labs(x = "Year", y = "Mean Percetage Area Coverage (%)")+
                  scale_y_continuous(limits = c(0,100), expand = c(0,0))+
                  #scale_fill_viridis_d(option = "E", direction = 1)+
                  scale_fill_manual("WDPA_type", values = c("OECMs"= "#6DCD59FF", "Protected Areas" = "#FDE725FF"))+
                  theme(legend.position = "bottom",
                        text=element_text(size=20),
                        legend.title = element_blank(),
                        panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        axis.text.x = element_text(colour = "black", vjust = 0),
                        axis.text.y = element_text(colour = "black"))+
                  guides(fill = legendneed))
          dev.off()
        }

      }
      
      if (length(cts) > 0){
        for (i in 1:length(cts)){
          ct1 <- as.character(cts[i])
          tabi <- tab[tab$region == ct1 & tab$year >= 1980,]
          head(tabi)
          basy <- 0
          topy <- 100
          baseyr <- 1980
          if (max(tabi$qn95) < 20){
            topy <- 50
          }
          
          legendneed <- "legend"
          confidencebands <- "Protected Areas and OECMs"
          
          if(max(filter(tabi, WDPA_type == "OECMs")$mid)==0){
            tabi <- filter(tabi, WDPA_type != "OECMs")
            confidencebands <- "Protected Areas"
            legendneed <- "none"
          }
          
          if (png_run){
            png1name  <- paste("final graphs ",year_run,"/", sset, "/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".png", sep = "")
            png(file = png1name, width = 12, height = 10, units = "in", res = 600)
            
            print(ggplot(tabi, aes(x = year, y = mid))+
                    geom_area(data = . %>% filter(WDPA_type != "Protected Areas and OECMs"), alpha =0.4, aes(fill = WDPA_type), colour= NA)+
                    geom_ribbon(data = . %>% filter(WDPA_type == confidencebands),aes(ymin = qn05,ymax = qn95), alpha = 0.2)+
                    scale_x_continuous(limits = c(1980, (max(tab$year) + 1)), expand = c(0,0))+
                    labs(x = "Year", y = "Mean Percetage Area Coverage (%)")+
                    scale_y_continuous(limits = c(0,100), expand = c(0,0))+
                    #scale_fill_viridis_d(option = "E", direction = 1)+
                    scale_fill_manual("WDPA_type", values = c("OECMs"= "#6DCD59FF", "Protected Areas" = "#FDE725FF"))+
                    theme(legend.position = "bottom",
                          text=element_text(size=20),
                          legend.title = element_blank(),
                          panel.border = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"),
                          axis.text.x = element_text(colour = "black", vjust = 0),
                          axis.text.y = element_text(colour = "black"))+
                    guides(fill = legendneed))
            dev.off()
          }
          
          if (pdf_run){
            pdf1name <- paste("final graphs ",year_run,"/", sset, "/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".pdf", sep = "")
            pdf(file = pdf1name, width = 12, height = 10)
            
            print(ggplot(tabi, aes(x = year, y = mid))+
                    geom_area(data = . %>% filter(WDPA_type != "Protected Areas and OECMs"), alpha =0.4, aes(fill = WDPA_type), colour= NA)+
                    geom_ribbon(data = . %>% filter(WDPA_type == confidencebands),aes(ymin = qn05,ymax = qn95), alpha = 0.2)+
                    scale_x_continuous(limits = c(1980, (max(tab$year) + 1)), expand = c(0,0))+
                    labs(x = "Year", y = "Mean Percetage Area Coverage (%)")+
                    scale_y_continuous(limits = c(0,100), expand = c(0,0))+
                    #scale_fill_viridis_d(option = "E", direction = 1)+
                    scale_fill_manual("WDPA_type", values = c("OECMs"= "#6DCD59FF", "Protected Areas" = "#FDE725FF"))+
                    theme(legend.position = "bottom",
                          text=element_text(size=20),
                          legend.title = element_blank(),
                          panel.border = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"),
                          axis.text.x = element_text(colour = "black", vjust = 0),
                          axis.text.y = element_text(colour = "black"))+
                    guides(fill = legendneed))
            dev.off()
          }

        }
      } # ends loop one graph per country
    } # ends loop varis
  } # ends loop input files
}

#ibat_fig6_legends ----
ibat_fig6_legends <- function(){
  
  OECMsPres <- kba_class %>% 
    filter(WDPA_type == "OECMs") %>% 
    group_by(ISO) %>% 
    summarise(TotalOECM = sum(percPA)) %>% 
    mutate(Fig6Caption = case_when(TotalOECM == 0 ~ "Figure 6. Trends in mean percentage coverage of KBAs by protected areas. Grey shading shows 95% confidence intervals where relevant. No KBAs are covered by Other effective area-based conservation measures (OECMs) in this country.",
                                   TRUE ~ "Figure 6. Trends in mean percentage coverage of KBAs by protected Areas and Other effective area-based conservation measures (OECMs). Grey shading shows 95% confidence intervals where relevant. Yellow and green fill shows the respective contributions of protected areas and OECMs.")) %>% 
    select(-c(TotalOECM))
  
  readr::write_excel_csv(OECMsPres, "IBAT_fig6_captions.csv")
  
}
#ibat_fig5_dataprep ----
ibat_fig5_dataprep <- function(){
  
  kbaoverlap <- rbind(kba_class, notassessed)
  
  siteclass <- siteclass %>% 
    mutate(IbaStatus = case_when(IbaStatus == "confirmed" ~ 1,
                                 TRUE ~ 0 ),
           AzeStatus = case_when(AzeStatus == "confirmed" ~ 1,
                                 TRUE ~ 0 ),
           KbaStatus = 1)
  
  kbaoverlap <- merge(kbaoverlap, siteclass, by = "SitRecID")

  kbaoverlap <- kbaoverlap %>% 
    mutate(count = 1)
  
  #Building Categories ----
  kbaoverlapterrestrial <- kbaoverlap %>% 
    filter(terrestrial == 1) %>% 
    mutate(ecosystem = "terrestrial")
  
  kbaoverlapfresh <- kbaoverlap %>% 
    filter(Freshwater == 1) %>% 
    mutate(ecosystem = "freshwater")
  
  kbaoverlapmarine <- kbaoverlap %>% 
    filter(marine == 1) %>% 
    mutate(ecosystem = "marine")
  
  kbaoverlapmount <- kbaoverlap %>% 
    filter(mountain == 1) %>% 
    mutate(ecosystem = "mountain")
  
  kbaoverlapall <- kbaoverlap %>% 
    mutate(ecosystem = "all")
  
  kbaoverlapKBA <- kbaoverlap %>% 
    filter(KbaStatus == 1) %>% 
    mutate(sitetype = "KBA")
  
  kbaoverlapIBA <- kbaoverlap %>% 
    filter(IbaStatus == 1) %>% 
    mutate(sitetype = "IBA")
  
  kbaoverlapAZE<- kbaoverlap %>% 
    filter(AzeStatus == 1) %>% 
    mutate(sitetype = "AZE")
  
  
  kbaoverlapoutput <- rbind(kbaoverlapall,kbaoverlapfresh,kbaoverlapmarine,kbaoverlapmount,kbaoverlapterrestrial)
  
  kbaoverlapsitetype <- rbind(kbaoverlapAZE,kbaoverlapIBA,kbaoverlapKBA)
  
  kbaoverlapsitetype <- select(kbaoverlapsitetype, c("SitRecID","sitetype"))
  
  kbaoverlapfinal <- merge(kbaoverlapoutput, kbaoverlapsitetype, by = "SitRecID")
  
  assign("ibat_table7_input", kbaoverlapfinal, envir = .GlobalEnv)
  
  kbaoverlapfinal <- distinct(kbaoverlapfinal, .keep_all = T)
  
  ##Summarising
  kbaoverlapfinalgroups <- group_by(kbaoverlapfinal, ISO, sitetype, ecosystem) %>% 
    summarise(total = sum(count)/3)
  
  kbaoverlapfinal <- kbaoverlapfinal %>% 
    group_by(ISO, ecosystem, sitetype,PA.Coverage,WDPA_type) %>% 
    summarise(n = n())
  
  fig5output <- merge(kbaoverlapfinal, kbaoverlapfinalgroups, by = c("ISO", "ecosystem","sitetype"), all.x = T)
  
  fig5output <- mutate(fig5output, cov_percent = (n/total)*100) %>% 
    rename(iso3 = ISO,
           coverage = PA.Coverage)
  
  write.csv(fig5output, "IBATFig5Input.csv", row.names = F)
  assign("fig5output", fig5output, envir = .GlobalEnv)
  assign("kbaoverlap",kbaoverlap, envir = .GlobalEnv)
  
}
#ibat_fig5_plot ----
ibat_fig5_plot <- function(){
  
  #Renaming values for presentation
  fig5output[fig5output$coverage=="complete",]$coverage <- "Complete"
  fig5output[fig5output$coverage=="partial",]$coverage <- "Partial"
  fig5output[fig5output$coverage=="none",]$coverage <- "None"
  fig5output[fig5output$coverage=="not assessed",]$coverage <- "Not assessed"
  
  fig5output[fig5output$ecosystem=="all",]$ecosystem <- "All"
  fig5output[fig5output$ecosystem=="marine",]$ecosystem <- "Marine"
  fig5output[fig5output$ecosystem=="terrestrial",]$ecosystem <- "Terrestrial"
  fig5output[fig5output$ecosystem=="freshwater",]$ecosystem <- "Freshwater"
  fig5output[fig5output$ecosystem=="mountain",]$ecosystem <- "Mountain"
  
  fig5output[fig5output$WDPA_type=="Protected Areas and OECMs",]$WDPA_type <- "Protected Areas and OECMs Combined"
  
  # Filter out High Seas, and Select fields
  fig5output <- fig5output %>% 
    filter(iso3 != "ABNJ") %>%  
    select(iso3, sitetype, ecosystem, n, coverage, cov_percent, WDPA_type)
  
  ## Create list of countries with OECMs so that a figure with the correct number of panels is produced.
  listOECMcnts <- filter(fig5output, WDPA_type == "OECMs" & coverage %in% c("Partial","Complete"))
  
  listOECMcnts <- filter(kba_class, WDPA_type == "OECMs" & percPA > 0)
  
  listOECMcnts <- as.vector(unique(listOECMcnts$ISO))
  
  # All KBAs LOOP ####
  
  for (cty in unique(fig5output$iso3)){
    
    # Subset the data
    
    tmp <- ""
    tmp <- filter(fig5output, iso3 == cty & sitetype == "KBA") # filter to a specific country, and rows corresponding to all kbas
    tmp <- tmp[tmp$cov_percent != 0, ] # remove rows where the percent coverage is 0
    
    # Add blank rows where there is no data for an ecosystem (so as to not throw off the bar widths)
    if(nrow(tmp[tmp$ecosystem=="Terrestrial",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "KBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Terrestrial"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Marine",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "KBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Marine"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Freshwater",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "KBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Freshwater"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Mountain",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "KBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Mountain"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(!unique(tmp$iso3) %in% listOECMcnts){
      tmp <- filter(tmp, WDPA_type == "Protected Areas")
      print <- "small"
    }
    
    if(unique(tmp$iso3) %in% listOECMcnts){
      print <- "large"
    }
    
    # Set the factor levels
    tmp$ecosystem <- factor(tmp$ecosystem, levels = c("All", "Terrestrial", "Marine", "Freshwater", "Mountain"))
    tmp$coverage <- factor(tmp$coverage, levels = c("Complete", "Partial", "None", "Not assessed"))
    tmp$WDPA_type <- factor(tmp$WDPA_type, levels = c("Protected Areas and OECMs Combined", "Protected Areas", "OECMs"))
    
    # Figure out the widths to use
    all <- 0
    ter <- 0
    mar <- 0
    fw <- 0
    mt <- 0
    mx <- 0
    w <- 1
    
    all <- nrow(tmp[tmp$ecosystem=="All",])
    ter <- nrow(tmp[tmp$ecosystem=="Terrestrial",])
    mar <- nrow(tmp[tmp$ecosystem=="Marine",])
    fw <- nrow(tmp[tmp$ecosystem=="Freshwater",])
    mt <- nrow(tmp[tmp$ecosystem=="Mountain",])
    mx <- max(all, ter, mar, fw, mt)
    
    if(mx == 1){w <- 0.25}
    if(mx == 2){w <- 0.5}
    if(mx == 3){w <- 0.75}
    if(mx == 4){w <- 1}
    if(mx == 5){w <- 0.75}
    if(mx == 6){w <- 0.75}
    
    
    # Create plot
    
    x <- ggplot(tmp, aes(x = ecosystem, y = cov_percent, fill = coverage))+
      geom_bar(stat ="identity",position = position_dodge2(preserve = "single"),width = 0.8 * w)+
      facet_rep_wrap(~WDPA_type, ncol = 1, scales = 'free')+
      labs(y = "% KBAs Covered")+
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black")) +
      scale_y_continuous(limits = c(0,100),
                         breaks = c(0, 20, 40, 60, 80, 100),
                         labels = function(x) paste0(x,"%"),
                         expand = c(0,0)) +
      scale_fill_manual("coverage",
                        values=c("Complete" = "#669900", "Partial" = "#9BBB59", "None" = "#D7E4BD", "Not assessed" = "#7F7F7F"),drop = FALSE)
    # Export plot
    
    filename = paste0(cty,"_kba.png")
    
    if(print == "small"){
      ggsave(filename, plot=x, device= "png", width = 7, height = 3.5, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/all/"))
    }
    
    if(print == "large"){
      ggsave(filename, plot=x, device= "png", width = 7, height = 9, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/all/"))
    }
    
    # Saving 7 x 3.5 in image (plot x to test size)
    
  }
  
  
  # IBA LOOP #### 
  
  for (cty in unique(fig5output$iso3)){
    
    # Subset the data
    
    tmp <- ""
    tmp <- filter(fig5output, iso3 == cty & sitetype == "IBA") # filter to a specific country, and rows corresponding to all ibas
    tmp <- tmp[tmp$cov_percent != 0, ] # remove rows where the percent coverage is 0
    
    # Add blank rows where there is no data for an ecosystem (so as to not throw off the bar widths)
    if(nrow(tmp[tmp$ecosystem=="Terrestrial",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "IBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Terrestrial"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Marine",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "IBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Marine"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Freshwater",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "IBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Freshwater"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(nrow(tmp[tmp$ecosystem=="Mountain",])==0){
      tmp[nrow(tmp)+3,] <- NA
      tmp$iso3[is.na(tmp$iso3)] <- cty
      tmp$sitetype[is.na(tmp$sitetype)] <- "IBA"
      tmp$ecosystem[is.na(tmp$ecosystem)] <- "Mountain"
      #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
      tmp$n[is.na(tmp$n)] <- 0
      #tmp[max(nrow(tmp)),]$n_minus_na <- 0
      tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
      tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
      tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
    }
    
    if(!unique(tmp$iso3) %in% listOECMcnts){
      tmp <- filter(tmp, WDPA_type == "Protected Areas")
      print <- "small"
    }
    
    if(unique(tmp$iso3) %in% listOECMcnts){
      print <- "large"
    }
    
    # Set the factor levels
    tmp$ecosystem <- factor(tmp$ecosystem, levels = c("All", "Terrestrial", "Marine", "Freshwater", "Mountain"))
    tmp$coverage <- factor(tmp$coverage, levels = c("Complete", "Partial", "None", "Not assessed"))
    tmp$WDPA_type <- factor(tmp$WDPA_type, levels = c("Protected Areas and OECMs Combined", "Protected Areas", "OECMs"))
    
    # Figure out the widths to use
    all <- 0
    ter <- 0
    mar <- 0
    fw <- 0
    mt <- 0
    mx <- 0
    w <- 1
    
    all <- nrow(tmp[tmp$ecosystem=="All",])
    ter <- nrow(tmp[tmp$ecosystem=="Terrestrial",])
    mar <- nrow(tmp[tmp$ecosystem=="Marine",])
    fw <- nrow(tmp[tmp$ecosystem=="Freshwater",])
    mt <- nrow(tmp[tmp$ecosystem=="Mountain",])
    mx <- max(all, ter, mar, fw, mt)
    
    if(mx == 1){w <- 0.25}
    if(mx == 2){w <- 0.5}
    if(mx == 3){w <- 0.75}
    if(mx == 4){w <- 1}
    if(mx == 5){w <- 0.75}
    
    
    # Create plot
    
    x <- ggplot(tmp, aes(x = ecosystem, y = cov_percent, fill = coverage))+
      geom_bar(stat ="identity",position = position_dodge2(preserve = "single"),width = 0.8 * w)+
      facet_rep_wrap(~WDPA_type, ncol = 1, scales = 'free')+
      labs(y = "% KBAs Covered")+
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black")) +
      scale_y_continuous(limits = c(0,100),
                         breaks = c(0, 20, 40, 60, 80, 100),
                         labels = function(x) paste0(x,"%"),
                         expand = c(0,0)) +
      scale_fill_manual("coverage",
                        values=c("Complete" = "#669900", "Partial" = "#9BBB59", "None" = "#D7E4BD", "Not assessed" = "#7F7F7F"),drop = FALSE)
    
    # Export plot
    
    filename = paste0(cty,"_iba.png")
    
    if(print == "small"){
      ggsave(filename, plot=x, device= "png", width = 7, height = 3.5, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/iba/"))
    }
    
    if(print == "large"){
      ggsave(filename, plot=x, device= "png", width = 7, height = 9, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/iba/"))
    }
    
  }
  
  
  # AZE LOOP #### 
  
  for (cty in unique(fig5output$iso3)){
    # Subset the data
    
    tmp <- ""
    tmp <- filter(fig5output, iso3 == cty & sitetype == "AZE") # filter to a specific country, and rows corresponding to all kbas
    tmp <- tmp[tmp$cov_percent != 0, ] # remove rows where the percent coverage is 0
    
    # Create plot
    if (nrow(tmp) > 0){
      # Add blank rows where there is no data for an ecosystem (so as to not throw off the bar widths)
      if(nrow(tmp[tmp$ecosystem=="Terrestrial",])==0){
        tmp[nrow(tmp)+3,] <- NA
        tmp$iso3[is.na(tmp$iso3)] <- cty
        tmp$sitetype[is.na(tmp$sitetype)] <- "AZE"
        tmp$ecosystem[is.na(tmp$ecosystem)] <- "Terrestrial"
        #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
        tmp$n[is.na(tmp$n)] <- 0
        #tmp[max(nrow(tmp)),]$n_minus_na <- 0
        tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
        tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
        tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
      }
      
      if(nrow(tmp[tmp$ecosystem=="Marine",])==0){
        tmp[nrow(tmp)+3,] <- NA
        tmp$iso3[is.na(tmp$iso3)] <- cty
        tmp$sitetype[is.na(tmp$sitetype)] <- "AZE"
        tmp$ecosystem[is.na(tmp$ecosystem)] <- "Marine"
        #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
        tmp$n[is.na(tmp$n)] <- 0
        #tmp[max(nrow(tmp)),]$n_minus_na <- 0
        tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
        tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
        tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
      }
      
      if(nrow(tmp[tmp$ecosystem=="Freshwater",])==0){
        tmp[nrow(tmp)+3,] <- NA
        tmp$iso3[is.na(tmp$iso3)] <- cty
        tmp$sitetype[is.na(tmp$sitetype)] <- "AZE"
        tmp$ecosystem[is.na(tmp$ecosystem)] <- "Freshwater"
        #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
        tmp$n[is.na(tmp$n)] <- 0
        #tmp[max(nrow(tmp)),]$n_minus_na <- 0
        tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
        tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
        tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
      }
      
      if(nrow(tmp[tmp$ecosystem=="Mountain",])==0){
        tmp[nrow(tmp)+3,] <- NA
        tmp$iso3[is.na(tmp$iso3)] <- cty
        tmp$sitetype[is.na(tmp$sitetype)] <- "AZE"
        tmp$ecosystem[is.na(tmp$ecosystem)] <- "Mountain"
        #tmp[max(nrow(tmp)),]$label <- "Freshwater n = 0"
        tmp$n[is.na(tmp$n)] <- 0
        #tmp[max(nrow(tmp)),]$n_minus_na <- 0
        tmp$coverage[is.na(tmp$coverage)] <- tmp[1,]$coverage
        tmp$cov_percent[is.na(tmp$cov_percent)] <- 0
        tmp$WDPA_type[is.na(tmp$WDPA_type)] <- c("OECMs","Protected Areas","Protected Areas and OECMs Combined")
      }
      
      if(!unique(tmp$iso3) %in% listOECMcnts){
        tmp <- filter(tmp, WDPA_type == "Protected Areas")
        print <- "small"
      }
      
      if(unique(tmp$iso3) %in% listOECMcnts){
        print <- "large"
      }
      
      # Set the factor levels
      tmp$ecosystem <- factor(tmp$ecosystem, levels = c("All", "Terrestrial", "Marine", "Freshwater", "Mountain"))
      tmp$coverage <- factor(tmp$coverage, levels = c("Complete", "Partial", "None", "Not assessed"))
      tmp$WDPA_type <- factor(tmp$WDPA_type, levels = c("Protected Areas and OECMs Combined", "Protected Areas", "OECMs"))
      
      # Figure out the widths to use
      all <- 0
      ter <- 0
      mar <- 0
      fw <- 0
      mt <- 0
      mx <- 0
      w <- 1
      
      all <- nrow(tmp[tmp$ecosystem=="All",])
      ter <- nrow(tmp[tmp$ecosystem=="Terrestrial",])
      mar <- nrow(tmp[tmp$ecosystem=="Marine",])
      fw <- nrow(tmp[tmp$ecosystem=="Freshwater",])
      mt <- nrow(tmp[tmp$ecosystem=="Mountain",])
      mx <- max(all, ter, mar, fw, mt)
      
      if(mx == 1){w <- 0.25}
      if(mx == 2){w <- 0.5}
      if(mx == 3){w <- 0.75}
      if(mx == 4){w <- 1}
      if(mx == 5){w <- 0.75}
      
      
      # Create plot
      
      x <- ggplot(tmp, aes(x = ecosystem, y = cov_percent, fill = coverage))+
        geom_bar(stat ="identity",position = position_dodge2(preserve = "single"),width = 0.8 * w)+
        facet_rep_wrap(~WDPA_type, ncol = 1, scales = 'free')+
        labs(y = "% KBAs Covered")+
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.title.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(colour = "black"),
              axis.text.y = element_text(colour = "black")) +
        scale_y_continuous(limits = c(0,100),
                           breaks = c(0, 20, 40, 60, 80, 100),
                           labels = function(x) paste0(x,"%"),
                           expand = c(0,0)) +
        scale_fill_manual("coverage",
                          values=c("Complete" = "#669900", "Partial" = "#9BBB59", "None" = "#D7E4BD", "Not assessed" = "#7F7F7F"),drop = FALSE)
      
      # Export plot
      
      filename = paste0(cty,"_aze.png")
      
      if(print == "small"){
        ggsave(filename, plot=x, device= "png", width = 7, height = 3.5, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/aze/"))
      }
      
      if(print == "large"){
        ggsave(filename, plot=x, device= "png", width = 7, height = 9, dpi = 150, units = "in", path = paste("IBATFig5 ", year_run, sep = "", "/aze/"))
      }
    }
  }
}
#ibat_fig5_legends ----
ibat_fig5_legends <- function(){
  
  siteclass <- siteclass %>% 
    mutate(IbaStatus = case_when(IbaStatus == "confirmed" ~ 1,
                                 TRUE ~ 0 ),
           AzeStatus = case_when(AzeStatus == "confirmed" ~ 1,
                                 TRUE ~ 0 ),
           KbaStatus = 1)
  
  KBAISO <- select(kba_class,c("SitRecID","ISO"))
  
  FIG5Cap <- merge(KBAISO,siteclass, by = "SitRecID")

  FIG5Cap <- distinct(FIG5Cap, SitRecID, .keep_all = T)
  
  FIG5Cap <- FIG5Cap %>% 
    group_by(ISO) %>% 
    summarise(n_kba = sum(KbaStatus),
              n_iba = sum(IbaStatus),
              n_aze = sum(AzeStatus))
  
  OECMsPres <- kbaoverlap %>% 
    filter(WDPA_type == "OECMs") %>% 
    group_by(ISO) %>% 
    summarise(TotalOECM = sum(percPA)) %>% 
    mutate(MainText = case_when(TotalOECM == 0 ~ " by protected areas. Each KBA is classified according to its coverage by protected areas: complete (≥98%), partial (2%< and <98%), none (≤2%), and Not assessed (owing to lack of spatial data). No KBAs are covered by Other effective area-based conservation measures (OECMs) in this country.",
                                TRUE ~ " by protected areas, Other effective area-based conservation measures (OECMs), and the combination of these. Each KBA is classified according to its coverage by protected areas/OECMs: complete (≥98%), partial (2%< and <98%), none (≤2%), and Not assessed (owing to lack of spatial data)."))
  
  FIG5Cap <- merge(FIG5Cap, OECMsPres, by = "ISO")
  
  listiso <- as.character(unique(FIG5Cap$ISO))
  
  for(x in 1:length(listiso)){
    k = as.character(FIG5Cap$n_kba) 
    i = as.character(FIG5Cap$n_iba)  
    a = as.character(FIG5Cap$n_aze)
    MainText = as.character(FIG5Cap$MainText)
    
    FIG5Cap$caption <- paste("Figure 5. Coverage of all KBAs (N=",k,"), IBAs (N=",i,") and AZEs (N=",a,")",MainText, sep = "")
  }
  
  FIG5Cap <- select(FIG5Cap, -c("TotalOECM","MainText"))
  
  readr::write_excel_csv(FIG5Cap, "IBAT_fig5_captions.csv")
  
}
#ibat_table7 ----
ibat_table7 <- function(){
  
  ibat_table7_input <- ibat_table7_input %>% 
    mutate(count = 1)
  
  #Summarising
  ibat_table7_input <- ibat_table7_input %>% 
    group_by(ISO,sitetype)
  
  ibat_table7_input <- ibat_table7_input[!duplicated(ibat_table7_input[ , c("SitRecID", "sitetype")]), ]
  
  
  ibat_table7_input_final <- ibat_table7_input %>% 
    mutate(terrestrial = as.numeric(terrestrial),
           Freshwater = as.numeric(Freshwater),
           marine = as.numeric(marine),
           mountain = as.numeric(mountain)) %>% 
    summarise(Terrestrial = sum(terrestrial),
              Marine = sum(marine),
              Freshwater = sum(Freshwater),
              Mountain = sum(mountain),
              Total = sum(KbaStatus))
  
  readr::write_excel_csv(ibat_table7_input_final, "table7_national_kba_totals.csv")
  
}
#sdg_format_1 ----
sdg_format_1 <- function(){
  
  fls <- dir(path = paste("output tables ", year_run,"/",sep = ""),pattern = "Output data for")
  
  fls2 <- fls[grepl("SDG|SDG_Region|SDG_Subregion|Developed_Developing|global|LDC|LLDC_SIDS|Europe_NorthernAmerica_Australia_NewZealand", fls)] #TODO add needed regional groups to this
  #subset to only SDG output files
  
  ## for SDG files needed for reporting:
  fin3 <- data.frame()
  for (w in 1:length(fls2)){ #length(fls)
    fl1 <- fls2[w]
    
    tab <- read.csv(paste("output tables ", year_run,"/",fl1, sep = ""))    
    
    tab <- filter(tab, WDPA_type == "Protected Areas and OECMs")
    
    ## remove latin america and sub-saharan africa groups if in subregions (to avoid duplication) - need to update in main script
    if (grepl("SDG_Subregion", fl1)){
      tab <- tab[tab$region != "Latin America and the Caribbean (MDG=M49)" & tab$region != "Sub-Saharan Africa (M49)",]
    }
    

    if (!"region" %in% names(tab)){
      tab$region="Global"
    }
    
    tab <- tab[tab$region != "0",]
    tab2 <- tab[c("year","X95CIlowCount","X95CImidCount","X95CIhiCount","X95CIlowPercentage","X95CImidPercentage","X95CIhiPercentage","CI95lowPercentage_Area","CI95midPercentage_Area","CI95hiPercentage_Area","region","sset")]
    tab2$file1 <- fl1
    fin3 <- rbind(fin3,tab2)
    w <- w + 1
  }
  
  length(unique(fin3$file1)) # should be the number of output files to merge
  
  subsets <- unique(fin3$sset) 
  
  for (g in 1:length(subsets)){
    sset <- subsets[g]
    
    nffile <- paste("SDG PA coverage ", sset, " KBAs by country region ", year_run, ".csv", sep="")
    fin4 <- fin3[fin3$sset == sset,]
    fin4 <- fin4[,1:11]
    names(fin4) <- c("year","Count of KBAs completely covered by PAs and (where available) OECMs (lower CI)","Count of KBAs completely covered by PAs and (where available) OECMs (estimate)","Count of KBAs completely covered by PAs and (where available) OECMs (upper CI)","% KBAs completely covered by PAs and (where available) OECMs (lower CI)","% KBAs completely covered by PAs and (where available) OECMs (value)","% KBAs completely covered by PAs and (where available) OECMs (upper CI)","Mean % area of each KBA covered by PAs and (where available) OECMs (lower CI)","Mean % area of each KBA covered by PAs and (where available) OECMs (value)","Mean % area of each KBA covered by PAs and (where available) OECMs (upper CI)","Global/region/country")
    write.csv(fin4, paste("output tables ", year_run,"/",nffile, sep = ""), row.names = F)
  }
}
#fix_reg_grp ----
fix_reg_grp <- function(){
  
  reg_grp <- reg_grp[reg_grp$`Region Name` != "Developed regions (MDG)",]
  colnames(reg_grp)[colnames(reg_grp) == "Country (or Area) Name"] <- "Country Name"
  Encoding(reg_grp$`Country Name`)  <- "latin-8"
  Encoding(reg_grp$`Region Name`) <- "latin-8"
  assign("reg_grp",reg_grp,envir = .GlobalEnv)
  
}

#sdg_format_2 ----
sdg_format_2 <- function(){
  
  isos <- select(isos, c('countryname', 'ISO_SDG'))
  fls <- dir(path = paste("output tables ", year_run,"/",sep = ""),pattern = "SDG PA coverage") #to read in the tables from the Output folder relevant SDG coverage
  fls <- fls[!grepl("all", fls)] #remove "all"

  #read in all dataset and get unique list of regions, so that any regions without data can be added in with 'N' values to indicate no data
  all_regions <- read.csv(paste("output tables ", year_run,"/","SDG PA coverage all KBAs by country region ",year_run,".csv",sep = ""), check.names = F)
  Encoding(all_regions$`Global/region/country`) <- "latin8"
  region_list <- unique(all_regions$`Global/region/country`)
  region_list <- region_list[region_list != "Developed regions (MDG)"]
  
  for (x in 1:length(fls)){ 
    fl1 <- fls[x]
    tab <- read.csv(paste("output tables ", year_run,"/",fl1,sep = ""), check.names = F)
    
    tab <- tab[tab$year >= 2000,] #subset to years greater than 2000
    
    tab <- select(tab, c('Global/region/country', 'year', 'Mean % area of each KBA covered by PAs and (where available) OECMs (value)', 'Mean % area of each KBA covered by PAs and (where available) OECMs (upper CI)', 'Mean % area of each KBA covered by PAs and (where available) OECMs (lower CI)')) #select necessary columns
    
    Encoding(tab$`Global/region/country`) <- "latin8"
    
    tab <- unique(tab)
    
    missing_regions <- as.character(region_list[!(region_list %in% tab$`Global/region/country`)]) #get list of regions missing for the regional group in the loop
    missing_regions <- as.data.frame(missing_regions) #convert to dataframe
    missing_regions <- merge(missing_regions, 2000:year_run) #add in records from 2000 to 2021
    missing_regions <- cbind(missing_regions, "N", "N", "N") #add in 'N' where no data
    colnames(missing_regions) <- colnames(tab) #set column names to match
    
    tab <- rbind(tab, missing_regions) #add to main dataset the missing regions and 'N' for values
    
    colnames(tab) <- c('GeoAreaName', 'TimePeriod', 'Value', 'UpperBound', 'LowerBound') #change column names to match SDG reporting
    
    tab$GeoAreaName[tab$GeoAreaName == 'Global' | tab$GeoAreaName == 'global'] <- 'World' #replace name global with world to fit report style
    
    #add in necessary columns
    tab$TimeDetail <- tab$TimePeriod
    tab$Nature <- 'C'
    tab$Units <- 'PERCENT'
    tab$`Reporting Type` <- 'G'
    tab$Source <- paste('BirdLife International, IUCN and UNEP-WCMC (', (max(tab$TimePeriod) + 1), '). Based on spatial overlap between polygons for Key Biodiversity Areas from the World Database of Key Biodiveristy Areas (www.keybiodiversityareas.org) and polygons for protected areas from the World Database on Protected Areas and (where available) for Other Effective area-based Conservation Measures and from the World Database on OECMs (www.protectedplanet.net)', sep = "")
    tab$FootNote <- ''
    tab$FootNote[tab$Value == "N"] <- "Non-relevant"
    
    reg_grp <- select(reg_grp, c('M49 Code', 'ISO Code', 'Country Name', 'M49 Code(region)', 'Region Name', 'Reference Area Type [i.e. global, SDG groupings, MDG groupings, etc.]')) #select columns
    
    reg_grpC <- select(reg_grp, c('M49 Code', 'ISO Code', 'Country Name'))
    
    colnames(reg_grpC) <- c('GeoAreaCode', 'ISO Code', 'Country Name')
    
    reg_grpC$RefAreaType_InternalUseOnly <- '3.0-Country' #add country ref
    
    reg_grpR <- select(reg_grp, c('M49 Code(region)', 'Region Name', 'Reference Area Type [i.e. global, SDG groupings, MDG groupings, etc.]'))
    
    colnames(reg_grpR) <- c('GeoAreaCode2', 'Region Name', 'RefAreaType_InternalUseOnly2') #rename columns
    reg_grpR <- unique(reg_grpR)
    
    tab2 <- merge(tab, reg_grpC, by.x = 'GeoAreaName', by.y = 'ISO Code', all.x = T) #combine SDG reporting info with KBA-PA overlap output  - for countries
    Encoding(tab2$GeoAreaName) <- "latin-8"
    tab2 <- unique(tab2)
    
    for (row in 1:nrow(tab2)){ #replace ISO code with country name (for countries i.e. when country name is not NA)
      if(!is.na(tab2$`Country Name`[row])){
        tab2$GeoAreaName[row] <- tab2$`Country Name`[row]
      }
    }
    
    tab3 <- merge(tab2, reg_grpR, by.x = 'GeoAreaName', by.y = 'Region Name', all=T) #combine output with regional reporting information
    
    for (row in 1:nrow(tab3)){ #fill in area codes and ref type based on regional groupings
      if (is.na(tab3$RefAreaType_InternalUseOnly[row])){
        tab3$GeoAreaCode[row] <- tab3$GeoAreaCode2[row]
        tab3$RefAreaType_InternalUseOnly[row] <- tab3$RefAreaType_InternalUseOnly2[row]
      }
    }
    
    tab3 <- tab3[!is.na(tab3$RefAreaType_InternalUseOnly), ] #filter out any sites that didn't get ref codes and so shouldn't be in the final output
    
    tab3 <- mutate(tab3, Type = case_when(RefAreaType_InternalUseOnly == "3.0-Country" ~ "Country",
                                          TRUE ~ "Region"))
    
    tab <- tab3
    
    if (grepl('all', fl1)){
      tab$Indicator <- 'All KBAs - For Info'
      tab$SeriesID <- 'All KBAs - For Info'
      tab$SeriesCode <- 'All KBAs - For Info'
      tab$SeriesDescription <- 'Mean proportion of freshwater Key Biodiversity Areas (KBAs) covered by protected areas and (where available) OECMs (%)'
      tab$"Observation Status" <-  'A'}
    
    
    if (grepl('Freshwater', fl1)){
      tab$Indicator <- '15.1.2'
      tab$SeriesID <- '2837'
      tab$SeriesCode <- 'ER_PTD_FRHWRT'
      tab$SeriesDescription <- 'Mean proportion of freshwater Key Biodiversity Areas (KBAs) covered by protected areas and (where available) OECMs (%)'
      tab$"Observation Status" <-  'A'}
    
    if (grepl('marine', fl1)){
      tab$Indicator <- '14.5.1'
      tab$SeriesID <- '2999'
      tab$SeriesCode <- 'ER_MRN_MPA'
      tab$SeriesDescription <- 'Mean proportion of marine Key Biodiversity Areas (KBAs) covered by protected areas and (where available) OECMs (%)'
      tab$"Observation Status" <-  'A'}
    
    if (grepl('terrestrial', fl1)){
      tab$Indicator <- '15.1.2'
      tab$SeriesID <- '2839'
      tab$SeriesCode <- 'ER_PTD_TERR'
      tab$SeriesDescription <- 'Mean proportion of terrestrial Key Biodiversity Areas (KBAs) covered by protected areas and (where available) OECMs (%)'
      tab$"Observation Status" <-  'A'}
    
    if (grepl('mountain', fl1)){
      tab$Indicator <- '15.4.1'
      tab$SeriesID <- '2838'
      tab$SeriesCode <- 'ER_PTD_MTN'
      tab$SeriesDescription <- 'Mean proportion of mountain Key Biodiversity Areas (KBAs) covered by protected areas and (where available) OECMs (%)'
      tab$"Observation Status" <-  'A'}
    
    tab <- select(tab, c('Indicator', 'SeriesID', 'SeriesDescription', 'GeoAreaCode', 'GeoAreaName', 'TimePeriod', 'Value', 'TimeDetail', 'UpperBound', 'LowerBound', 'Source', 'FootNote', 'Nature', 'Units', 'SeriesCode','Observation Status','Type', 'Reporting Type','RefAreaType_InternalUseOnly'))
    
    tab <- merge(tab, isos, by.x = "GeoAreaName", by.y = "countryname", all.x = T) %>% 
      rename(ISOalpha3 = ISO_SDG)
    
    colnames(tab)[colnames(tab) == 'TimeDetail'] <- 'Time_Detail'
    
    tab <- tab[
      order(tab[, "RefAreaType_InternalUseOnly"], tab[, "GeoAreaName"], tab[, "TimePeriod"]),
    ]
    
    tab <- select(tab, -c("RefAreaType_InternalUseOnly"))
    
    tab <- tab[,c("Indicator",	"SeriesID",	"SeriesDescription",	"GeoAreaCode",	"GeoAreaName",	"TimePeriod",	"Value","Time_Detail",	"UpperBound",	"LowerBound",	"Source",	"FootNote",	"Nature"	,"Units",	"Reporting Type",	"Observation Status",	"ISOalpha3",	"Type",	"SeriesCode")]
    
    if (grepl('Freshwater', fl1)){
      write.csv(tab, paste("output tables ", year_run,"/",'119-15.1.2-2837-ER_PTD_FRHWRT-5654.csv', sep = ""), row.names = F,fileEncoding = 'UTF-8')
    }
    
    if (grepl('marine', fl1)){
      write.csv(tab, paste("output tables ", year_run,"/",'119-14.5.1-2999-ER_MRN_MPA-4532.csv', sep = ""), row.names = F,fileEncoding = 'UTF-8')
    }
    
    if (grepl('terrestrial', fl1)){
      write.csv(tab, paste("output tables ", year_run,"/",'119-15.1.2-2839-ER_PTD_TERR-5654.csv', sep = ""), row.names = F,fileEncoding = 'UTF-8')
    }
    
    if (grepl('mountain', fl1)){
      write.csv(tab, paste("output tables ", year_run,"/",'119-15.4.1-2838-ER_PTD_MTN-5654.csv', sep = ""), row.names = F,fileEncoding = 'UTF-8')
    }
  }
}
