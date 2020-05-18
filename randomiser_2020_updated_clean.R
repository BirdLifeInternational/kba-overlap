# IBA randomizer 2.0
# JPW Scharlemann 14/04/2014 jorn.scharlemann@unep-wcmc.org, updated by Ash Simkins March 2020
# Script to randomly allocate year of protection and percentage protected to IBAs with missing data
# Missing years and percentage protection are allocated based on the PAs with known information from that country, or if the country has two or fewer IBAs that are protected information will be allocated at random from all IBAs
# data preparation:
#
####################
# Prepare a datafile
# extract from Excel spreadsheet and save as .csv file
# ibaname (or IBA ID)
# country
# region
# perprotected = proportion protected of IBA (i.e. percentage protected / 100), 0 if missing information
# year = year of protection, random_year coded as true if this overlap contains protected area overlap with unknown years (and so assigned randomly)
####################


#### Part 1 - Read in input files and define functions #################
#TODO update wd to where you stored the Input data for R... files
year_run <- format(Sys.Date(), "%Y")

setwd (paste("C:/Users/Ashley.Simkins/OneDrive - BirdLife International/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables", year_run))
#workd <- paste("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables", year_run)

inout <- read.csv(paste("../in_out_files_", year_run, ".csv", sep = ""))
tabmf <- read.csv("../classif_kbas_FINAL_2020.csv")

subsets <- c("all","marine","terrestrial","Freshwater", "mountain")   #, "mountain"
ssets2run <- 1:5     #1:length(subsets)
#ssets2run=1 #for all only

yrs = seq(1900, as.numeric(format(Sys.Date(), "%Y")), by = 1) # years to be analysed, note the max year takes the year at the time the analyses are run


##############################################################################################
# define the custom functions
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


# x=ibadat2$year
# y=ibadat2$country
# z=ibadat2$perprotected
# zx=ibadat2$random_year

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


# cummulative number of IBAs that are totally protected
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


# cummulative mean percentage of IBAs that are protected
# where x = protected and y = year and id = siteid
statavep <- function(x, y, id) {
#statavep(ibadat2$perprotected, output[,1], rownames(output)) {
  #yrs = seq(1900, 2016, by = 1)
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

###############
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
    print(i)
  }
  return(res)
}

# select subsets
##############################
# calculate values
#############################
calcvals = function(x,y) {
  res <- matrix(data = NA, j, length(yrs))
  for (i in 1:j) {
    res[i,] <- stattotp(x, y[,i], rownames(y)) #total coverage
    print(i)
    print(j)
  }
  return(res)
}

calcvals2 = function(x,y) {
  res <- matrix(data = NA, j, length(yrs))
  for (i in 1:j) {
    res[i,] <- statavep(x, y[,i], rownames(y)) #mean coverage
    print(i)
    print(j)
  }
  return(res)
}

##################### end functions ########################################################

### Part 2 - run randomisation for each regional grouping in turn ###############


for (w in 5:nrow(inout)){ #loops through input files (using file names from inout csv file)
#for (w in 3:4){
  #w=13 #for CMS regions only
  #w=12 #for new World_bank only
  #w=10
  #w=3
  #w=1
  # Parameters that need to be set by user
  j = 100 # number of randomisations needed
  
  infile = as.character(inout$inputfile[w])  #"Input data for R global Region KBAs.csv"
  glb = F
  reg = T
  if (inout$global[w] == 1){ #if global
    glb <- T
    reg <- F
  }
  glb
  reg
  
  #outfile = "Output data for R global KBAs.csv"
  #outfile2 = "Output data for R Region KBAs.csv"
  
  #if (glb) {
  
  # NOTE: Outfiles will be overwritten, including those for regions.
  ####################
  
  ibadat1 <-  read.csv(infile, header = TRUE, sep = ",", quote="\"", dec=".", comment.char="", as.is = TRUE) # read in data
  
  outputfile1 <- as.character(inout$outputfile1[w])
  
  poolryears <- ibadat1$year[ibadat1$random_year == FALSE & ibadat1$year > 0] #create a list of years that were not randomly assigned, to use if a country has no random years #note this uses ibadat1 as needs to not only look at the system subset, but from the protected areas in the country as a whole
  
  
  # if (glb){
  #   outputfile1 <- "Output data for R Country KBAs"
  # }
  
  for (k in ssets2run){
    #k=1
    sset <- subsets[k]
    if (sset == 'all') { #(k == 1) {
      ibadat <- ibadat1
      outfile <- paste(inout$outputfile1[w], "_", sset, ".csv", sep = "")
    }
    #ibadat <- ibadat1 #read in data from input file
    
    if (sset != 'all') { #(k > 1) {
      kbasi <- tabmf$SitRecID[tabmf[,which(names(tabmf) == sset)] == 1]
      ibadat <- ibadat1[ibadat1$siteid %in% kbasi,]
      outfile <- paste(inout$outputfile1[w], "_", sset, ".csv", sep = "")
    }
    
    #outfile <- paste(inout$outputfile1[w], "_", sset, ".csv", sep = "")
    
    head(ibadat)
    regs <- unique(ibadat$region)
    
    # if (glb){
    #   regs <- "global"
    # }
    
    regs
    length(regs)
    
    
    
    regres <- data.frame()
    for (z in 1:length(regs)){
      #z=152
      #z=which(regs=="Germany")
      #if (reg){
      z
      regs[z]
      #ibadat <- ibadat1 #removed as didn't think this was necessary? Check
      ibadat2 <- ibadat[ibadat$region == regs[z],]
      #ibadat <- ibadat2
      #}
      
      #attach (ibadat2)
      
      #attach (ibadat)
      n <- length(ibadat2$siteid)
      ibaname <- unique(ibadat2$siteid)
      m <-  length(unique(ibadat2$siteid))
      print(paste("Number of KBAs being analysed ", m," with ", n, " sites.", sep = ""))
      #poolryears <- ibadat2$year[ibadat2$random_year == FALSE] #create a list of years that were not randomly assigned, to use if a country has no random years
      ####################
      
      ###############################
      # start data randomization
      ##############################
      
      output <- mkdata(ibadat2$perprotected, ibadat2$country, ibadat2$year, ibadat2$random_year, ibadat2$siteid, j) #creates a list of years to make the error bars from (j repetitions so in this case a list of 100 possible years) #rows are the siteIDs, columns are the repetitions
      
      # calculate the results   ## % of PAs fully covered
      alldata <- calcvals(ibadat2$perprotected, output) #creates matrix of possible values for all repetitions for all years, where columns are different years and rows are the different repetitions based on the years randomly determined above - this is now country level data
      
      res <- matrix(data = NA, ncol = 7, nrow = length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")))
      res[,1] <- yrs
      for (i in 1:length(yrs)){
        res[i,2] <- quantile(alldata[,i], probs = c(0.025))
        res[i,3] <- quantile(alldata[,i], probs = c(0.5))
        res[i,4] <- quantile(alldata[,i], probs = c(0.975))
        print(i)
      }
      res[,5] <- res[,2]/m *100
      res[,6] <- res[,3]/m *100
      res[,7] <- res[,4]/m *100
      
      res2 <- data.frame(res)
      names(res2) = c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")
      
      #write.table(res, file = outfile, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE)
      
      
      # calculate the results   ## % ok KBAs covered
      alldata <- calcvals2(ibadat2$perprotected, output)
      
      res <- matrix(data = NA, ncol = 7, nrow = length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage_Area", "95CImidPercentage_Area", "95CIhiPercentage_Area")))
      res[,1] <- yrs
      for (i in 1:length(yrs)){
        #i=23
        res[i,2] <- quantile(alldata[,i], probs = c(0.025))
        res[i,3] <- quantile(alldata[,i], probs = c(0.5))
        res[i,4] <- quantile(alldata[,i], probs = c(0.975))
        print(i)
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
      
      if (glb){
        write.csv(res2, file = outfile, row.names = F)
      }
      
      if (reg){
        regres <- rbind(regres, res2)
      }
      
      #detach(ibadat)
      
      
    } #ends loop of regs
    if (reg){
      str(regres)
      unique(regres$region)
      length(unique(regres$region))
      tail(regres)
      write.table(regres, file = outfile, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE)
    } 
    
  } #ends loop of subsets (marine, Freshwater, etc)
  # end script
} ## ends the loop of input files
##############
