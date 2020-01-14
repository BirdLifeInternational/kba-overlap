
# IBA randomizer 0.8
# JPW Scharlemann 14/04/2014 jorn.scharlemann@unep-wcmc.org
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
# year = year of protection, 3000 if missing information
####################

setwd ("C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables 2019")
workd <- "C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables 2019"

inout <- read.csv("../in_out_files.csv")
tabmf <- read.csv("../classif_kbas_FINAL2019.csv")

subsets <- c("all","marine","terrestrial","Freshwater", "mountain")   #, "mountain"
ssets2run <- 1:5     #1:length(subsets)
#ssets2run=1 #for all only

yrs = seq(1900, 2019, by = 1) # years to be analysed


for (w in 1:nrow(inout)) #loops through input files (using file names from inout csv file)
{
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
  if (inout$global[w] == 1) #if global
  {
    glb=T
    reg=F
  }
  glb
  reg
  
  #outfile = "Output data for R global KBAs.csv"
  #outfile2 = "Output data for R Region KBAs.csv"
  
  if (glb) {
    
    # NOTE: Outfiles will be overwritten, including those for regions.
    ####################
    
    ibadat1 = read.csv(infile, header = TRUE, sep = ",", quote="\"", dec=".", comment.char="", as.is = TRUE) # readin data
    
    for (k in ssets2run)
    {
      #k=1
      sset=subsets[k]
      if (k == 1) {
        ibadat=ibadat1
        outfile=paste(inout$outputfile1[w], ".csv", sep="")
      }
      if (k > 1) {
        kbasi=tabmf$SitRecID[tabmf[,which(names(tabmf)==sset)]==1]
        ibadat=ibadat1[ibadat1$siteid%in%kbasi,]
        outfile=paste(inout$outputfile1[w], "_", sset, ".csv", sep="")
      }
      
      attach (ibadat)
      n = length(siteid)
      ibaname = unique(siteid)
      m = length(unique(siteid))
      print(paste("Number of IBAs being analysed ", m," with ",n, " sites.", sep = ""))
      poolryears=ibadat$year
      ####################
      # define the custom functions
      # correct sampling function, as standard R function not correct
      resample <- function(x, size, ...)
        if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
        } else sample(x, size, ...)
      # random allocation of year protected for those with missing years, asign from all if none found in country
      # where x = year, y = country and z = %protected
      ranyear <-function(x, y, z) {
        out <- as.vector(c())
        for (i in 1:length(y))
        {
          if(x[i] != 3000) {
            out = append(out, x[i], after = length(out))
          } else {
            if(length(as.numeric(x[y == y[i] & x != 3000 & z > 0])) > 1) {
              out = append(out, resample(as.numeric(x[y == y[i] & x != 3000 & z > 0]), 1, replace = TRUE), after = length(out))
            } else {
              out = append(out, resample(as.numeric(x[y == y & x != 3000 & z > 0]), 1, replace = TRUE), after = length(out))
            }}}
        return(out)
      }
      
      # cummulative number of IBAs that are totally protected
      # where x = protected and y = year and id = siteid
      stattotp <-function(x, y, id) {
        #yrs = seq(1900, 2016, by = 1)
        out <- as.vector(c())
        for (i in 1:length(yrs))
        {
          temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum)
          out = append(out, length(temp[temp >= 98]), after = length(out))
        }
        return(out)
      }
      
      
      # cummulative average percentage of IBAs that are protected
      # where x = protected and y = year and id = siteid
      statavep <-function(x, y, id) {
        #yrs = seq(1900, 2016, by = 1)
        out <- as.vector(c())
        for (i in 1:length(yrs))
        {
          temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum)
          out = append(out, sum(temp/100), after = length(out))
        }
        return(out)
      }
      
      
      ###############################
      # start data randomization
      ##############################
      mkdata <- function(x, y, z, j) {
        # where X = %protected, y=country, z=year, j=number of iterations
        # results are written into an array with row, column, dimension representing samples, years from 1900-2016, and count of IBAs with 100% protection & mean area protected
        # res[,,1] will give counts of IBAs with 100% protection, and res[,,2] will give the means
        res <- matrix(data=NA, length(x),j)
        rownames (res) <- siteid # write the IBA ID number to rowname
        for (i in 1:j) {
          ry = ranyear(z, y, x)
          #st = stattotp (rp, ry)
          #sm = statmeanp(rp, ry)
          res[,i] <- ry
          print(i)
        }
        return(res)
      }
      output = mkdata(perprotected, country, year, j)
      
      # select subsets
      ##############################
      # calculate values
      #############################
      calcvals = function(x,y) {
        res <- matrix(data=NA, j, length(yrs))
        for (i in 1:j) {
          res[i,] <- stattotp(x, y[,i], rownames(y))
          print(i)
          print(j)
        }
        return(res)
      }
      
      calcvals2 = function(x,y) {
        res <- matrix(data=NA, j, length(yrs))
        for (i in 1:j) {
          res[i,] <- statavep(x, y[,i], rownames(y))
          print(i)
          print(j)
        }
        return(res)
      }
      
      # calculate the results   ## % of PAs fully covered
      alldata = calcvals(perprotected, output)
      
      res <- matrix(data=NA, ncol=7, nrow=length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")))
      res[,1] <- yrs
      for (i in 1:length(yrs))
      {
        res[i,2] <-quantile(alldata[,i], probs = c(0.025))
        res[i,3] <-quantile(alldata[,i], probs = c(0.5))
        res[i,4] <-quantile(alldata[,i], probs = c(0.975))
        print(i)
      }
      res[,5] <- res[,2]/m *100
      res[,6] <- res[,3]/m *100
      res[,7] <- res[,4]/m *100
      
      res2=data.frame(res)
      names(res2)=c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")
      
      #write.table(res, file = outfile, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE)
      
      
      # calculate the results   ## % ok KBAs covered
      alldata = calcvals2(perprotected, output)
      
      res <- matrix(data=NA, ncol=7, nrow=length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage_Area", "95CImidPercentage_Area", "95CIhiPercentage_Area")))
      res[,1] <- yrs
      for (i in 1:length(yrs))
      {
        #i=23
        res[i,2] <-quantile(alldata[,i], probs = c(0.025))
        res[i,3] <-quantile(alldata[,i], probs = c(0.5))
        res[i,4] <-quantile(alldata[,i], probs = c(0.975))
        print(i)
      }
      res[,5] <- res[,2]/m *100
      res[,6] <- res[,3]/m *100
      res[,7] <- res[,4]/m *100
      
      res2$CI95lowPercentage_Area=res[,5]
      res2$CI95midPercentage_Area=res[,6]
      res2$CI95hiPercentage_Area=res[,7]
      res2$code=inout$code[w]
      res2$sset=sset
      write.csv(res2, file = outfile, row.names=F)
      
      detach(ibadat)
      
    }
  }
  # end script
  ##############
  
  
  
  
  
  
  ### by region
  
  if (reg) {
    
    #########################################
    #infile = "Input data for R global LDC KBAs.csv"
    #outfile2 = "Output data for R LDC KBAs.csv"
    
    ibadat1 = read.csv(infile, header = TRUE, sep = ",", quote="\"", dec=".", comment.char="", as.is = TRUE) # read in data
    
    outputfile1=as.character(inout$outputfile1[w])
    #if (glb) outputfile1="Output data for R Country KBAs"
    
    #if(!"region"%in%names(ibadat1)) ibadat1$region=ibadat1$country
    
    for (k in ssets2run)
    {
      #k=1
      sset=subsets[k]
      if (k==1) {
        ibadat=ibadat1
        outfile2=paste(outputfile1, ".csv", sep="")
      }
      if (k>1) {
        kbasi=tabmf$SitRecID[tabmf[,which(names(tabmf)==sset)]==1]
        ibadat=ibadat1[ibadat1$siteid%in%kbasi,]
        outfile2=paste(outputfile1, "_", sset, ".csv", sep="")
      }
      
      head(ibadat)
      regs=unique(ibadat$region)
      regs
      length(regs)
      
      
      #j=1
      #j=1000
      #j=1000
      
      regres=data.frame()
      for (z in 1:length(regs))
      {
        #z=152
        #z=which(regs=="Germany")
        z
        ibadat2=ibadat[ibadat$region==regs[z],]
        attach (ibadat2)
        
        n = length(siteid)
        ibaname = unique(siteid)
        m = length(unique(siteid))
        print(paste("Number of IBAs being analysed ", m," with ",n, " sites.", sep = ""))
        ####################
        # define the custom functions
        # correct sampling function, as standard R function not correct
        resample <- function(x, size, ...)
          if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
          } else sample(x, size, ...)
        # random allocation of year protected for those with missing years, asign from all if none found in country
        # where x = year, y = country and z = %protected
        ranyear <-function(x, y, z) {
          out <- as.vector(c())
          for (i in 1:length(y))
          {
            if(x[i] != 3000) {
              out = append(out, x[i], after = length(out))
            } else {
              if(length(as.numeric(x[y == y[i] & x != 3000 & z > 0])) > 1) {
                out = append(out, resample(as.numeric(x[y == y[i] & x != 3000 & z > 0]), 1, replace = TRUE), after = length(out))
              } else {
                out = append(out, resample(poolryears, 1, replace = TRUE), after = length(out))
              }}}
          return(out)
        }
        
        # cummulative number of IBAs that are totally protected
        # where x = protected and y = year and id = siteid
        stattotp <-function(x, y, id) {
          #yrs = seq(1900, 2016, by = 1)
          out <- as.vector(c())
          for (i in 1:length(yrs))
          {
            temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum)
            out = append(out, length(temp[temp >= 98]), after = length(out))
          }
          return(out)
        }
        
        
        # cummulative average percentage of IBAs that are protected
        # where x = protected and y = year and id = siteid
        statavep <-function(x, y, id) {
          #yrs = seq(1900, 2016, by = 1)
          out <- as.vector(c())
          for (i in 1:length(yrs))
          {
            temp = tapply(x[y <= yrs[i]], id[y <= yrs[i]], sum)
            if (length(temp)>0) out = append(out, sum(temp/100), after = length(out))
            if (length(temp)==0) out = append(out, 0, after = length(out))
          }
          return(out)
        }
        
        
        ###############################
        # start data randomization
        ##############################
        mkdata <-function(x, y, z, j) {
          # where X = %protected, y=country, z=year, j=number of iterations
          # results are written into an array with row, column, dimension representing samples, years from 1900-2016, and count of IBAs with 100% protection & mean area protected
          # res[,,1] will give counts of IBAs with 100% protection, and res[,,2] will give the means
          res <- matrix(data=NA, length(x),j)
          rownames (res) <- siteid # write the IBA ID number to rowname
          for (i in 1:j) {
            ry = ranyear(z, y, x)
            #st = stattotp (rp, ry)
            #sm = statmeanp(rp, ry)
            res[,i] <- ry
            print(i)
          }
          return(res)
        }
        output = mkdata(perprotected, country, year, j)
        
        # select subsets
        ##############################
        # calculate values
        #############################
        calcvals = function(x,y) {
          res <- matrix(data=NA, j, length(yrs))
          for (i in 1:j) {
            res[i,] <- stattotp(x, y[,i], rownames(y))
            print(i)
            print(j)
          }
          return(res)
        }
        
        calcvals2 = function(x,y) {
          res <- matrix(data=NA, j, length(yrs))
          for (i in 1:j) {
            res[i,] <- statavep(x, y[,i], rownames(y))
            print(i)
            print(j)
          }
          return(res)
        }
        
        
        # calculate the results   % PAs totally covered
        alldata = calcvals(perprotected, output)
        
        res <- matrix(data=NA, ncol=7, nrow=length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")))
        res[,1] <- yrs
        for (i in 1:ncol(alldata))
        {
          res[i,2] <-quantile(alldata[,i], probs = c(0.025))
          res[i,3] <-quantile(alldata[,i], probs = c(0.5))
          res[i,4] <-quantile(alldata[,i], probs = c(0.975))
          print(i)
        }
        res[,5] <- res[,2]/m *100
        res[,6] <- res[,3]/m *100
        res[,7] <- res[,4]/m *100
        
        res2=data.frame(res)
        names(res2)=c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage", "95CImidPercentage", "95CIhiPercentage")
        
        # calculate the results   % coverage of KBAs
        alldata = calcvals2(perprotected, output)
        
        res <- matrix(data=NA, ncol=7, nrow=length(yrs), dimnames = list(c(seq(1,length(yrs), by =1)),c("year", "95CIlowCount","95CImidCount","95CIhiCount", "95CIlowPercentage_Area", "95CImidPercentage_Area", "95CIhiPercentage_Area")))
        res[,1] <- yrs
        for (i in 1:length(yrs))
        {
          res[i,2] <-quantile(alldata[,i], probs = c(0.025))
          res[i,3] <-quantile(alldata[,i], probs = c(0.5))
          res[i,4] <-quantile(alldata[,i], probs = c(0.975))
          print(i)
        }
        res[,5] <- res[,2]/m *100
        res[,6] <- res[,3]/m *100
        res[,7] <- res[,4]/m *100
        
        res2$CI95lowPercentage_Area=res[,5]
        res2$CI95midPercentage_Area=res[,6]
        res2$CI95hiPercentage_Area=res[,7]
        
        res2$region=regs[z]
        res2$code=inout$code[w]
        res2$sset=sset
        
        detach(ibadat2)
        
        regres=rbind(regres, res2)
      }
      
      str(regres)
      unique(regres$region)
      length(unique(regres$region))
      tail(regres)
      write.table(regres, file = outfile2, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE)
    } # ends the loop of subsets (marine, freswhater, etc)
  } # ends loop regional
  
} ## ends the loop of input files
