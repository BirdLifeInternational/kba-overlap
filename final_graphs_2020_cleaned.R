### Code to generate PDF and PNG outputs of the kba-protected area overlaps for each regional grouping

#set working directory
folder <- "C:/Users/Ashley.Simkins/Documents/SDG/KBA-PA overlap/KBA_PA_Overlap_rewritten/input tables 2019"
setwd(folder)

year_run <- format(Sys.Date(), "%Y") #get the current year


#create main folder in which to store the output graphs
if (file.exists(paste(folder, "/../final graphs ", year_run, sep = ""))){ #create graphs folder
  finfolder <- paste(folder, "/../final graphs ", year_run, sep = "")
} else {
  dir.create(file.path(paste(folder, "/../final graphs ", year_run, sep = "")))
  finfolder <- paste(folder, "/../final graphs ", year_run, sep = "")
}

#define function needed
lu <- function (x = x){
  length(unique(x))
}


cname <- "KBAs" #define for use in filenames later           

pdf_run <- F #make PDF graphs
png_run <- T #make PNG graphs

fls <- dir(pattern = "Output data for ") #only selects the randomisation output files in the folder
fls
length(fls)

for (w in 1:length(fls)){ #for all data
#for (w in 56:length(fls)) #for just world bank data
  #w=11
  fl1 <- fls[w]
  fl1
  tab <- read.csv(fl1)    ### or KBAs
  
  # Data fixing (where errors in names)
  if (!is.null(tab$region)){
    tab$region <- as.character(tab$region) #set region column to be a character variable
    #Encoding(tab$region) <- "UTF-8" #fix character encoding
    tab$region[tab$region == "C<f4>te d'Ivoire"] <- "Cote d'Ivoire"
    tab$region[tab$region == "Cura<e7>ao (to Netherlands)"] <- "Curacao (to Netherlands)"
    tab$region[tab$region == "S<e3>o Tom<e9> e Pr<ed>ncipe"] <- "Sao Tome e Principe"
    tab$region[tab$region == "R<e9>union (to France)"] <- "Reunion (to France)"
    tab$region[tab$region == "Western Asia<U+00A0>(M49) and Northern Africa (M49)"] <- "Western Asia (M49) and Northern Africa (M49)"
  }
  
  sset <- tab$sset[1]
  desc <- tab$code[1]
  desc
  
  #check and create folder for that regional grouping in which to store graphs
  if (!(file.exists(paste(finfolder, sset, sep = "/")))){ #if folder does not already exist, create it
    dir.create(file.path(paste(finfolder, sset, sep = "/")))
  }
  
  #check and create folder for that system in which to store graphs
  if (!(file.exists(paste(finfolder, sset,desc,  sep = "/")))){ #if subfolder does not already exist, create it
    dir.create(file.path(paste(finfolder, sset, desc, sep = "/")))
  }
  
  
  
  #finfolder <- paste(folder, "final graphs/", sep="/")  ### "graphs_per_country/" should be an existing subfolder in the folder mentioned above ; change if necessary
  
  ### Set up graphs
  
  varis <- c("percKBAcov","percKBATot")
  
  for (y in 1:length(varis)){
    #y=1
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
      lbgr <- "mean % area covered by protected areas"
    }
    
    cinz <- grey(0.7)
    min(tab$qn95 - tab$qn05)
    max(tab$qn95 - tab$qn05)
    
    head(tab)
    str(tab)
    
    cts <- unique(tab$region[tab$region != "0"])
    lu(cts)
    
    
    
    ###### produce single graph with all countries - not sure we actually have ever used this?
    # if (length(cts) > 1){
    # 
    #   if (png_run){
    #     pngallname <- paste(finfolder, "/", sset,"/", desc, "/", nfile, cname, " all plots ", desc, "_", sset, ".png", sep = "")
    #     pngallname
    #     png(file = pngallname, width = 12, height = 10, units = "in", res = 600)
    #   }
    # 
    #   if (pdf_run){
    #     pdfallname <- paste(finfolder, "/", sset,"/", desc, "/", nfile, cname, " all plots ", desc, "_", sset, ".pdf", sep = "")
    #     pdfallname
    #     pdf(file = pdfallname, width = 12, height = 10)
    #   }
    # 
    #   par(las = 1, cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.5, mar = c(5,5,4,2), mgp = c(3.5, 1, 0))
    # 
    #   for (i in 1:length(cts)){
    #     #i=1
    #     #i=which(cts=="CAF")
    #     ct1 <- as.character(cts[i])
    #     ct1
    #     tabi <- tab[tab$region == ct1,]
    #     head(tabi)
    # 
    #     basy <- 0
    #     topy <- 100
    #     baseyr <- 1980
    #     if (max(tabi$qn95) < 20){
    #       topy <- 50
    #     }
    # 
    #     par(las = 1, cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.5, mar = c(5,5,4,2), mgp = c(3.5, 1, 0))
    # 
    #     with(tabi,plot(year, mid, ylim = c(basy,topy), xlim = c(baseyr, max(year) + 1), type = "n", xlab = "Year", ylab = lbgr, yaxt = "n", main = ct1))
    #     axis(2, seq(basy, topy, by = (topy - basy)/5))
    #     with(tabi, polygon(c(year, rev(year)), c(qn05, rev(qn95)), col = cinz, border = cinz))
    #     with(tabi, lines(year, mid, col = 2, lwd = 2))
    #   }
    #   dev.off()
    # } #end loop one graph for all countries
    
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
      
      if (png_run){
        png1name <- paste(finfolder, "/", sset,"/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".png", sep = "")
        png1name
        png(file <- png1name, width = 12, height = 10, units = "in", res = 600)
      }
      
      if (pdf_run){
        pdf1name <- paste(finfolder, "/", sset,"/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".pdf", sep = "")
        pdf1name
        pdf(file <- pdf1name, width = 12, height = 10)
      }
      
      par(las = 1,cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.5, mar = c(5,5,4,2), mgp = c(3.5, 1, 0))
      
      with(tabi,plot(year,mid, ylim = c(basy,topy), xlim = c(baseyr, max(year) + 1), type = "n", xlab = "Year", ylab = lbgr, yaxt = "n"))
      axis(2, seq(basy, topy, by = (topy - basy)/5))
      
      with(tabi, polygon(c(year, rev(year)), c(qn05, rev(qn95)), col = cinz, border = cinz))
      with(tabi, lines(year, mid, col = 2, lwd = 2))
      dev.off()
    }
    
    if (length(cts) > 0){
      for (i in 1:length(cts)){
        #i=1
        #i=which(cts=="CAF")
        ct1 <- as.character(cts[i])
        ct1
        tabi <- tab[tab$region == ct1,]
        head(tabi)
        basy <- 0
        topy <- 100
        baseyr <- 1980
        if (max(tabi$qn95) < 20){
          topy <- 50
        }
        
        if (png_run){
          png1name  <- paste(finfolder, "/", sset, "/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".png", sep = "")
          png1name
          png(file = png1name, width = 12, height = 10, units = "in", res = 600)
        }
        
        if (pdf_run){
          pdf1name <- paste(finfolder, "/", sset, "/", desc, "/", nfile, cname, "_", ct1, "_", sset, ".pdf", sep = "")
          pdf1name
          pdf(file = pdf1name, width = 12, height = 10)
        }
        
        par(las = 1,cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.5, mar = c(5,5,4,2), mgp = c(3.5, 1, 0))
        
        with(tabi,plot(year, mid, ylim = c(basy, topy), xlim = c(baseyr, max(year) + 1), type = "n", xlab = "Year", ylab = lbgr, yaxt = "n"))
        axis(2, seq(basy, topy, by = (topy - basy)/5))
        
        with(tabi, polygon(c(year, rev(year)), c(qn05, rev(qn95)), col = cinz, border = cinz))
        with(tabi, lines(year, mid, col = 2, lwd = 2))
        dev.off()
      }
    } # ends loop one graph per country
  } # ends loop varis
} # ends loop input files



## create single file for outputs

fls <- dir(pattern = "Output data for ")
fls
length(fls)

fin1 <- data.frame()
for (w in 1:length(fls)){ #length(fls)
  #w=7
  fl1 <- fls[w]
  fl1
  tab <- read.csv(fl1)    ### or KBAs
  head(tab)
  nrow(tab)
  unique(tab$region)
  
  if (!"region" %in% names(tab)){
    tab$region="Global"
  }
  
  head(tab)
  tab <- tab[tab$region != "0",]
  nrow(tab)
  unique(tab$region)
  tab2 <- tab[c("year","X95CIlowCount","X95CImidCount","X95CIhiCount","X95CIlowPercentage","X95CImidPercentage","X95CIhiPercentage","CI95lowPercentage_Area","CI95midPercentage_Area","CI95hiPercentage_Area","region","sset")]
  tab2$file1 <- fl1
  fin1 <- rbind(fin1,tab2)
}

length(unique(fin1$file1)) # should be the number of output files to merge

subsets <- unique(fin1$sset) 

for (g in 1:length(subsets)){
  #g=1
  sset <- subsets[g]
  
  nffile <- paste("PA coverage ", sset, " KBAs by country  region.csv")
  nffile
  fin2 <- fin1[fin1$sset == sset,]
  nrow(fin2)
  unique(fin2$sset)
  fin2 <- fin2[,1:11]
  names(fin2) <- c("year","Count of KBAs completely covered by PAs (lower CI)","Count of KBAs completely covered by PAs (estimate)","Count of KBAs completely covered by PAs (upper CI)","% KBAs completely covered by PAs (lower CI)","% KBAs completely covered by PAs (value)","% KBAs completely covered by PAs (upper CI)","Mean % area of each KBA covered by PAs (lower CI)","Mean % area of each KBA covered by PAs (value)","Mean % area of each KBA covered by PAs (upper CI)","Global/region/country")
  write.csv(fin2, nffile, row.names = F)
}

