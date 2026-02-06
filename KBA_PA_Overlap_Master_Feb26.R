## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KBA-protected area overlap calculator v3.1 (https://github.com/BirdLifeInternational/kba-overlap)
## Tom Scott (tom.scott@birdlife.org), May 2024 - capacity to split OECM coverage out from protected areas, restructure to allow for master script and general performance updates
## based on code by Maria Dias (2016) updated by Ash Simkins & Lizzie Pearmain (March 2020).
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### IMPORTANT NOTES
# WDPA layer is extremely large, can take several minutes (up to half an hour) to load.
# This code runs the spatial overlap itself country by country and saves the output each time. This avoids reanalysing the countries already done in case of error.

# See documentation stored on GitHub for instructions and background on using this code.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 1. Initial set up and data loading/cleaning ----
##TODO: Set folder as working directory and year run (to be used for file and folder naming later on) 
folder <- "C:/Users/Tom.Scott/OneDrive - BirdLife International/Sites/SDG 2026/WorkingFolder/"
setwd(folder)
year_run <- format(Sys.Date(), "%Y")

##Call all functions from functions file in directory - this contains all the processes required to complete this analysis and produce outputs below
source("KBA_PA_Overlap_Functions_May24.r")

##Load packages
load_packages()

##Build full directory with other folders for output files - this will not overwrite these files should they already exist
build_directory()

##TODO: Read in all premade tabular datasets (see documentation) - add in names of respective files below 
tabmf <- read.csv("RawInput/kbas_realms_sept2025.csv") #table containing classifications of terrestrial, marine, mountain and freshwater kbas
isos <- read.csv("RawInput/ISOTableDec25PreFullRegions.csv") #iso table
notassessed <- read.csv("RawInput/") #table of point sites structured to reflect pa_coverage_per_kba output file 
siteclass <- read.csv("RawInput/") #table of IBA/KBA/AZE status for IBAT output 

#### 2. Spatial analysis ----
##TODO: Read in all premade spatial datasets.
kbas <- st_read('RawInput/', stringsAsFactors = F) #kba layer
pas <- st_read('RawInput/', layer = '', stringsAsFactors = F) #pa layer

##Clean kbas
kba_clean()

##Remove pas not included in this analysis for SDG reporting as these often contain potentially unprotected areas
pas <- filter(pas, 
              !STATUS %in% c("Proposed","Not Reported"),
              DESIG_ENG != "UNESCO-MAB Biosphere Reserve")

##Transboundary pas - assigns to both countries
transboundary_pas()

##TODO Define PA/OECM Split and create lists
OECM_split = T #Split out OECM coverage from PA coverage (not needed for SDG reporting)
pa_oecm_split()

##List countries to run this analysis for - this list can be changed to contain any specific countries for more focused analysis or reruns 
listcnts <- as.character(unique(kbas$ISO3))

##Complete overlap analysis - testing this globally took approx 4.5 hours to run, potential to reduce this by filtering the PA layer pre-analysis to only include those sites that intersect with KBAs at all 
complete_overlap()

#### 3. Randomising country level for IBAT ----
##Re-scaling results 
rescale_results()

##Create "in_out_table" and making "input tables" 
fields <- colnames(select(isos, c("ISO_BL", "ISO_SDG", "global","SDG_Region", "SDG_Subregion", "Developed_Developing", "LDCs", "LLDCs_SIDs", "ECA_ALL", "ECA_CA", "ECA_EA", "ECA_NA", "ECA_SA",
                                  "ECA_WA", "ECA_AUC", "ECA_ECCAS", "ECA_ECOWAS", "ECA_IGAD", "ECA_SADC", "ECA_AMU", "ECA_CEN_SAD", "ECA_COMESA", "ECA_EAC", "ECA_IE",
                                  "ECA_LLC", "ECA_LL_LDC", "ECA_LDC", "ECA_MRC", "ECA_NO_LDC", "ECA_NOP", "ECA_O_LDC", "ECA_OP", "ECA_SAHEL", "ECA_SIS", "ECA_SSA",
                                  "ECA_SACU", "ECE_ALL", "ECE_CIS11", "ECE_EECCA", "ECE_EU27", "ECE_EU_EuroArea", "ECE_WestBalkans", "ECE_EAEU", "ECLAC_ALL",
                                  "ECLAC.LA", "ECLAC_Caribbean", "ESCAP_ALL", "ESCAP_ASEAN", "ESCAP_ENEA", "ESCAP_LDC", "ESCAP_LLDC", "ESCAP_NCA", "ESCAP_PACIFIC",
                                  "ESCAP_SEA", "ESCAP_SSWA", "ESCAP_OTHER_AREA", "ESCAP_PIDE", "ESCAP_SAARC", "ESCAP_CSN", "ESCAP_SIDS", "ESCAP_WB_HIGH", "ESCAP_WB_UPPER_MID",
                                  "ESCAP_WB_LOWER_MID", "ESCAP_WB_LOW", "ESCWA_Arab", "ESCWA_GCC", "ESCWA_MASHREQ", "ESCWA_MAGHREB", "ESCWA_LDC", "ESCWA_CONFLICT", "ESCWA_NOCONFLICT_MID",
                                  "ESCWA_ARAB_MID", "ESCWA_ARAB_LOW", "ESCWA_ARAB_HIGH"
))) #select relevant columns from ISO table - now includes all regions for regional UNSD offices
fields <- c('global', "country", fields) #adds a global category to the table output, and country is the information from the KBA layer
build_inout() #creates in out table of all fields above specifying file names - only "ISO_SDG" will be used for IBAT

fields2use <- c("global","ISO_SDG")
input_tables()

##Subsets and years
subsets <- c("all","marine","terrestrial","Freshwater", "mountain")
yrs = seq(1900, as.numeric(year_run), by = 1) # years to be analysed, note the max year takes the year at the time the analyses are run

##Create randomisation functions
random_function()

##Randomisiation and writing output files - into new output folders - this will only do randomisation for ISO_SDG groupings (country level) which is what is required by IBAT
inout <- filter(inout_full, code == "ISO_SDG")
j = 1000 # number of randomisations needed
sdg_run = F #If false, this produces trends for PAs and OECMs seperately as well as combined values
randomisation()

#### 4. Summary tables for IBAT and internal reporting ----
#Summary per kba
summary_per_kba() #saves output csv

#Summary global 
summary_global() #saves output csv, must run summary_per_kba first

#### 5. Producing output for IBAT----
#IBAT fig 6 plotting (current final graphs)
ibat_fig6_plot() #saves output pngs

#IBAT fig 6 legends
ibat_fig6_legends() #saves output csv

#IBAT fig 5 create input file and reformat as per item 50 of xlsx
ibat_fig5_dataprep() #saves output csv

#IBAT fig 5 plotting (kba, iba and aze - can code in options for these)
ibat_fig5_plot() #saves output pngs

#IBAT fig 5 legends
ibat_fig5_legends() #saves output csv

#IBAT table 7
ibat_table7() #TODO must first run ibat_fig5_dataprep

#Summary per country (percentage coverage by country.csv) - this has dependencies on other functions in part 4 hence it is included here
summary_per_country()

#### 6. SDG reporting - ONLY COMPLETE ONCE UPDATED ISO TABLE WITH REGIONAL GROUPINGS RECIEVED FROM UNSD  ----
##Setting up
year_run <- as.numeric(format(Sys.Date(), "%Y")) - 1  #Setting year_run but behind one, this part is typically done a couple of months after the above and thus the year needs to match that of when the previous work was done
isos <- read.csv("RawInput/FinalRegionalISOTableDec25.csv") #read in iso table
inout_full <- read.csv(paste("in_out_files", year_run, '.csv', sep=""))
reg_grp <- read.csv("RawInput/regional_groupings_Jan26.csv",check.names = FALSE) #TODO read in table containing regional groups for SDG reporting 
reg_com_grp <- read.csv("RawInput/regional_commission_groupings_Jan26.csv")

##Re-scaling results 
rescale_results()

##TODO Creating input tables - check if any more regional groupings are to be added here
fields2use <- c("global","SDG_Region", "SDG_Subregion", "Developed_Developing", "LDCs", "LLDCs_SIDs", "ECA_ALL", "ECA_CA", "ECA_EA", "ECA_NA", "ECA_SA",
                "ECA_WA", "ECA_AUC", "ECA_ECCAS", "ECA_ECOWAS", "ECA_IGAD", "ECA_SADC", "ECA_AMU", "ECA_CEN_SAD", "ECA_COMESA", "ECA_EAC", "ECA_IE",
                "ECA_LLC", "ECA_LL_LDC", "ECA_LDC", "ECA_MRC", "ECA_NO_LDC", "ECA_NOP", "ECA_O_LDC", "ECA_OP", "ECA_SAHEL", "ECA_SIS", "ECA_SSA",
                "ECA_SACU", "ECE_ALL", "ECE_CIS11", "ECE_EECCA", "ECE_EU27", "ECE_EU_EuroArea", "ECE_WestBalkans", "ECE_EAEU", "ECLAC_ALL",
                "ECLAC.LA", "ECLAC_Caribbean", "ESCAP_ALL", "ESCAP_ASEAN", "ESCAP_ENEA", "ESCAP_LDC", "ESCAP_LLDC", "ESCAP_NCA", "ESCAP_PACIFIC",
                "ESCAP_SEA", "ESCAP_SSWA", "ESCAP_OTHER_AREA", "ESCAP_PIDE", "ESCAP_SAARC", "ESCAP_CSN", "ESCAP_SIDS", "ESCAP_WB_HIGH", "ESCAP_WB_UPPER_MID",
                "ESCAP_WB_LOWER_MID", "ESCAP_WB_LOW", "ESCWA_Arab", "ESCWA_GCC", "ESCWA_MASHREQ", "ESCWA_MAGHREB", "ESCWA_LDC", "ESCWA_CONFLICT", "ESCWA_NOCONFLICT_MID",
                "ESCWA_ARAB_MID", "ESCWA_ARAB_LOW", "ESCWA_ARAB_HIGH")
input_tables()

##Subsets and years - just in this script, not as extra function
subsets <- c("all","marine","terrestrial","Freshwater", "mountain")
yrs = seq(1900, as.numeric(year_run), by = 1) # years to be analysed, note the max year takes the year at the time the analyses are run

##Create randomisation functions
random_function()

##Randomisiation and writing output files - into new output folders - this will only do randomisation for ISO_SDG groupings (country level)
inout <- filter(inout_full, !code %in% c("ISO_SDG","ISO_BL","country"))
j = 1000 # number of randomisations needed
sdg_run = T #If false, this produces trends for PAs and OECMs separately as well as combined values
randomisation()

#Creating SDG Coverage files
sdg_format_1()

#Reformat regional groupings 
fix_reg_grp()

#Create and save SDG output files
sdg_format_2()

#Create output files for regional commission reporting
sdg_reg_com()

