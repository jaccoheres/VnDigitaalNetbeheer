# LoadData_NHN.R
# By Joris van der Els.
# Start 08-01-2014
#
# This script loads the net-topology information and the K&M scenarios for a specific region
# Easiest to create one script for each region to allow for easy modification per region

# Remove all data
rm(list=ls(all=TRUE))
gc(verbose=FALSE)

# Load packages
# No packages required

#set path
path = "C:/1. Programmeerwerk/Bottum Up Analyse/2. Data"

################################################################################ Load data 
print("--Calculate memory-intensive GV telemetry users (0/6)--")

# telemetrylist = list.files(paste0(path,"/2. Baseload GV/2. SAP TESLA"), pattern = '.csv')
# setwd(paste0(path,"/2. Baseload GV/2. SAP TESLA"))
# for(ii in 1:2){#length(telemetrylist)){
#    GVtel  = read.table(telemetrylist[ii] , sep = ",", dec=":" ,colClasses = "character", header = TRUE)   
# }
# setwd(paste0(path,"/2. Baseload GV/2. SAP TESLA"))
# load('SAP_TESLA_NHN.Rda')
# Users  = read.table("MSR_AANSLUITING.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)

setwd(paste0(path,"/2. Baseload GV/2. SAP TESLA"))

# load('SAP_TESLA_NHN.Rda')
# Users  = read.table("MSR_AANSLUITING.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)

print("--Loading data (1b/6)--")
# Asset management network data
setwd(paste0(path,"/1. Baseload KV"))
Users  = read.table("MSR_AANSLUITING.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)#, sep = ";", dec="," ,colClasses = "character", stringsAsFactors=FALSE, header = TRUE)
MSR    = read.table("MSR_AANSLUITING.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)
EDSN   = read.table("EDSN.csv"                                    , sep = ",", dec="," ,colClasses = "character", header = TRUE)
# KVonb   = read.table("KVonbekendeHLD.csv"                       , sep = "|", dec="," ,colClasses = "character", header = TRUE)
MSRonb  = read.table("KVonbekendeMSR.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)

setwd(paste0(path,"/4. Kabel en MSR-gegevens"))
HLDcap  = read.table("LS_kabel_bonoka.txt"                             , sep = "|", dec="." , header = TRUE)
MSRcap  = read.table("MSRgegevens_bonoka.csv"                          , sep = ",", dec="," ,colClasses = "character", header = TRUE)
HLDspec = read.table("Match kabeltypes NHN.csv"                        , sep = ";", dec="," ,colClasses = "character", header = TRUE)
Vnames  = read.table("Vertaaltabel Vision_ID naar NRG_Nr_Behuizing.csv" , sep = ";", dec="," ,colClasses = "character", header = TRUE)

setwd(paste0(path,"/2. Baseload GV"))
GV            = read.table("GVBaseloadsaanstations.csv"                      , sep = ",", dec="," ,colClasses = "character", header = TRUE)
GVprofiletext = read.table("profielenGV.csv"                                 , sep = ",", dec="," ,colClasses = "character", header = TRUE)

# Klant & Markt scenario's
setwd(paste0(path,"/5. K&M input/EV KV"))
EV_low      = read.table("20141111_NHN_Scen1.csv"  , sep = ",", dec="," ,colClasses = "character", header = TRUE)
EV_med      = read.table("20141111_NHN_Scen2.csv"  , sep = ",", dec="," ,colClasses = "character", header = TRUE)
EV_high     = read.table("20141111_NHN_Scen3.csv"  , sep = ",", dec="," ,colClasses = "character", header = TRUE)
EV_hydro    = read.table("20141111_NHN_Scen4.csv"  , sep = ",", dec="," ,colClasses = "character", header = TRUE) # This is a hydrogen vehicle scenario
EV_profile  = read.table("EV thuislaadprofiel.csv" , sep = ";", dec=",", header = TRUE)

setwd(paste0(path,"/5. K&M input/PV KV"))
PV_low     = read.table("castoutNHLow.csv"        , sep = ",", dec="," ,colClasses = "character", header = TRUE)
PV_med     = read.table("castoutNHMed.csv"        , sep = ",", dec="," ,colClasses = "character", header = TRUE)
PV_high    = read.table("castoutNHHigh.csv"       , sep = ",", dec="," ,colClasses = "character", header = TRUE)
PV_profile = read.table("PV_profile_dec2014.csv"  , sep = ";", dec="," , header = FALSE)

setwd(paste0(path,"/5. K&M input/WP KV"))
WP_low     = read.table("wp laag-2030.csv"    , sep = ";", dec="," ,colClasses = "character", header = TRUE)
WP_med     = read.table("wp midden-2030.csv"  , sep = ";", dec="," ,colClasses = "character", header = TRUE)
WP_high    = read.table("wp hoog-2030.csv"    , sep = ";", dec="," ,colClasses = "character", header = TRUE)
WP_profile = read.table("WP profiel_2Dec_JvdE.csv"                       , sep = ";", dec="," , header = FALSE)

######################################################### Save results
print("--Saving results (6/6)--")
setwd(paste0(path,"/7. Output"))

# Save necessary data
save.image("Data_NH_v2.RData")
print("--Done!--")