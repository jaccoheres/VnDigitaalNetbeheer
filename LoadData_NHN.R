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
drive = substr(getwd(),1,3)
path = paste0(drive,"1. Programmeerwerk/Bottum Up Analyse/2. Data")

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
# 
# setwd(paste0(path,"/2. Baseload GV/2. SAP TESLA"))

print("--Loading data (1b/6)--")
# Asset management network data
setwd(paste0(path,"/1. Baseload KV"))
Users  = read.table("MSR_AANSLUITING.csv"                                , sep = ",", dec=",", as.is=c(2,3,8,18),numerals="no.loss",header = TRUE)
MSR    = read.table("MSR_AANSLUITING.csv"                                , sep = ",", dec=",", as.is=c(2,3,8,18),numerals="no.loss",header = TRUE)
EDSN   = read.table("EDSN.csv"                                           , sep = ",", dec=".", colClasses = c(rep("character",3),rep("numeric",10)) ,header = TRUE)
# KVonb   = read.table("KVonbekendeHLD.csv"                                , sep = "|", dec="," ,colClasses = "character", header = TRUE)
# MSRonb  = read.table("KVonbekendeMSR.csv"                                , sep = ",", dec="," ,header = TRUE)

setwd(paste0(path,"/4. Kabel en MSR-gegevens"))
HLDcap  = read.table("LS_kabel_bonoka.txt"                               , sep = "|", dec=".", as.is=c(5,11), header = TRUE)
MSRcap  = read.table("MSRgegevens_bonoka.csv"                            , sep = ",", dec=",", as.is=c(2,7,9,11,12), header = TRUE)
HLDspec = read.table("Match kabeltypes NHN.csv"                          , sep = ";", dec=",", as.is=1, header = TRUE)
Vnames  = read.table("Vertaaltabel Vision_ID naar NRG_Nr_Behuizing.csv"  , sep = ";", dec=",", colClasses="character", header = TRUE)

setwd(paste0(path,"/2. Baseload GV"))
GV              = read.table("GVBaseloadsaanstations.csv"                  , sep = ",", dec=",", colClasses="character", header = TRUE)
GVprofiletext   = read.table("profielenGV.csv"                             , sep = ",", dec="." , as.is=2,header = TRUE)
setwd(paste0(path,"/2. Baseload GV/2. SAP TESLA"))
load("SAP_TESLA_NHN_proc.Rda")
GVprofiletelmet = saptesla_export

# Klant & Markt scenario's
setwd(paste0(path,"/5. K&M input/EV KV"))
EVpartKV_low      = read.table("20141111_NHN_Scen1.csv"                  , sep = ",", dec="," ,header = TRUE)
EVpartKV_hydro    = read.table("20141111_NHN_Scen4.csv"                  , sep = ",", dec="," , header = TRUE) # This is a hydrogen vehicle scenario
EVpartKV_med      = read.table("20141111_NHN_Scen2.csv"                  , sep = ",", dec="," , header = TRUE)
EVpartKV_high     = read.table("20141111_NHN_Scen3.csv"                  , sep = ",", dec="," , header = TRUE)
EVpartKV_profile  = read.table("EV thuislaadprofiel.csv"                 , sep = ";", dec=",", as.is=1,header = TRUE)

setwd(paste0(path,"/5. K&M input/EV GV"))
EVzakKV_low      = read.table("EVZakelijkKVConvNH.csv"                   , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVzakKV_hydro    = read.table("EVZakelijkKVWaterstofNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE) # This is a hydrogen vehicle scenario
EVzakKV_med      = read.table("EVZakelijkKVElekAandrNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVzakKV_high     = read.table("EVZakelijkKVBattEVNH.csv"                 , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)

EVzakGV_low      = read.table("EVZakelijkGVConvNH.csv"                   , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVzakGV_hydro    = read.table("EVZakelijkGVWaterstofNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE) # This is a hydrogen vehicle scenario
EVzakGV_med      = read.table("EVZakelijkGVElekAandrNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVzakGV_high     = read.table("EVZakelijkGVBattEVNH.csv"                 , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)

EVpark_low      = read.table("EVParkeerConvNH.csv"                       , sep = ";", dec="," , as.is=1,header = TRUE)
EVpark_hydro    = read.table("EVParkeerWaterstofNH.csv"                  , sep = ";", dec="," , as.is=1,header = TRUE) # This is a hydrogen vehicle scenario
EVpark_med      = read.table("EVParkeerElekAandrNH.csv"                  , sep = ";", dec="," , as.is=1,header = TRUE)
EVpark_high     = read.table("EVParkeerBattEVNH.csv"                     , sep = ";", dec="," , as.is=1,header = TRUE)

EVtank_low      = read.table("EVTankstationConvNH.csv"                   , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVtank_hydro    = read.table("EVTankstationWaterstofNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE) # This is a hydrogen vehicle scenario
EVtank_med      = read.table("EVTankstationElekAandrNH.csv"              , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)
EVtank_high     = read.table("EVTankstationBattEVNH.csv"                 , sep = ";", dec="," , as.is=1,numerals="no.loss",header = TRUE)

EVzak_profile    = read.table("EV werklaadprofiel.csv"                   , sep = ";", dec=",", header = TRUE)
EVpark_profile   = read.table("EV parkeerlaadprofiel.csv"                , sep = ";", dec=",", header = TRUE)
EVtank_profile   = read.table("EV snellaadprofiel constant 100kW.csv"    , sep = ";", dec=",", header = TRUE)

setwd(paste0(path,"/5. K&M input/PV KV"))
PV_low     = read.table("castoutNHLow.csv"                               , sep = ",", dec="." , as.is=2, header = TRUE)
PV_med     = read.table("castoutNHMed.csv"                               , sep = ",", dec="." , as.is=2, header = TRUE)
PV_high    = read.table("castoutNHHigh.csv"                              , sep = ",", dec="." , as.is=2, header = TRUE)
PV_profile = read.table("20150109PVprof.csv"                             , sep = ",", dec="." , as.is=2, header = TRUE)

setwd(paste0(path,"/5. K&M input/WP KV"))
WP_low     = read.table("wp laag-2030.csv"                               , sep = ";", dec="," , as.is=2, header = TRUE)
WP_med     = read.table("wp midden-2030.csv"                             , sep = ";", dec="," , as.is=2, header = TRUE)
WP_high    = read.table("wp hoog-2030.csv"                               , sep = ";", dec="," , as.is=2, header = TRUE)
WP_profile = read.table("WP profiel_2Dec_JvdE.csv"                       , sep = ";", dec="," , header = FALSE)

EVpartKV_profile = rep(EVpartKV_profile[seq(1,1439,by=15),2],365) #Repeat profile to obtain a year-profile
EVzak_profile    = rep(EVzak_profile[seq(1,1439,by=15),2],365)    #Repeat profile to obtain a year-profile
EVpark_profile   = rep(EVpark_profile[seq(1,1439,by=15),2],365)   #Repeat profile to obtain a year-profile
EVtank_profile   = rep(EVtank_profile[seq(1,1439,by=15),2],365)   #Repeat profile to obtain a year-profile

print("--Add OS field and OS information to MSR table (2e/6)--")
# Add OS and OS field colums to table 'MSR' for future notice
# We lose 26 MSRs (e.g. 26 MSRs do not have a field and OS defined)
indexlist = match(MSR$MSR,MSRcap$NUMMER_BEH)
MSR       = data.frame(MSR,"OSLD"=MSRcap$ROUTENAAM[indexlist],"OS"=MSRcap$OS_NAAM[indexlist])
MSR$OSLD  = as.character(MSR$OSLD)
MSR$OS    = as.character(MSR$OS)
indexlist = match(GV$netnr,MSRcap$NUMMER_BEH)
GV        = data.frame(GV,"OSLD"=MSRcap$ROUTENAAM[indexlist],"OS"=MSRcap$OS_NAAM[indexlist])
GV$OSLD   = as.character(GV$OSLD)
GV$OS     = as.character(GV$OS)

######################################################### Save results
print("--Saving results (6/6)--")
rm(saptesla_export)
setwd(paste0(path,"/7. Output"))

# Save necessary data
save.image("Data_NH_v2.RData")
print("--Done!--")