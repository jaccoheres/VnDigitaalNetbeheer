# DataPreparation.R
# This script loads & modifies the AM and K&M data and prepares it for further processing
# If the network data stays the same, the script only has to be executed once.
# The script is therefore NOT optimized for speed (i.e. get coffee AFTER you press Source)
#
# By Werner van Westering MSc.
# Start 28-10-2014
# 
# How this script works:
# 1. Initialize basic variables and load all data from data files
# 2. Perform various clean up operations
# 3. Create a connection matrix in the form: 'LSLD_load = connection_matrix * PC6_load' 
#    so the calculations become rediciously fast
# 4. Convert the PC4 EV scenarios to PC6 EV scenarios
# 5. Match the K&M PC6 to AM PC6 and create scenario matrices (scenario matrices store
#    SJV value and number of EV, PV, WP per PC6 per scenario per year)
# 6. Save results
#
# Note: in this script the impact of EV, PV and WP on the network is quantified on top of the base load.
# As a convention, the following order is ALWAYS used when creating matrices for calculation:
# Total load = c(baseload, EV, PV, WP)
# e.g. the order of the vectors is always [base, EV, PV, WP]
#
# For a detailed description of the abbriviations, see run.R

# Remove all data
rm(list=ls(all=TRUE))
gc(verbose=FALSE)

# Load packages
library(reshape2)
library(plyr)
library(data.table)
library(slam)       #Used for sparse matrices
library(tictoc)     #Because I am a Matlab person
library(dplyr)
library(ff)
library(utils)

#set path
path = "C:/1. Programmeerwerk/Bottum Up Analyse/2. Data"

################################################################################ Initialise variables and load data 

print("--Initializing basic variables (1a/6)--")
startyear = 2014
endyear   = 2030
nEVscen   = 4
nPVscen   = 3
nWPscen   = 3
nCPUs     = 4
nKVtech   = 3     # number of modeled technologies for KV (EV, PV, WP)
nGVtech   = 1     # number of modeled technologies for GV (EV)
# order
baseorder = 1
EVorder   = 2
PVorder   = 3
WPorder   = 4

print("--Loading data (1b/6)--")
setwd(paste0(path,"/7. Output"))
load("Data_NH_v2.RData")

################################################################################## Clean up data
print("--Cleaning up data (2/6)--")
# Convert most variables to matrices for easy calculation
print("--Converting variables to matrices (2a/6)--")
EVpartKVPC4mat= cbind(data.matrix(EVpartKV_low)[,3:19],data.matrix(EVpartKV_hydro)[,3:19],data.matrix(EVpartKV_med)[,3:19],data.matrix(EVpartKV_high)[,3:19])
EVzakKVEANmat = cbind(data.matrix(EVzakKV_low)[,2:18],data.matrix(EVzakKV_hydro)[,2:18],data.matrix(EVzakKV_med)[,2:18],data.matrix(EVzakKV_high)[,2:18])
EVzakGVEANmat = cbind(data.matrix(EVzakGV_low)[,2:18],data.matrix(EVzakGV_hydro)[,2:18],data.matrix(EVzakGV_med)[,2:18],data.matrix(EVzakGV_high)[,2:18])
# EVparkPC6mat  = cbind(data.matrix(EVpark_low)[,2:18],data.matrix(EVpark_hydro)[,2:18],data.matrix(EVpark_med)[,2:18],data.matrix(EVpark_high)[,2:18])
# EVtankEANmat  = cbind(data.matrix(EVtank_low)[,2:18],data.matrix(EVtank_hydro)[,2:18],data.matrix(EVtank_med)[,2:18],data.matrix(EVtank_high)[,2:18])
PVallmat      = cbind(data.matrix(PV_low)[,6:22],data.matrix(PV_med)[,6:22],data.matrix(PV_high)[,6:22])
WPallmat      = cbind(data.matrix(WP_low)[,2:18],data.matrix(WP_med)[,2:18],data.matrix(WP_high)[,2:18])

# Helpful variables related to modeled technologies
PC4           = data.matrix(EVpartKV_low)[,2]
EVzakKVEAN    = EVzakKV_low$EAN
EVzakGVEAN    = EVzakGV_low$EAN
WPKMPC6       = WP_low$PC6
PVKMPC6       = PV_low$PC6

# KV profiles
KVbaseprofile = data.matrix(EDSN)[,4:dim(data.matrix(EDSN))[2]]
KVEVprofile   = data.matrix(cbind(EVpartKV_profile,EVzak_profile))
KVPVprofile   = data.matrix(PV_profile)
KVWPprofile   = data.matrix(WP_profile)
AllKVtechprofiles = as.matrix(data.table(KVEVprofile,KVPVprofile,KVWPprofile))

# define indices how KV profiles are ordered. KV profiles are coupled on either PC6 or EAN
# currently vector AllKVtechprofiles is ordered as
# [PC6, EAN, PC6, PC6] (addressing EVpartKV(PC6),EVzakKV(EAN),KVPV(PC6),KVWP(PC6))
KVPC6index    = c(1,3,4)
KVEANindex    = 2

#GV profiles
GVbaseprofile = matrix(KVbaseprofile[,6],length(KVbaseprofile[,6]),20)     #Fill gaps in GVprofile with the EDSN E3A profile (baseprofile[,6])
GVtemp        = data.matrix(GVprofiletext)[,3:12]                      
GVtemp        = rbind(GVtemp[-1:-288,],GVtemp[97:288,])                      #Sync the days of the week and account for leap year
GVbaseprofile[,c(1,2,4,7,8,10,13,14,19,20)] = GVtemp
GVEVprofile   = data.matrix(cbind(EVzak_profile))
AllGVtechprofiles  = as.matrix(data.table(GVEVprofile))

# Calculate number of profiles per technology and generate indices which can be used to 
# address a technology in "AllKVprofiles" (e.g. EV profiles = AllKVprofiles[,EVprofileindex])
# 1. number of KV profiles
nKVbaseprofile  = max(dim(KVbaseprofile)[2],1)
nKVtechprofiles = max(dim(AllKVtechprofiles)[2],1)
nKVprofiles     = nKVbaseprofile + nKVtechprofiles
nKVEVprofile    = max(dim(KVEVprofile)[2],1)
nKVPVprofile    = max(dim(KVPVprofile)[2],1)
nKVWPprofile    = max(dim(KVWPprofile)[2],1)
# 2. KV base indices
KVbaseindex     = 1:nKVbaseprofile
# 3. KV tech indices
KVEVindex       = 1:nKVEVprofile
KVPVindex       = (nKVEVprofile+1):(nKVEVprofile+nKVPVprofile)
KVWPindex       = (nKVEVprofile+nKVPVprofile+1):(nKVEVprofile+nKVPVprofile+nKVWPprofile)

# 4. number of GV profiles
nGVbaseprofile  = max(dim(GVbaseprofile)[2],1)
nGVtechprofiles = max(dim(AllGVtechprofiles)[2],1)
nGVprofiles     = nGVbaseprofile + nGVtechprofiles
nGVEVprofile    = max(dim(GVEVprofile)[2],1)
# 5. GV base indices
GVbaseindex     = 1:nGVbaseprofile
# 6. GV tech indices
GVEVindex       = 1:nGVEVprofile

#clean up workspace
# fix after fixing naming conventions
# rm(EV_low, EV_med, EV_high, EV_hydro, PV_low, PV_med, PV_high, WP_low, WP_med, WP_high, EVpartKV_profile, EVzak_profile, EVpark_profile, EVtank_profile, PV_profile, WP_profile, EDSN)

# Data cleanup: shorten ARI to PC6
Users$ARI_ADRES = substr(Users$ARI_ADRES, 1, 6)
MSR$ARI_ADRES = substr(MSR$ARI_ADRES, 1, 6)

print("--Cleaning up and indexing yearprofiles (2b/6)--")
# Convert K&M EV, WP and PV profiles to a year-profile with a 15 minute interval, and combine all profiles into "AllKVprofiles" vector                

print("--Create unique lists of assets and PC6 (2c/6)--")
KVEAN   = unique(MSR$EAN)
GVEAN   = unique(GV$EAN)
PC6     = sort(unique(MSR$ARI_ADRES))
LSLD    = sort(unique(Users$LS_KABELS_ID))
LSLD    = substr(LSLD, 1, 9)
HLD     = sort(unique(Users$HOOFDLEIDING))
MSRlist = unique(c(MSR$MSR,GV$netnr))   # Retrieve MSR numbers
OSLD    = unique(MSRcap$ROUTENAAM)
OS      = unique(MSRcap$OS_NAAM)
# Calculate number of households per PC6
hhPC6   = table(Users$ARI_ADRES)        

############################################################################# Calculating base loads
print("--Calculating baseload (2d/6)--")
## Calculate the PC6 baseload peaks, SJV values are in kW/quarter hour, so they should be multiplied by four to obtain kWh 
SJV    = as.numeric(sub(",",".",Users$STANDAARD_JAARVERBRUIK))      *4 #Yearly electricity use in kWh
SJVlow = as.numeric(sub(",",".",Users$STANDAARD_JAARVERBRUIK_LAAG)) *4 #SJVlow is the SJV during the 'daluren'
SJVlow[is.na(SJVlow)] = 0                                              #Remove missing entries
SJV[is.na(SJV)] = 0                                                    #Remove missing entries



## Account for unconnected KV PC6 peak loads
SJV2    = as.numeric(sub(",",".",MSRonb$SJV_NORMAAL))                #Yearly electricity use in kWh
SJVlow2 = as.numeric(sub(",",".",MSRonb$SJV_LAAG))                          #SJVlow is the SJV during the 'daluren'
SJVlow2[is.na(SJVlow2)] = 0                                           #Remove missing entries
SJV2[is.na(SJV2)] = 0  
SJVKVonb = SJVlow2+SJV2
PC6onb = MSRonb$POSTCOD

# Convert EDSN profile in peak values for each EAN
EDSNvector = c('E1A','E1B','E1C','E2A','E2B','E3A','E3B','E3C','E3D','E4A')
EDSNindex     = match(MSRonb$PROFIELCATEGORI,EDSNvector) # 12 connections not found
EDSNindex(is.na(EDSNindex)) = 1

#SJVKVonb = SJV of uncoupled KV
#EDSNindex = EDSN-profile-index
#PC6onb = PC6 location of EAN

####Save results


############################################################ Create sparse connection matrix
# The goal is to create a connection matrix in the form: 'LSLD_load = connection_matrix * PC6_load' 
# so the calculations become rediciously fast. The connection matrix essentially describes how many
# households of each PC6 are connected to each LSLD.
print("--Create connection matrices (3/6)--")

CreateInterconnectionMatrix <- function(FromTo,FromReferenceList,ToReferenceList,makeUnique) {
   #INPUT: 
   #FromTo            = elements which will be coupled (example c(MSR$HOOFDLEIDING,MSR$MSR))
   #FromReferenceList = unique list of From-elements (example: variable 'HLD')
   #ToReferenceList   = unique list of To-elements (example: variable 'MSRlist')
   
   if(makeUnique) {FromTo = unique(FromTo)}
   #define indices
   Fromindexlist = match(FromTo[,1],FromReferenceList)
   Toindexlist   = match(FromTo[,2],ToReferenceList)     # Find the MSR index for each MSR ID
   NANlist       = is.na(Toindexlist)==FALSE
   
   #Create connection matrix
   i             = Toindexlist[NANlist]
   j             = Fromindexlist[NANlist]
   v_weights     = table(Fromindexlist[NANlist])
   v             = rep(1/v_weights,v_weights)
   if(makeUnique) {ncols = length(FromReferenceList)} else {ncols = length(FromTo[,1])}
   Outputmatrix = simple_triplet_matrix(i, j, v, nrow = length(ToReferenceList), ncols, dimnames = NULL) 
   return(Outputmatrix)
}

print("--> Build up connection matrices (3a/6)--")
# KVEAN to asset 
KVEANtoPC6  = CreateInterconnectionMatrix(MSR[c("EAN","ARI_ADRES")],KVEAN,PC6,FALSE)      #Only use this for missing KV baseload. Couple EAN directly to asset using KVEANtoHLD
KVEANtoHLD  = CreateInterconnectionMatrix(MSR[c("EAN","HOOFDLEIDING")],KVEAN,HLD,FALSE)
KVEANtoMSR  = CreateInterconnectionMatrix(MSR[c("EAN","MSR")],KVEAN,MSRlist,FALSE)
KVEANtoOSLD = CreateInterconnectionMatrix(MSR[c("EAN","OSLD")],KVEAN,OSLD,FALSE)
KVEANtoOS   = CreateInterconnectionMatrix(MSR[c("EAN","OS")],KVEAN,OS,FALSE)

# GVEAN to asset 
GVEANtoMSR  = CreateInterconnectionMatrix(GV[c("EAN_CODE","netnr")],GVEAN,MSRlist,FALSE)
GVEANtoOSLD = CreateInterconnectionMatrix(GV[c("EAN_CODE","OSLD")],GVEAN,OSLD,FALSE)
GVEANtoOS   = CreateInterconnectionMatrix(GV[c("EAN_CODE","OS")],GVEAN,OS,FALSE)

# PC6 to asset
PC6toHLD   = CreateInterconnectionMatrix(MSR[c("ARI_ADRES","HOOFDLEIDING")],PC6,HLD,TRUE)
PC6toMSR   = CreateInterconnectionMatrix(MSR[c("ARI_ADRES","MSR")],PC6,MSRlist,TRUE)
PC6toOSLD  = CreateInterconnectionMatrix(MSR[c("ARI_ADRES","OSLD")],PC6,OSLD,TRUE)
PC6toOS    = CreateInterconnectionMatrix(MSR[c("ARI_ADRES","OS")],PC6,OS,TRUE)


# Asset to asset
HLDtoMSR = CreateInterconnectionMatrix(MSR[c("HOOFDLEIDING","MSR")],HLD,MSRlist,TRUE)
MSRtoOSLD = CreateInterconnectionMatrix(cbind(c(MSR$MSR,GV$netnr),c(MSR$OSLD,GV$OSLD)),MSRlist,OSLD,TRUE)
OSLDtoOS = CreateInterconnectionMatrix(cbind(c(MSR$OSLD,GV$OSLD),c(MSR$OS,GV$OS)),OSLD,OS,TRUE)

############## Create other useful interconnection matrices from 'base' interconnection matrices
print("--Create other required interconnection matrices from 'base' (3b/6)--") 
# Interconnection back from MSR to HLD. Equals inverse(HLDtoMSR)
dupl = !duplicated(HLDtoMSR$j)
j_new = (c(HLDtoMSR$i[dupl],HLDtoMSR$i[!dupl]))
i_new = 1:length(j_new)
nMSR = dim(HLDtoMSR)[2]
MSRtoHLD=simple_triplet_matrix(i_new,j_new,rep(1,length(i_new)),nrow = length(i_new),ncol = nMSR)

# Because OSLDtoMSR is sparse and only has '1' as entry, inverse(OSLDtoMSR) = t(OSLDtoMSR)
OSLDtoMSR = t(MSRtoOSLD)

############## Find max capacity for HLD and MSR
print("--Find max capacity for HLD and MSR and HLD length (3g/6)--")
# Calculate maximum capacity and length of each HLD
# NOTE: this assumes that the cable with the highest capacity in an LS_HLD gets the maximum load. To be refined based on better net-topological model

#Find the correct HLD type 
indexlist = match(HLDcap$UITVOERING,HLDspec$UITVOERING)  

#Retrieve capacity of each HLD type and set Imax to zero where there is no match
Imax = as.numeric(HLDspec$Min.van.Inom2[indexlist])                                                       
Imax[is.na(Imax)] = 0                     

dfLSLDmax = data.frame(HOOFDLEIDING = HLDcap$HOOFDLEIDING,        # Create data frame for matching and summarizing.
                       LENGTE       = HLDcap$SYSTEEM_LENGTE,
                       capaciteit   = 0.4*Imax*(1/sqrt(3)))   
dfHLDmax  = ddply(dfLSLDmax, .(HOOFDLEIDING), summarise,          # Find maximum capacity and total cable length
                  MaxCap = max(capaciteit),                       # per LS_HLD
                  length = sum(LENGTE))                                                                                            
dfHLDlist = data.frame("HOOFDLEIDING"=HLD)                        # Cast HLD to dataframe for joining                                                              
dfHLDlist = join(dfHLDlist,dfHLDmax,by="HOOFDLEIDING")            # Join HLDlist with HLDmax                                              
HLDmax    = dfHLDlist$MaxCap
HLDlength = dfHLDlist$length
HLDmax[is.na(HLDmax)]       = 0                                   # Set is.na's to 0 (from incomplete match between HLDlist and HLDmax)
HLDlength[is.na(HLDlength)] = 0                                   # Set is.na's to 0 (from incomplete match between HLDlist and HLDmax)


#cleanup variables
rm(Imax, dfLSLDmax,dfHLDlist)                    

# Match MSR capacity to Unique MSR list
indexlist = match(MSRlist,MSRcap$NUMMER_BEH)
MSRmax    = MSRcap$TOT_TRAFO[indexlist]
MSRmax    = as.numeric(MSRmax)
MSRmax[is.na(MSRmax)] = 0 #Fix the MSR's for which the capacity is unknown

######################################################### Convert EV PC4 scenario to PC6
print("--Ordering EV KV data (4/6)--")
print("--Extrapolating PC4 EV data to PC6 (4a/6")
# Data is extrapolated using a weighted average. The actual number of EV's per household is calculated
# by dividing the total number of EV's per PC4 by the total number of households in that PC4. The number
# is then multiplied with the number of households on each PC6 and rounded. EV_PC6 = hh_PC6*(total_EV/hh_PC4)

# Match K&M PC4 to AM PC4 and create AM PC4 EV list
KMPC4 = PC4                                         #Generate KM PC4 list
AMPC4 = sort(unique(substr(Users$ARI_ADRES, 1, 4))) #Generate AM PC4 list
AMPC6 = Users$ARI_ADRES
indeces = match(AMPC4,KMPC4)                  #Find the location of AM PC4 in the KM list
AMEVPC4 = EVpartKVPC4mat[indeces,]            #Transform the EV scenario list to AM PC4

# Calculate number of households per PC for the weighted average
hhPC6 = table(Users$ARI_ADRES)                #Calculate number of households per PC6
hhPC4 = table(substr(Users$ARI_ADRES, 1, 4))  #Calculate number of households per PC4

# Transform the PC4 list into a PC6 list with a weighted average
# This be t' not a proper matrix manipulation, but it works. ARRRRRrrrrrrr!
hhPC4matrix = matrix(data = rep(hhPC4, times = dim(AMEVPC4)[2]), nrow = dim(AMEVPC4)[1] ,ncol = dim(AMEVPC4)[2])
EV_per_hh_PC4 = AMEVPC4/hhPC4matrix                      #Calculate the EV/hh on each PC4

# Create final matrix
indexlist   = match((substr(PC6, 1, 4)),AMPC4)             #find indeces for transformation PC4 => PC6
hhPC6matrix = matrix(rep(hhPC6, times = dim(AMEVPC4)[2]), nrow = length(hhPC6) ,ncol = dim(AMEVPC4)[2])
EVpartKV    = EV_per_hh_PC4[indexlist,] * hhPC6matrix      #Generate EV per hh on PC6 level. 

EVpartKV[is.na(EVpartKV)]=0                                      #Remove NA's caused by missing KM data for certain PC's

# Clean up variables
rm(EVallPC4mat,AMEVPC4,EV_per_hh_PC4)

######################################################### Match K&M PC6 to AM
print("--Converting K&M PC6 scenarios to AM PC6 and clean up (5a/6)--")
# PV
indexlist = match(PC6,PVKMPC6)            #Find translation table
PVall     = PVallmat[indexlist,]          #Create new matrix

# WP
indexlist = match(PC6,WPKMPC6)            #Find translation table
WPall     = WPallmat[indexlist,]          #Create new matrix

#EV KVzak
indexlist  = match(Users$EAN,EVzakKVEAN)         #Find translation table
EVzakKV    = EVzakKVEANmat[indexlist,]     #Create new matrix

#EV GVzak
indexlist  = match(GV$EAN,EVzakGVEAN)         #Find translation table
EVzakGV    = EVzakGVEANmat[indexlist,]     #Create new matrix

#Remove NA's
PVall[is.na(PVall)]=0
WPall[is.na(WPall)]=0
EVzakKV[is.na(EVzakKV)]=0
EVzakGV[is.na(EVzakGV)]=0

# Clean up variables
rm(PVallmat,WPallmat,WPKMPC6,PVKMPC6)

################################################### Calculating peak time
print("--Generate scenario list (5b/6)--")
#Generate the scenario list in the correct order for easy export
nOSLD            = dim(PC6toOSLD)[1]
nMSR             = dim(PC6toMSR)[1]
nHLD             = dim(PC6toHLD)[1]
nyears           = endyear - (startyear-1)
nscenarios       = nyears*nEVscen*nPVscen*nWPscen
scenarionamelist = c("Low","Med","High","Extr")

### Initialize matrices which will hold scenario's per PC6 HLD and MSR
tempKVScenariosperPC6  = c()
tempKVScenariosperEAN  = c()
tempGVScenariosperEAN  = c()
KVScenariosperHLD      = array(NA, c(nHLD,4,nscenarios))   #matrix of dimension [nrow = nHLD ,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
KVScenariosperMSR      = array(NA, c(nMSR,4,nscenarios))   #matrix of dimension [nrow = nMSR ,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
KVScenariosperOSLD     = array(NA, c(nOSLD,4,nscenarios))  #matrix of dimension [nrow = nOSLD,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
GVScenariosperMSR      = array(NA, c(nMSR,nGVtechprofiles,nscenarios))   #matrix of dimension [nrow = nMSR ,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
GVScenariosperOSLD     = array(NA, c(nOSLD,nGVtechprofiles,nscenarios))  #matrix of dimension [nrow = nOSLD,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
scenarionumber         = c()

KVEANindexvect          = 2
KVPC6indexvect          = c(1,3,4)





################# WORK IN PROGRESS

#Calculate profiles per MSR
progressbar = txtProgressBar(min = 0, max = nscenarios, initial = 0, char = "=", style = 3)
index =1
##Create scenario list
# Create matrices which hold the SJV and number of EV, PV and WP per HLD and MSR
for (EVii in 1:nEVscen) {
   for(PVii in 1:nPVscen) {
      for(WPii in 1:nWPscen) {     
         for(yearii in 1:nyears) {
            setTxtProgressBar(progressbar,index)
            # For a single EV, PV, WP scenario and a single year, create a matrix which has for each PC6
            # the total SJV per EDSN category, and the number of EV, PV and WP in that PC6
            
            # KV Scenarios
            tempKVScenariosperEAN = cbind(EVzakKV[,yearii+(EVii-1)*nyears])
            tempKVScenariosperPC6 = cbind(EVpartKV[,yearii+(EVii-1)*nyears],PVall[,yearii+(PVii-1)*nyears],WPall[,yearii+(WPii-1)*nyears])
            # Apply matrix multiplication to arrive at matrices per HLD and MSR
            KVScenariosperHLD[,KVPC6indexvect,index] = matprod_simple_triplet_matrix(PC6toHLD,tempKVScenariosperPC6)
            KVScenariosperHLD[,KVEANindexvect,index] = matprod_simple_triplet_matrix(KVEANtoHLD,tempKVScenariosperEAN)
            KVScenariosperMSR[,KVPC6indexvect,index] = matprod_simple_triplet_matrix(PC6toMSR,tempKVScenariosperPC6)
            KVScenariosperMSR[,KVEANindexvect,index] = matprod_simple_triplet_matrix(KVEANtoMSR,tempKVScenariosperEAN)
            KVScenariosperOSLD[,KVPC6indexvect,index] = matprod_simple_triplet_matrix(PC6toOSLD,tempKVScenariosperPC6)
            KVScenariosperOSLD[,KVEANindexvect,index] = matprod_simple_triplet_matrix(KVEANtoOSLD,tempKVScenariosperEAN)
            
            # GV Scenarios
            tempGVScenariosperEAN = cbind(EVzakGV[,yearii+(EVii-1)*nyears])
            GVScenariosperMSR[,,index] = matprod_simple_triplet_matrix(GVEANtoMSR,tempGVScenariosperEAN)
            GVScenariosperOSLD[,,index] = matprod_simple_triplet_matrix(GVEANtoOSLD,tempGVScenariosperEAN)
            
            index = index + 1
         } 
         #Scenarionumber: indexnumber, PV scenario, EV scenario, WP scenario, 
         scenarionumber = rbind(scenarionumber,c((index-1)/nyears,EVii,PVii,WPii))      
      }
   }
}
close(progressbar)

# Generate KV baseload
EDSNperEAN = matrix(0,length(SJV),length(colnames(KVbaseprofile)))
profilenumber = match(Users$PROFIEL_TYPE,colnames(KVbaseprofile))
for (i in 1:length(SJV)) {
   EDSNperEAN[i,profilenumber[i]]=SJV[i]+SJVlow[i]
}
EDSNperHLD = t(matprod_simple_triplet_matrix(KVEANtoHLD, EDSNperEAN))
EDSNperMSR = t(matprod_simple_triplet_matrix(KVEANtoMSR, EDSNperEAN))
EDSNperOSLD = t(matprod_simple_triplet_matrix(KVEANtoOSLD, EDSNperEAN))

KVbaseloadperHLD  = matrix(NA,nHLD,365*24*4) #This can be done but not on an 8RAM computer
KVbaseloadperMSR  = matrix(NA,nMSR,365*24*4)
KVbaseloadperOSLD = matrix(NA,nOSLD,365*24*4)

for (i in 1:nHLD) {KVbaseloadperHLD[i,] = KVbaseprofile %*% EDSNperHLD[,i]} 
for (i in 1:nMSR) {KVbaseloadperMSR[i,] = KVbaseprofile %*% EDSNperMSR[,i]}
for (i in 1:nOSLD) {KVbaseloadperOSLD[i,] = KVbaseprofile %*% EDSNperOSLD[,i]}

# Generate GV baseload
print("--Generate GV baseload (5c/6) (be patient)--")
KVKnumber = as.numeric(GV$KVKSEGMENT)
KVKnumber[is.na(KVKnumber)] = 3
GV_SJV = as.numeric(GV$SJVtot)
GV_SJV[is.na(GV_SJV)]=0
GVuse        = (t(GVbaseprofile[,KVKnumber])*GV_SJV*4)
GVbaseloadperMSR = (matprod_simple_triplet_matrix(GVEANtoMSR, (GVuse)))  #Possible speed-up
GVbaseloadperOSLD = (matprod_simple_triplet_matrix(GVEANtoOSLD, (GVuse)))

# Calculate total base load
gc(verbose=FALSE)
baseloadperHLD = KVbaseloadperHLD 
baseloadperMSR = KVbaseloadperMSR + GVbaseloadperMSR
baseloadperOSLD = KVbaseloadperOSLD + GVbaseloadperOSLD

rm(KVbaseloadperHLD,KVbaseloadperMSR,GVbaseloadperMSR,KVbaseloadperOSLD,GVbaseloadperOSLD)


######################################################### Save results
print("--Saving results (6/6)--")
setwd(paste0(path,"/7. Output"))

# Save necessary data
save.image("Connections_NH_v2.RData")
print("--Done!--")



# For reference, original network interconnection script
#
# # Define the matrix
# #PC6toLSLD = simple_triplet_zero_matrix(nrow = length(LSLD), ncol = length(PC6))
# PC6toHLD = simple_triplet_zero_matrix(nrow = length(HLD), ncol = length(PC6))
# 
# # Fill the sparse connection matrix (Warning: Complicated code incoming)
# progressbar = txtProgressBar(min = 0, max = length(PC6), initial = 0, char = "=", style = 3)
# 
# tic()
# # foreach (ii=1:50) %dopar%  { #Unfortunately parallel processing of sparse matrices is 
# # not trivial. I have to save the results into a list format and then create the sparse matrix... Another time.
# for (ii in 1:length(PC6){ #I have chosen to do the computations column-wise, because it saves computations
#    setTxtProgressBar(progressbar,ii)
#    # Find how many households of each PC6 are in each LSLD
#    indeces  = Users$ARI_ADRES==PC6[ii];     #Find EANs which are part of current PC
#    #LSLDs    = Users$LS_KABELS_ID[indeces]  #Retrieve LSLDs of EANs which are part of current PC
#    #numLSLD  = table(LSLDs)                 #Count how often each LSLD is present
#    HLDs     = Users$HOOFDLEIDING[indeces]   #Retrieve LSLDs of EANs which are part of current PC
#    numHLD   = table(HLDs)                   #Count how often each LSLD is present
#    
#    # Find out in which indeces to store the results
#    #LSLDuniq  = unique(LSLDs)               #Find unique LSLDs
#    #LSLDindex = which(LSLD %in% LSLDuniq)   #Convert LSLDs into indeces
#    HLDuniq  = unique(HLDs)                  #Find unique LSLDs
#    HLDindex = which(HLD %in% HLDuniq)       #Convert LSLDs into indeces   
#    
#    # Save the resutls
#    #PC6toLSLD[LSLDindex,ii] = numLSLD/hhPC6[ii]         #Store LSLD count in correct column on correct spot
#    PC6toHLD[HLDindex,ii] = numHLD/hhPC6[ii]            #Store HLD count in correct column on correct spot
# }
# close(progressbar)
# toc()
#
# # Define the matrix
# HLDtoMSRlist= MSR$HOOFDLEIDING              # Retrieve the HLD cables connected to each User
# 
# indexlist    = match(HLD,HLDtoMSRlist)      # Search for HLD_IDs in HLD_MSR list
# MSRwithHLD   = MSR$MSR[indexlist]            # Get the MSR IDs for the corresponding HLDs
# MSRindexlist = match(MSRwithHLD,MSRlist)    # Find the MSR index for each MSR ID
# NANlist      = is.na(MSRindexlist)==FALSE   # Remove all NA's (i.e. HLDs which are not matched)
# 
# #Create connection matrix
# HLDindex     = 1:length(HLD)
# HLDNANindex  = HLDindex[NANlist]
# i            = MSRindexlist[NANlist]
# j            = HLDNANindex
# v            = matrix(1,length(HLDNANindex),1)
# HLDtoMSR     = simple_triplet_matrix(i, j, v, nrow = length(MSRlist), ncol = max(j),dimnames = NULL)  
#
# 
# # Create GV to MSR connection matrix
# print("--Filling sparse connection matrix for GV to MSR (3c/6)--")
# # Define the matrix
# GVtoMSRlist  = GV$netnr # Retrieve the GVs connected to each MSR
# 
# indexlist    = match(GVtoMSRlist,MSRlist)    # Match the MSR IDs
# # GVuse        = as.numeric(GV$SJVtot)*4*as.numeric(GV$maxfrac2) #Factor 4 is for the conversion from quarters to hours
# 
# i = indexlist
# j = 1:length(GV$netnr)
# v = matrix(1,length(GV$netnr),1)
# GVtoMSR = simple_triplet_matrix(i, j, v, nrow = length(MSRlist), ncol = length(GV$netnr),dimnames = NULL)
# 
# 
# # Create MSR TO OS_field connection matrix
# print("--Filling sparse connection matrix for MSR to OS Field (3d/6)--")
# # Define the matrix
# MSRtoOSLDlist  = c(MSR$MSR,GV$netnr)            # Retrieve the MSR connected to each User and GV
# 
# indexlist      = match(MSRlist,MSRtoOSLDlist)   # Search for MSRs in MSR_MSR list
# OSLDwithMSR    = c(MSR$OSLD,GV$OSLD)[indexlist] # Get the OSLD IDs for the corresponding MSRs
# OSLDindexlist  = match(OSLDwithMSR,OSLD)        # Find the OSLD index for each OSLD ID
# NANlist        = is.na(OSLDindexlist)==FALSE    # Remove all NA's (i.e. MSRs which are not matched)
# 
# #Create connection matrix
# MSRindex     = 1:length(MSRlist)
# MSRNANindex  = MSRindex[NANlist]
# i            = OSLDindexlist[NANlist]
# j            = MSRNANindex
# v            = matrix(1,length(MSRNANindex),1)
# MSRtoOSLD    = simple_triplet_matrix(i, j, v, nrow = length(OSLD), ncol = max(j),dimnames = NULL)
# 
# # Create OS Field TO OS connection matrix
# print("--Filling sparse connection matrix for OS Field to OS (3e/6)--")
# # Define the matrix
# OSLDtoOSlist   = c(MSR$OSLD,GV$OSLD)          # Retrieve the OSLD connected to each User and GV
# 
# indexlist      = match(OSLD,OSLDtoOSlist)     # Search for OSLDs in OSLD_MSR list
# OSwithOSLD     = c(MSR$OS,GV$OS)[indexlist]   # Get the OS IDs for the corresponding HLDs
# OSindexlist    = match(OSwithOSLD,OS)         # Find the OS index for each OS ID
# NANlist        = is.na(OSindexlist)==FALSE    # Remove all NA's (i.e. OSLDs which are not matched)
# 
# #Create connection matrix
# OSLDindex     = 1:length(OSLD)
# OSLDNANindex  = OSLDindex[NANlist]
# i             = OSindexlist[NANlist]
# j             = OSLDNANindex
# v             = matrix(1,length(OSLDNANindex),1)
# OSLDtoOS      = simple_triplet_matrix(i, j, v, nrow = length(OS), ncol = length(OSLD),dimnames = NULL)
# 
######################################################################################################
#
# For reference, a parrallel processing script per scenario
# 
# 
# #Define function for peak time computation
# GetPeaktimes <- function(index,profiles,scenario) {
#    #GetPeaktimes returns the peak times per MSR in the network
#    #
#    #GetPeaktimes(Scenario,AllKVprofiles)
#    #
#    #Need far more RAM to program it this way
#    #    for(jj in 1:nprofiles) {
#    #       times     = profiles %*% scenario  #FIX
#    #       peaktimes = max.col(times)
#    #       MSRtime = scenarios[,peaktimes]
#    #    }
#    #    return(MSRtime)
#    
#    #Initialize
#    setTxtProgressBar(progressbar,index)
#    nAssets  = dim(scenario)[1]
#    peaktime = matrix(nrow = nAssets,ncol=2*nscenarios)
#    
#    #Calculate peak times per Asset (If you have more RAM, you can vectorize it quite easily and win a lot of speed)
#    for(Assetii in 1:nAssets) {
#       Assettotalprofile                  = AllKVprofiles %*% scenario[Assetii,]
#       peaktimemax[Assetii]               = which.max(Assettotalprofile)         #peaktime
#       peaktimemin[Assetii]               = which.min(Assettotalprofile)         #peaktimemin
#       
#       Assetloadmax[Assetii] = AllKVprofiles[peaktimemax,] * scenario[Assetii,]
#       Assetloadmin[Assetii] = AllKVprofiles[peaktimemin,] * scenario[Assetii,]
#    }  
#    
#    outputmatrix = matrix(c(peaktimemax,peaktimemin,Assetloadmax,Assetloadmin),nAssets,4)
#    
# #    #Determine peak loads (start of vectorization)
# #    MSRloadmax = AllKVprofiles[peaktimemax,] * scenario
# #    MSRloadmin = AllKVprofiles[peaktimemin,] * scenario
#    return(outputmatrix)
# }
# 
# ##Do parallel computations
# #Initialize
# progressbar = txtProgressBar(min = 0, max = nscenarios, initial = 0, char = "=", style = 3)
# cl<-makeCluster(nCPUs) 
# registerDoSNOW(cl)
# tic()
# # #Repeat profiles for fast and easy multiplication (ARRRRRRrrr)
# # AllKVprofilesrepeat = array(AllKVprofiles,c(dim(ScenariosperMSR)[1],1,dim(AllKVprofiles)[1]))
# 
# #Calculate peak times
# MSRtimes = foreach(ii = 1:nscenarios,.packages='slam',.combine=cbind,.verbose=FALSE) %dopar% {
#    GetPeaktimes(ii,AllKVprofiles,ScenariosperMSR[,,ii])}
# #Close
# toc()
# stopCluster(cl)
