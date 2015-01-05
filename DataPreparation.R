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

#set path
path = "C:/1. Programmeerwerk/Bottum Up Analyse/2. Data"

################################################################################ Initialise variables and load data 
print("--Initializing basic variables (1a/6)--")
nyears = 17
nEVscen = 4
nPVscen = 3
nWPscen = 3
nCPUs = 4

print("--Loading data (1b/6)--")
# Asset management network data
setwd(paste0(path,"/1. Baseload KV"))
Users  = read.table("MSR_AANSLUITING.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)#, sep = ";", dec="," ,colClasses = "character", stringsAsFactors=FALSE, header = TRUE)
MSR   = read.table("MSR_AANSLUITING.csv"                          , sep = ",", dec="," ,colClasses = "character", header = TRUE)
EDSN   = read.table("EDSN.csv"                                    , sep = ",", dec="," ,colClasses = "character", header = TRUE)
# KVonb   = read.table("KVonbekendeHLD.csv"                       , sep = "|", dec="," ,colClasses = "character", header = TRUE)
MSRonb  = read.table("KVonbekendeMSR.csv"                         , sep = ",", dec="," ,colClasses = "character", header = TRUE)

setwd(paste0(path,"/4. Kabel en MSR-gegevens"))
HLDcap  = read.table("LS_kabel_bonoka.txt"                             , sep = "|", dec="," ,colClasses = "character", header = TRUE)
MSRcap  = read.table("MSRgegevens_bonoka.csv"                          , sep = ",", dec="," ,colClasses = "character", header = TRUE)
HLDspec = read.table("Match kabeltypes NHN.csv"                        , sep = ";", dec="," ,colClasses = "character", header = TRUE)
Vnames = read.table("Vertaaltabel Vision_ID naar NRG_Nr_Behuizing.csv" , sep = ";", dec="," ,colClasses = "character", header = TRUE)

setwd(paste0(path,"/2. Baseload GV"))
GV        = read.table("GVBaseloadsaanstations.csv"                      , sep = ",", dec="," ,colClasses = "character", header = TRUE)
GVprofile = read.table("profielenGV.csv"                                 , sep = ",", dec="," ,colClasses = "character", header = TRUE)

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
#WP_low     = read.table("Warmtepompen NHN 13-11-2014 min.csv"  , sep = ";", dec="," ,colClasses = "character", header = TRUE)
#WP_med     = read.table("Warmtepompen NHN 13-11-2014 med.csv"  , sep = ";", dec="," ,colClasses = "character", header = TRUE)
#WP_high    = read.table("Warmtepompen NHN 13-11-2014 max.csv"  , sep = ";", dec="," ,colClasses = "character", header = TRUE)
#WP_extr    = read.table("Warmtepompen NHN 13-11-2014 xtr.csv"  , sep = ";", dec="," ,colClasses = "character", header = TRUE)
WP_profile = read.table("WP profiel_2Dec_JvdE.csv"                       , sep = ";", dec="," , header = FALSE)

################################################################################## Clean up data
print("--Cleaning up data (2/6)--")
# Convert most variables to matrices for easy calculation
print("--Converting variables to matrices (2a/6)--")
EVallPC4mat  = cbind(data.matrix(EV_low)[,3:19],data.matrix(EV_hydro)[,3:19],data.matrix(EV_med)[,3:19],data.matrix(EV_high)[,3:19])
PVallmat     = cbind(data.matrix(PV_low)[,6:22],data.matrix(PV_med)[,6:22],data.matrix(PV_high)[,6:22])
WPallmat     = cbind(data.matrix(WP_low)[,2:18],data.matrix(WP_med)[,2:18],data.matrix(WP_high)[,2:18])
EVprofile    = data.matrix(EV_profile)[,2]
PVprofile    = data.matrix(PV_profile)
WPprofile    = data.matrix(WP_profile)
baseprofile  = data.matrix(EDSN)[,4:dim(data.matrix(EDSN))[2]]
PC4          = data.matrix(EV_low)[,2]
WPKMPC6      = WP_low$PC6
PVKMPC6      = PV_low$PC6

#clean up workspace
rm(EV_low, EV_med, EV_high, EV_hydro, PV_low, PV_med, PV_high, WP_low, WP_med, WP_high, EV_profile, PV_profile, WP_profile, EDSN)

# Data cleanup: shorten ARI to PC6
Users$ARI_ADRES = substr(Users$ARI_ADRES, 1, 6)

print("--Cleaning up and indexing yearprofiles (2b/6)--")
# Convert K&M EV, WP and PV profiles to a year-profile with a 15 minute interval, and combine all profiles into "Allprofiles" vector
# PVprofile = PVprofile[-seq(2,52704,by=3)]        #Quick and dirty conversion from 10 minute interval to 15 minute interval: Remove every 2nd and 5th entry
# WPprofile = WPprofile[-seq(2,52704,by=3)]        #Quick and dirty conversion from 10 minute interval to 15 minute interval: Remove every 2nd and 5th entry
# PVprofile = PVprofile[-1:-96]                    #Throw away a day (K&M uses a leap year) (Should be 29th of februari, FIX this next year :)
# WPprofile = WPprofile[-1:-96]                    #Throw away a day (K&M uses a leap year)
EVprofile = rep(EVprofile[seq(1,1439,by=15)],365)#Repeat profile to obtain a year-profile
Allprofiles = as.matrix(data.table(baseprofile,EVprofile,PVprofile,WPprofile))

# Calculate number of profiles per technology 
nprofiles = dim(Allprofiles)[2]
nbaseprofile = max(dim(baseprofile)[2],1)
nEVprofile = max(dim(EVprofile)[2],1)
nPVprofile = max(dim(PVprofile)[2],1)
nWPprofile = max(dim(WPprofile)[2],1)

# Create indices which can be used to address a technology in "Allprofiles" (e.g. EV profiles = Allprofiles[,EVprofileindex])
baseprofileindex = 1:nbaseprofile
EVprofileindex = (nbaseprofile+1):(nbaseprofile+nEVprofile)
PVprofileindex = (nbaseprofile+nEVprofile+1):(nbaseprofile+nEVprofile+nPVprofile)
WPprofileindex = (nbaseprofile+nEVprofile+nWPprofile+1):(nbaseprofile+nEVprofile+nPVprofile+nWPprofile)

print("--Create lists of unique PC6, LSLD, HLD and MSR elements (2c/6)--")
# Create list with unique PC6 codes
PC6     = sort(unique(Users$ARI_ADRES))
# Create list with LSLD codes
LSLD    = sort(unique(Users$LS_KABELS_ID))
LSLD    = substr(LSLD, 1, 9)
# Create list with HLD codes
HLD     = sort(unique(Users$HOOFDLEIDING))
# Create list with MSR codes
MSRlist = unique(c(MSR$MSR,GV$netnr))   # Retrieve MSR numbers
# Create list with OS fields
OSLD    = unique(MSRcap$ROUTENAAM)
# Create list with OS
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

EDSNperPC6 = data.table(Users$ARI_ADRES,Users$PROFIEL_TYPE,SJV+SJVlow)
EDSNperPC6 = group_by(EDSNperPC6, V1,V2)
EDSNperPC6 = summarise(EDSNperPC6, SJV = sum(V3))
EDSNperPC6 = as.matrix(dcast(EDSNperPC6,V1~V2)[3:12])
EDSNperPC6[is.na(EDSNperPC6)]=0

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

#######BRRRRRroken
# ## Account for unconnected KV PC6 peak loads
# SJV2    = as.numeric(sub(",",".",MSRonb$SJV_NORMAAL))                #Yearly electricity use in kWh
# SJVlow2 = as.numeric(sub(",",".",MSRonb$SJV_LAAG))                          #SJVlow is the SJV during the 'daluren'
# SJVlow2[is.na(SJVlow2)] = 0                                           #Remove missing entries
# SJV2[is.na(SJV2)] = 0  
# 
# # Convert EDSN profile in peak values for each EAN
# EDSNindex     = match(MSRonb$PROFIELCATEGORI,EDSNvector) # 12 connections not found
# profiletype   = basepeak[EDSNindex]
# 
# # Calculate base use per PC6 
# baseload2     = data.table(cbind(base,MSRonb$POSTCOD)) #Calculate peak use
# baseload$SJV = as.numeric(baseload$V1)
# basetemp     = baseload[,sum(SJV),by="V2"]                                 #Sum baseloads for each PC6
# basetemp     = basetemp[with(basetemp, order(V2)), ]                       #Sort results
# basetemp[is.na(basetemp)] = 0
# KV_onb_load  = data.matrix(basetemp)[,2]
# PC6onb       = sort(unique(baseload$V2))
# 
# 
# ###############################MISSING MATCH TO EDSN MAX
# 
# #Map the KV_unconnected to the KV PC6
# indexlist    = match(PC6,PC6onb)           #Find translation table
# KV_unc_load  = KV_onb_load[indexlist]      #Create new matrix
# KV_unc_load[is.na(KV_unc_load)] = 0        #Remove NA's (Yet again...)
# base         = KV_unc_load + base
############# Broken until here


############################################################ Create sparse connection matrix
# The goal is to create a connection matrix in the form: 'LSLD_load = connection_matrix * PC6_load' 
# so the calculations become rediciously fast. The connection matrix essentially describes how many
# households of each PC6 are connected to each LSLD.
print("--Create connection matrices (3/6)--")
print("--Filling sparse connection matrix for PC6 to HLD (3a/6)--")

# Define the matrix
#PC6toLSLD = simple_triplet_zero_matrix(nrow = length(LSLD), ncol = length(PC6))
PC6toHLD = simple_triplet_zero_matrix(nrow = length(HLD), ncol = length(PC6))

# Fill the sparse connection matrix (Warning: Complicated code incoming)
progressbar = txtProgressBar(min = 0, max = length(PC6), initial = 0, char = "=", style = 3)

tic()
# foreach (ii=1:50) %dopar%  { #Unfortunately parallel processing of sparse matrices is 
# not trivial. I have to save the results into a list format and then create the sparse matrix... Another time.
for (ii in 1:length(PC6)){ #I have chosen to do the computations column-wise, because it saves computations
   setTxtProgressBar(progressbar,ii)
   # Find how many households of each PC6 are in each LSLD
   indeces  = Users$ARI_ADRES==PC6[ii];     #Find EANs which are part of current PC
   #LSLDs    = Users$LS_KABELS_ID[indeces]  #Retrieve LSLDs of EANs which are part of current PC
   #numLSLD  = table(LSLDs)                 #Count how often each LSLD is present
   HLDs     = Users$HOOFDLEIDING[indeces]   #Retrieve LSLDs of EANs which are part of current PC
   numHLD   = table(HLDs)                   #Count how often each LSLD is present
   
   # Find out in which indeces to store the results
   #LSLDuniq  = unique(LSLDs)               #Find unique LSLDs
   #LSLDindex = which(LSLD %in% LSLDuniq)   #Convert LSLDs into indeces
   HLDuniq  = unique(HLDs)                  #Find unique LSLDs
   HLDindex = which(HLD %in% HLDuniq)       #Convert LSLDs into indeces   
   
   # Save the resutls
   #PC6toLSLD[LSLDindex,ii] = numLSLD/hhPC6[ii]         #Store LSLD count in correct column on correct spot
   PC6toHLD[HLDindex,ii] = numHLD/hhPC6[ii]            #Store HLD count in correct column on correct spot
}
close(progressbar)
toc()

# Create HLD to MSR connection matrix
print("--Filling sparse connection matrix for HLD to MSR (3b/6)--")
# Define the matrix
HLDtoMSRlist= MSR$HOOFDLEIDING              # Retrieve the HLD cables connected to each User

indexlist    = match(HLD,HLDtoMSRlist)      # Search for HLD_IDs in HLD_MSR list
MSRwithHLD  = MSR$MSR[indexlist]            # Get the MSR IDs for the corresponding HLDs
MSRindexlist = match(MSRwithHLD,MSRlist)    # Find the MSR index for each MSR ID
NANlist      = is.na(MSRindexlist)==FALSE   # Remove all NA's (i.e. HLDs which are not matched)

#Create connection matrix
HLDindex     = 1:length(HLD)
HLDNANindex  = HLDindex[NANlist]
i = MSRindexlist[NANlist]
j = HLDNANindex
v = matrix(1,length(HLDNANindex),1)
HLDtoMSR = simple_triplet_matrix(i, j, v, nrow = length(MSRlist), ncol = max(j),dimnames = NULL)

# Create GV to MSR connection matrix
print("--Filling sparse connection matrix for GV to MSR (3c/6)--")
# Define the matrix
GVtoMSRlist  = GV$netnr # Retrieve the GVs connected to each MSR

indexlist    = match(GVtoMSRlist,MSRlist)    # Match the MSR IDs
GVuse        = as.numeric(GV$SJVtot)*4*as.numeric(GV$maxfrac2) #Factor 4 is for the conversion from quarters to hours

i = indexlist
j = 1:length(GVuse)
v = matrix(1,length(GVuse),1)
GVtoMSR = simple_triplet_matrix(i, j, v, nrow = length(MSRlist), ncol = length(GVuse),dimnames = NULL)

# Create MSR TO OS_field connection matrix
print("--Filling sparse connection matrix for MSR to OS Field (3d/6)--")
# Define the matrix
MSRtoOSLDlist  = c(MSR$MSR,GV$netnr)            # Retrieve the MSR connected to each User and GV

indexlist      = match(MSRlist,MSRtoOSLDlist)   # Search for MSRs in MSR_MSR list
OSLDwithMSR    = c(MSR$OSLD,GV$OSLD)[indexlist] # Get the OSLD IDs for the corresponding MSRs
OSLDindexlist  = match(OSLDwithMSR,OSLD)        # Find the OSLD index for each OSLD ID
NANlist        = is.na(OSLDindexlist)==FALSE    # Remove all NA's (i.e. MSRs which are not matched)

#Create connection matrix
MSRindex     = 1:length(MSRlist)
MSRNANindex  = MSRindex[NANlist]
i = OSLDindexlist[NANlist]
j = MSRNANindex
v = matrix(1,length(MSRNANindex),1)
MSRtoOSLD = simple_triplet_matrix(i, j, v, nrow = length(OSLD), ncol = max(j),dimnames = NULL)

# Create OS Field TO OS connection matrix
print("--Filling sparse connection matrix for OS Field to OS (3e/6)--")
# Define the matrix
OSLDtoOSlist   = c(MSR$OSLD,GV$OSLD)          # Retrieve the OSLD connected to each User and GV

indexlist      = match(OSLD,OSLDtoOSlist)     # Search for OSLDs in OSLD_MSR list
OSwithOSLD     = c(MSR$OS,GV$OS)[indexlist]   # Get the OS IDs for the corresponding HLDs
OSindexlist    = match(OSwithOSLD,OS)         # Find the OS index for each OS ID
NANlist        = is.na(OSindexlist)==FALSE    # Remove all NA's (i.e. OSLDs which are not matched)

#Create connection matrix
OSLDindex     = 1:length(OSLD)
OSLDNANindex  = OSLDindex[NANlist]
i = OSindexlist[NANlist]
j = OSLDNANindex
v = matrix(1,length(OSLDNANindex),1)
OSLDtoOS = simple_triplet_matrix(i, j, v, nrow = length(OS), ncol = length(OSLD),dimnames = NULL)

############## Create other useful interconnection matrices from 'base' interconnection matrices
print("--Create other required interconnection matrices (3f/6)--")
# Create interconnection matrix from users to MSR
PC6toMSR  = as.simple_triplet_matrix(matprod_simple_triplet_matrix(HLDtoMSR, PC6toHLD))  
PC6toOSLD = as.simple_triplet_matrix(matprod_simple_triplet_matrix(MSRtoOSLD, PC6toMSR)) 
PC6toOS   = as.simple_triplet_matrix(matprod_simple_triplet_matrix(OSLDtoOS, PC6toOSLD)) 
GVtoOSLD  = as.simple_triplet_matrix(matprod_simple_triplet_matrix(MSRtoOSLD, GVtoMSR)) 
GV6toOS   = as.simple_triplet_matrix(matprod_simple_triplet_matrix(OSLDtoOS, GVtoOSLD)) 
# Interconnection back from MSR to HLD. Equals inverse(HLDtoMSR)
# Because HLDtoMSR is sparse and only has '1' as entry, inverse(HLDtoMSR) = t(HLDtoMSR)
MSRtoHLD  = t(HLDtoMSR) 
# Comment above also holds for interconnection from OSLD to MSR
OSLDtoMSR = t(MSRtoOSLD)

############## Find max capacity for HLD and MSR
print("--Find max capacity for HLD and MSR (3g/6)--")
# Calculate maximum capacity of each HLD
# NOTE: this assumes that the cable with the highest capacity in an LS_HLD gets the maximum load. To be refined based on better net-topological model

#Find the correct HLD type 
indexlist = match(HLDcap$UITVOERING,HLDspec$UITVOERING)  

#Retrieve capacity of each HLD type and set Imax to zero where there is no match
Imax = as.numeric(HLDspec$Min.van.Inom2[indexlist])                                                       
Imax[is.na(Imax)] = 0                     

dfLSLDmax = data.frame(HOOFDLEIDING=HLDcap$HOOFDLEIDING,                           #Create data frame for matching and summarizing.
                       capaciteit=0.4*Imax*(1/sqrt(3)))   
dfHLDmax  = ddply(dfLSLDmax, .(HOOFDLEIDING), summarise, MaxCap = max(capaciteit))  #Find maximum capacity per LS_HLD.                                              
dfHLDlist = data.frame("HOOFDLEIDING"=HLD)                                         #Cast HLD to dataframe for joining                                                              
dfHLDlist = join(dfHLDlist,dfHLDmax,by="HOOFDLEIDING")                             #Join HLDlist with HLDmax                                              
HLDmax    = dfHLDlist$MaxCap                                                                                 
HLDmax[is.na(HLDmax)]=0                                                            #Set is.na's to 0 (from incomplete match between HLDlist and HLDmax)

#cleanup variables
rm(Imax, dfLSLDmax,dfHLDlist)                    

# Match MSR capacity to Unique MSR list
indexlist = match(MSRlist,MSRcap$NUMMER_BEH)
MSRmax    = MSRcap$TOT_TRAFO[indexlist]
MSRmax    = as.numeric(MSRmax)
MSRmax[is.na(MSRmax)] = 0 #Fix the MSR's for which the capacity is unknown

######################################################### Convert EV PC4 scenario to PC6
print("--Extrapolating PC4 EV data to PC6 (4/6)--")
# Data is extrapolated using a weighted average. The actual number of EV's per household is calculated
# by dividing the total number of EV's per PC4 by the total number of households in that PC4. The number
# is then multiplied with the number of households on each PC6 and rounded. EV_PC6 = hh_PC6*(total_EV/hh_PC4)

# Match K&M PC4 to AM PC4 and create AM PC4 EV list
KMPC4 = PC4                                         #Generate KM PC4 list
AMPC4 = sort(unique(substr(Users$ARI_ADRES, 1, 4))) #Generate AM PC4 list
AMPC6 = Users$ARI_ADRES
indeces = match(AMPC4,KMPC4)                  #Find the location of AM PC4 in the KM list
AMEVPC4 = EVallPC4mat[indeces,]               #Transform the EV scenario list to AM PC4

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
EVall       = EV_per_hh_PC4[indexlist,] * hhPC6matrix      #Generate EV per hh on PC6 level. 
                
EVall[is.na(EVall)]=0                                      #Remove NA's caused by missing KM data for certain PC's

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

#Remove NA's
PVall[is.na(PVall)]=0
EVall[is.na(EVall)]=0
WPall[is.na(WPall)]=0

# Clean up variables
rm(PVallmat,WPallmat,WPKMPC6,PVKMPC6)

################################################### Calculating peak time
print("--Generate scenario list (5b/6)--")
#Generate the scenario list in the correct order for easy export
nOSLD      = dim(PC6toOSLD)[1]
nMSR       = dim(PC6toMSR)[1]
nHLD       = dim(PC6toHLD)[1]
nscenarios = nyears*nEVscen*nPVscen*nWPscen

### Initialize matrices which will hold scenario's per PC6 HLD and MSR
tempScenariosperPC6  = matrix(NA, length(PC6),nprofiles)        
ScenariosperHLD      = array(NA, c(nHLD,nprofiles,nscenarios))   #matrix of dimension [nrow = nHLD,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
ScenariosperMSR      = array(NA, c(nMSR,nprofiles,nscenarios))   #matrix of dimension [nrow = nMSR,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]
ScenariosperOSLD     = array(NA, c(nOSLD,nprofiles,nscenarios))   #matrix of dimension [nrow = nOSLD,ncol = nprofiles,narray = nEV*nPV*nWP*nyears]

index = 1
progressbar = txtProgressBar(min = 0, max = nscenarios, initial = 0, char = "=", style = 3)
# Create matrices which hold the SJV and number of EV, PV and WP per HLD and MSR
for (EVii in 1:nEVscen) {
  for(PVii in 1:nPVscen) {
    for(WPii in 1:nWPscen) {
      for(yearii in 1:nyears) {
        setTxtProgressBar(progressbar,index)
        # For a single EV, PV, WP scenario and a single year, create a matrix which has for each PC6
        # the total SJV per EDSN category, and the number of EV, PV and WP in that PC6
        tempScenariosperPC6 = cbind(EDSNperPC6,EVall[,yearii+(EVii-1)*nyears],PVall[,yearii+(PVii-1)*nyears],WPall[,yearii+(WPii-1)*nyears])
        # Apply matrix multiplication to arrive at matrices per HLD and MSR
        ScenariosperHLD[,,index] = matprod_simple_triplet_matrix(PC6toHLD,tempScenariosperPC6)
        ScenariosperMSR[,,index] = matprod_simple_triplet_matrix(PC6toMSR,tempScenariosperPC6)
        ScenariosperOSLD[,,index] = matprod_simple_triplet_matrix(PC6toOSLD,tempScenariosperPC6)
        index = index + 1
        # TODO: Add GV
      }
    }
  }
}
close(progressbar)
rm(tempScenariosperPC6)

######################################################### Save results
print("--Saving results (6/6)--")
setwd(paste0(path,"/7. Output"))

# Save necessary data
save.image("Connections_NH_v2.RData")
print("--Done!--")
