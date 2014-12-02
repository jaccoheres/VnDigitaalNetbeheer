# Hoe goed is elke aansluitcategorie in beeld? ---------------------------------------------------------
CAR    <- fread("CAR_E.csv",colClass="character")                   #inlezen alle EAN's
PC4s   <- read.csv("N:/Bottum Up Analyse/2. Data/0. Gebiedsafbakening/Noord_Holland_PCzonderVelsen.csv",sep=";")
PC6s   <- read.csv("N:/Bottum Up Analyse/2. Data/0. Gebiedsafbakening/Noord_Holland_PC6.csv",sep=";")
GVBCDB <- read.csv("BAR_GIS_MH_NRG_GVB_AANSLUITING.csv",sep=";" ,colClasses=c(EAN_CODE="character"))
EANStoHFD  <- read.csv("N:/Bottum Up Analyse/2. Data/1. Baseload KV/LS_HLD_AANSLUITING.csv",sep=";",dec=",",colClasses=c(EAN="character"))
EANStoMSR  <- read.csv("N:/Bottum Up Analyse/2. Data/1. Baseload KV/MSR_AANSLUITING.csv",colClasses=c(EAN="character"))
MEETPUNTCDB<- read.csv("MEETPUNT_cdb.csv",sep=";",dec=",")
CDBGISMV   <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/CDB.GIS_MV.ssv",sep=";",dec=',')

CAR    <- data.frame(CAR)
GVBCDB <- data.frame(GVBCDB)

sum(CAR$POSTCOD=="")                              # aantal postcodes onbekend
CAR$PC4       <- substr(CAR$POSTCOD,1,4)          #PC4 aanmaken
CARNHN        <- CAR[CAR$PC4 %in% PC4s$Postcode,] #Selectie op Noord Holland Noord
CARNHN$totaal_verbruik <- as.numeric(CARNHN$totaal_verbruik)

#Toevoegen aansluitcategorie
AC1fyscat <- c("1x25","1x35","1x6","3x25")
AC2fyscat <- c("1x50","1x63","1x80","3x35","3x50","3x63","3x80")
CARNHN$AC<- ""
CARNHN$AC[which(CARNHN$FYSIEKE_CAPACITEI %in% AC1fyscat)] <- "AC1"
CARNHN$AC[which(CARNHN$FYSIEKE_CAPACITEI %in% AC2fyscat)] <- "AC2"
CARNHN$AC[which(CARNHN$FYSIEKE_CAPACITEI=="OBK")]         <- "OBK"
CARNHN$CONTRACT_CAPACITEI                                 <-as.numeric(CARNHN$CONTRACT_CAPACITEI )
CARNHN$AC[which(CARNHN$AC=="" & CARNHN$CONTRACT_CAPACITEI<60)]                                        <- "AC3"
CARNHN$AC[which(CARNHN$AC=="" & CARNHN$CONTRACT_CAPACITEI>=60 & CARNHN$CONTRACT_CAPACITEI<160)]       <- "AC4"
CARNHN$AC[which(CARNHN$AC=="" & CARNHN$CONTRACT_CAPACITEI>=160 & CARNHN$CONTRACT_CAPACITEI<2000)]     <- "AC5"
CARNHN$AC[which(CARNHN$AC=="" & CARNHN$CONTRACT_CAPACITEI>=2000 & CARNHN$CONTRACT_CAPACITEI<10000 )]  <- "AC6"
CARNHN$AC[which(CARNHN$AC=="" & CARNHN$CONTRACT_CAPACITEI>=10000)]                                    <- "AC7"
table(CARNHN$AC)

#Hoeveel zitten in set LS aanlsuitingen?
CARinLSHLD <- CARNHN[which(CARNHN$EAN_CODE_AANSLUITING %in% EANStoHFD$EAN),]
table(CARinLSHLD$AC)

#Hoeveel zitten in set GVB CDB?
CARinGVBCDB <- CARNHN[which(CARNHN$EAN_CODE_AANSLUITING %in% GVBCDB$EAN_CODE),]
table(CARinGVBCDB$AC)

#Hoeveel zitten in set Meetpunt CDB?
MEETPUNTCDB$MEETWAARDE_SLEUTEL <- as.character(MEETPUNTCDB$MEETWAARDE_SLEUTEL)
MEETPUNTCDB$giskey             <- as.character(MEETPUNTCDB$giskey)
#names(MEETPUNTCDB)
#table(MEETPUNTCDB$MEETWAARDE_SLEUTEL==MEETPUNTCDB$giskey)
CARinMEETPUNTCDB <- CARNHN[which(CARNHN$EAN_CODE_AANSLUITING %in% MEETPUNTCDB$giskey),]
table(CARinMEETPUNTCDB$AC)

#Alle EANS die zijn teruggevonden
CARTERUG <- rbind(CARinGVBCDB, CARinLSHLD,CARinMEETPUNTCDB)
CARTERUG <- unique(CARTERUG)
CARnietTERUG <- CARNHN[-which(CARNHN$EAN_CODE_AANSLUITING %in% CARTERUG$EAN_CODE_AANSLUITING),]
table(CARTERUG$AC)

ddply(CARNHN,"AC",summarise,SJVAC=sum(totaal_verbruik))
ddply(CARTERUG,"AC",summarise,SJVAC=sum(totaal_verbruik))

#MPOINTS GVBCDB in CDB
names(GVBCDB)
CARinGVBCDB2 <- merge(data.frame(CARinGVBCDB),GVBCDB, by.x=c("EAN_CODE_AANSLUITING"),by.y="EAN_CODE",all.x=T)
sum(GVBCDB$m_point %in% CDBGISMV$M_POINT)
sum(!(GVBCDB$m_point %in% CDBGISMV$M_POINT))

uniekeeans <- unique(CARinGVBCDB2$m_point)
sum(uniekeeans %in% CDBGISMV$M_POINT)
test<-(uniekeeans %in% CDBGISMV$M_POINT)

uniekecombi <- unique(CARinGVBCDB2[,c("EAN_CODE_AANSLUITING","m_point")])
uniekecombi$m_point[which(duplicated(uniekecombi$m_point))]

unique(CARNHN[-which(CARNHN$EAN_CODE_AANSLUITING %in% CARTERUG$EAN_CODE_AANSLUITING),]$PC4) 
LSHLDnietinCARNHN <- EANStoHFD[-which(EANStoHFD$EAN %in% CARNHN$EAN_CODE_AANSLUITING),]

#Wegschrijven databestanden van niet gekoppelde assets
LSHLDnietTERUG   <- CARnietTERUG[which(CARnietTERUG$AC=='AC1'|CARnietTERUG$AC=='AC2'),]
write.csv(          LSHLDnietTERUG,file="N:/Bottum Up Analyse/2. Data/1. Baseload KV/KVonbekendeHLD.csv")
MSRnietTERUG     <- CARNHN[which(!(CARNHN$EAN_CODE_AANSLUITING %in% EANStoMSR$EAN) & !(CARNHN$EAN_CODE_AANSLUITING %in% CARnietTERUG$EAN_CODE_AANSLUITING)
                                 & (CARNHN$AC=='AC1'|CARNHN$AC=='AC2')) ,] #selecteer EANS waarbij geen MSR is teruggevonden, maar wel een HLD
write.csv(          MSRnietTERUG,file="N:/Bottum Up Analyse/2. Data/1. Baseload KV/KVonbekendeMSR.csv")  #wegschrijven EANS waarbij HLD wel, maar MSR niet bekend is

sum(LSHLDnietTERUG$EAN_CODE_AANSLUITING %in% MSRnietTERUG$EAN_CODE_AANSLUITING)