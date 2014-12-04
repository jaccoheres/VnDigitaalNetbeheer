library(plyr)
#Inlezen Gisdata en meetdata
if( !exists("GVBCDB"))  {
    GVBCDB       <- read.csv("N:/Bottum Up Analyse/2. Data/3. Nettopologie/BAR_GIS_MH_NRG_GVB_AANSLUITING.csv",sep=";"
                             ,colClasses=c(EAN_CODE="character"))
}

if( !exists("CDBGISMV")){
  CDBGISMV       <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/CDB.GIS_MV.ssv",sep=";",dec=',')
}
#Inlezen koppelingen met KvK segmenten
RELNRtoGPART       <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/BAR_GIS_MH_NRG_GVB_AANSLUITING_ZPs.csv",sep=";")
GPARTtoKVKSEGMENT  <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/BAR_GIS_MH_NRG_GVB_AANSLUITING_ZPs_Segment.csv")

#Inlezen CAR
CARNHNGV           <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/3. GV Lijsten/CARNHNGV.csv",colClasses=c(EAN_CODE_AANSLUITING="character"))
#Inladen KvK-profielen en EDSN-profielen
profielen          <- read.csv("N:/Bottum Up Analyse/2. Data/2. Baseload GV/1. Profielen zonder telemetrie/profielenGV.csv")

profielenEDSN      <- read.csv("N:/Bottum Up Analyse/2. Data/1. Baseload KV/profielen Elektriciteit 2014 versie 1.00.csv",header=F,skip=6)
names(profielenEDSN) <- c("tijd","tijdvan","tijdtot","E1A","E1B","E1C","E2A","E2B","E3A","E3B","E3C","E3D","E4A")


#inlezen MSR-ruimtes
MSRs               <- read.csv("N:/Bottum Up Analyse/2. Data/4. Kabel en MSR-gegevens/MSRgegevens_bonoka.csv")
nrow(unique(GPARTtoKVKSEGMENT))
nrow(unique(RELNRtoGPART))
length(unique(GVBCDB$RELATIENUMMER))
length(unique(RELNRtoGPART$RELATIENUMMER))


#koppelen gpart aan GVBCDB
GVBCDB                      <- GVBCDB[-which(duplicated(GVBCDB[,c("EAN_CODE","m_point","netnr")])),]
GVBCDB$RELATIENUMMER        <- as.numeric(sub(",00","", GVBCDB$RELATIENUMMER))
RELNRtoGPART                <- unique(RELNRtoGPART)
nrow(unique(GPARTtoKVKSEGMENT))
GVBCDB                      <- merge(GVBCDB,RELNRtoGPART)
GVBCDB                      <- merge(GVBCDB,unique(GPARTtoKVKSEGMENT))
GVBCDB                      <- GVBCDB[which(GVBCDB$netnr %in% MSRs$NUMMER_BEH),]  #selecteren behuizingen NHN

profielenbeschikbaar        <-  c(1,2,4,7,8,10,13,14,19,20)
GVBCDB                      <-  transform(GVBCDB, freq.seg = ave(seq(nrow(GVBCDB)),EAN_CODE, FUN=length))
#GVBCDB[which(GVBCDB$freq.seg>1),]


#Koppelen met SJV
CDBGISMV$M_TIMESTAMP  <- as.Date(as.character(CDBGISMV$M_TIMESTAMP), format= "%d-%m-%y")
SJVuitCDB             <- ddply(CDBGISMV[which(CDBGISMV$M_TIMESTAMP>=as.Date("2013-01-01") & CDBGISMV$M_TIMESTAMP<=as.Date("2013-12-31")),],
                               "M_POINT",summarise,SJVCDB=12*mean(M_VALUE,na.rm=T))

GVBCDB                <- merge(GVBCDB,SJVuitCDB,by.x="m_point",by.y="M_POINT",all.x=T)
GVBCDB$SJVCDB[is.na(GVBCDB$SJVCDB)]  <-   CARNHNGV$totaal_verbruik[which(CARNHNGV$EAN_CODE_AANSLUITING %in% GVBCDB$EAN_CODE[is.na(GVBCDB$SJVCDB)])]
#names(GVBCDB)[56] <- "SJVCDB"
#berekenen maximum fractie profielen
maxima                <- data.frame(apply(profielen,2,max,na.rm=T)[c("P1","P2","P4","P7","P8","P10","P13","P14","P19","P20")])
names(maxima)         <- c("maxfrac")
maxima$profnummer     <- profielenbeschikbaar

maximaEDSN            <- data.frame(apply(profielenEDSN,2,max,na.rm=T))


GVBCDB                <- merge(GVBCDB,maxima,by.x="KVKSEGMENT",by.y="profnummer",all.x=T)
GVBCDB$maxfrac        <- as.numeric(GVBCDB$maxfrac)
GVBCDB$maxfrac[is.na(GVBCDB$maxfrac)] <- 6.659e-05
class(GVBCDB$maxfrac)

names(profielen)
names(GVBCDB)
names(SJVuitCDB)



#inlezen welk profiel bij welk segment hoort
segments = c(1,2,4,7,8,10,13,14,19,20)
GVEANS <- data.frame()
bla=list()
for(i in segments){
 filename=paste0("N:/Bottum Up Analyse/2. Data/2. Baseload GV/nhn/eans in segment ",i,".csv")
 bla[[i]] <- as.list(read.csv(filename))
}
