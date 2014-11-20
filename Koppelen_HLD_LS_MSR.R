setwd("N:/Bottum Up Analyse/2. Data/3. Nettopologie")
#append column to data frame with with frequency of one of it's columns

#inlezen hoofdleidingen in bonoka
hld              <- read.table("HLD_AANSL.txt",sep=";",dec=",",col.names=c("HoofdleidingID","Aantal_Aansluitingen"))
#inlezen koppelingen van LS-Hoofdleidingen op LS-kasten en OV-kasten
lskoppelingen    <- read.table("LS_aansluitingen.txt",sep=";",
                 col.names=c("VELDNUMMER","LS_VELD_ID","BMR_FUNCTIE","BMR_SUBFUNCTIE",
                             "STATIONBEHUIZING","BEV_NORM","SCHAK_NORM","LS_KABEL_ID",
                             "LS_HLD_ID","HOOFDLEIDING","SAMENGEVOEGD","AANTAL_AANSL"))
#inlezen koppelingen van LS-Hoofdleidingen op MS-ruimtes
mskoppelingen    <- read.table("MS_aansluitingen.txt",sep=";",
                 col.names=c("VELDNUMMER","LS_VELD_ID","BMR_FUNCTIE","BMR_SUBFUNCTIE",
                             "STATIONBEHUIZING","BEV_NORM","SCHAK_NORM","LS_KABEL_ID",
                             "LS_HLD_ID","HOOFDLEIDING","SAMENGEVOEGD","AANTAL_AANSL"))
#neem alleen gesloten beveligingen mee
lskoppelingen    <- lskoppelingen[which(lskoppelingen$BEV_NORM=="dicht"),]
mskoppelingen    <- mskoppelingen[which(mskoppelingen$BEV_NORM=="dicht"),]

#barplot van frequentietabel
barplot(table(c(as.vector(lskoppelingen$SAMENGEVOEGD),as.vector(mskoppelingen$SAMENGEVOEGD))))
#aanmaken  totaalfrequentietabel van alle koppelingen per hoofdleiding
koppelingen      <- data.frame(table(c(as.vector(lskoppelingen$SAMENGEVOEGD),as.vector(mskoppelingen$SAMENGEVOEGD))))
#toevoegen nieuwe kolom met frequentie
hld$nkoppelingen <- as.vector(sapply(as.vector(hld$HoofdleidingID),function(x)
  if(length(which(koppelingen$Var1==x))==0){0}else{koppelingen$Freq[which(koppelingen$Var1==x)]}))

#aantal hoofdleidingen niet gekoppeld
nrow(hld)-nrow(koppelingen)
#aantal aansluitingen niet gekoppeld aan LS-kast of MSR
sum(hld$Aantal_Aansluitingen[which(hld$nkoppelingen==0)])
#freuqntietabel
table(hld$nkoppelingen)

#normeren van het aantal aansluitingen
hld$naanslgenorm  <- ifelse(hld$nkoppelingen==0,hld$Aantal_Aansluitingen,hld$Aantal_Aansluitingen/hld$nkoppelingen)

#maak lijst van LS-kasten
lskast    <-data.frame(table(lskoppelingen$STATIONBEHUIZING))

#koppelen LS-kast aan MSR
lskastmsr    <- merge(lskoppelingen[,c("STATIONBEHUIZING","SAMENGEVOEGD")],mskoppelingen[,c("STATIONBEHUIZING","SAMENGEVOEGD")],by="SAMENGEVOEGD")
colnames(lskastmsr)<- c("HLD","LSkast","MSR")
# aantal gevonden LS-kasten
length(unique(lskoppelingen$STATIONBEHUIZING))
length(unique(lskastmsr$lskast))

#toevoegen frequentiekolom
#lskastmsr  <- transform(lskastmsr, freq.loc = ave(seq(nrow(lskastmsr)),lskast, FUN=length))

#zoeken lskasten niet direct gekoppeld
koppelingennietmsr<-lskoppelingen[-which(lskoppelingen$STATIONBEHUIZING %in% lskastmsr$lskast),]
length(unique(koppelingennietmsr$STATIONBEHUIZING))

lskastlskast  <- merge(koppelingennietmsr[,c("STATIONBEHUIZING","SAMENGEVOEGD")],lskoppelingen[,c("STATIONBEHUIZING","SAMENGEVOEGD")],by="SAMENGEVOEGD")
colnames(lskastlskast)<- c("HLD","lskast1","lskast2")
#verwijder koppelingen naar zichzelf
lskastlskast  <- lskastlskast[-which(lskastlskast$lskast1==lskastlskast$lskast2),]
lskastlskast  <- transform(lskastlskast, freq.loc = ave(seq(nrow(lskastlskast)),lskast1, FUN=length))
length(unique(lskastlskast$lskast1))

lskastmsr2    <- merge (lskastlskast,lskastmsr,by.x="lskast2",by.y="LSkast")
lskastmsr2    <- lskastmsr2[c("lskast1","HLD.x","lskast2","HLD.y","MSR")]
length(unique(lskastmsr2$lskast1))

koppelingennietmsr2 <- koppelingennietmsr[-which(koppelingennietmsr$STATIONBEHUIZING %in% lskastmsr2$lskast1),]
length(unique(koppelingennietmsr2$STATIONBEHUIZING))

lskast3   <- merge(koppelingennietmsr2[,c("STATIONBEHUIZING","SAMENGEVOEGD")],lskoppelingen[,c("STATIONBEHUIZING","SAMENGEVOEGD")],by="SAMENGEVOEGD")
colnames(lskast3)<- c("HLD","lskast3","lskast2")
#verwijder koppelingen naar zichzelf
lskast3  <- lskast3[-which(lskast3$lskast1==lskast3$lskast2),]
nrow(lskast3)
lskast3  <- transform(lskast3, freq.loc=ave(seq(nrow(lskast3)),lskast1,FUN=length))

lskastmsr3    <- merge (lskast3,lskastmsr,by.x="lskast2",by.y="LSkast")
lskastmsr3    <- lskastmsr3[c("lskast1","HLD.x","lskast2","HLD.y","MSR")]
length(unique(lskastmsr3$lskast3))


#inlezen eans
EANStoHFD  <- read.csv("N:/Bottum Up Analyse/2. Data/1. Baseload KV/LS_AANSLUITING.csv",sep=";",dec=",",colClasses=c(EAN="character"))
#koppelen aan MSR
EANStoMSR   <- merge(EANStoHFD,mskoppelingen[,c("STATIONBEHUIZING","HOOFDLEIDING")],by="HOOFDLEIDING")
names(EANStoMSR)[names(EANStoMSR) == 'STATIONBEHUIZING'] <- 'MSR'
EANStoMSR   <- transform(EANStoMSR, freq.EANopMSR=ave(seq(nrow(EANStoMSR)),EAN,FUN=length))
sum(EANStoMSR$freq.EANopMSR)
EANStoLSK  <- merge(EANStoHFD,lskoppelingen[,c("STATIONBEHUIZING","HOOFDLEIDING")],by="HOOFDLEIDING")
names(EANStoLSK)[names(EANStoLSK) == 'STATIONBEHUIZING'] <- 'LSkast'
EANStoMSR2 <- merge(EANStoLSK,lskastmsr,by="LSkast")
#EANStoMSR2 <- EANStoMSR2[-which(duplicated(EANStoMSR2[,c("EAN","MSR")])),] #verwijder dubbele koppelingen tussen dezelfde EAN en MSR
EANStoMSR3 <- merge(EANStoLSK,lskastmsr2,by.x="LSkast",by.y="lskast1")
#EANStoMSR3 <- EANStoMSR3[-which(duplicated(EANStoMSR3[,c("EAN","MSR")])),]
require(plyr)
EANStoMSR4 <- rbind.fill(EANStoMSR,EANStoMSR2,EANStoMSR3)
EANStoMSR4 <- EANStoMSR4[-which(duplicated(EANStoMSR4[,c("EAN","MSR")])),]
EANStoMSR4 <- transform(EANStoMSR4, freq.EANopMSR=ave(seq(nrow(EANStoMSR4)),EAN,FUN=length))
EANStoMSR4 <- EANStoMSR4[,c("EAN","HOOFDLEIDING","FUNCTIE","LS_KABELS_ID","SOORT","STATUS","ARI_ADRES",
                            "CONTRACT_CAPACITEIT","PROFIEL_TYPE","STANDAARD_JAARVERBRUIK","STANDAARD_JAARVERBRUIK_LAAG",
                            "NOMINALE_CAPACITEIT","WIJZIG_NAAM","LEVERINGSRICHTING","VERBLIJFSFUNCTIE_","UITVOERING",
                            "MSR","freq.EANopMSR")]
write.csv(EANStoMSR4,file="N:/Bottum Up Analyse/2. Data/1. Baseload KV/MSR_AANSLUITING.csv")
length(unique(EANStoMSR4$EAN))
sum(EANStoMSR4$freq.EANopMSR)

#inlezen msr's, selecteren op postcode en weer wegschrijven
#library(data.table)
#msrs<-fread("MSR.csv")
#postcodes<- read.csv("N:/Bottum Up Analyse/2. Data/0. Gebiedsafbakening/Noord_Holland_PC.csv", sep=";")
#msrs$PC4 <- substr(msrs$POSTCODE,1,4)
#msrs     <- msrs[which(msrs$PC4 %in% postcodes$Postcode),]
#write.csv(msrs,file="MSRbonoka.csv")
#table(msrs$BMR_FUNCTIE)


