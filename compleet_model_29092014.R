#Script om belasting per LS_hld/MSR te bepalen dmv prognoses K&M

rm(list=ls(all=TRUE))
setwd("\\\\fspaka01/userdata3b$/AL8368/Documents/Advies Groep/NieuwConnectiviteitsmodel/Belastingprofiel_Bepaling/")

##ONDERSTAAND STUK HOEFT WAARSCHIJNLIJK MAAR EEN KEER WANT HEEFT PACKAGE DPLYR NODIG (niet beschikbaar op server)
library(reshape2)
library(dplyr)
library(data.table)

#Eerst voor de algemene aansluitingen

Aansl_Flevoland <- read.table("Aansluitingen_Flevoland.csv", sep = ";", colClasses = "character")
Aansl_Flevoland_Gen <- read.table("Aansluitingen_Flevoland_Gen.csv", sep = ";", colClasses = "character")
Aansl_Flevoland <- rbind_all(list(Aansl_Flevoland, Aansl_Flevoland_Gen))

Aansl_Flevoland$PC6 <- substr(Aansl_Flevoland$ARI_ADRES, 1, 6)
Aansl_Flevoland$PC4 <- substr(Aansl_Flevoland$ARI_ADRES, 1, 4)
Aansl_Flevoland$STANDAARD_JAARVERBRUIK <- as.numeric(Aansl_Flevoland$STANDAARD_JAARVERBRUIK)

#GV Stuk, afhankelijk van de input (bar trace) moet worden besloten of dit zinvol is of niet

GV_Zak_Flevo <- read.table("GV_zakelijk.csv", sep = ";", colClasses = "character", header = TRUE)
GV_Zak_Flevo <- GV_Zak_Flevo[,c(1,2,20)]
GV_EAN_NL <- read.table("GV_E_2013.csv", sep = ";", dec = ",", colClasses= "character", header = TRUE)
colnames(GV_EAN_NL) <- c("EAN_CODE", colnames(GV_EAN_NL)[2:4])
GV_Zak_Flevo <- merge(GV_Zak_Flevo, GV_EAN_NL, by = "EAN_CODE")
colnames(GV_Zak_Flevo) <- c("EAN", colnames(GV_Zak_Flevo)[2:6])
Aansl_Flevoland_GV <- merge(Aansl_Flevoland, GV_Zak_Flevo, by = "EAN")
Aansl_Flevoland_GV$LEVERING <- as.numeric(Aansl_Flevoland_GV$LEVERING) 

#Base_L voor LS_Hld's

LS_Hld_BaseL <- Aansl_Flevoland %>% group_by(LS_HLD_ID, PROFIEL_TYPE) %>% summarise(Totaal_JV=sum(STANDAARD_JAARVERBRUIK))
LS_Hld_BaseL <- dcast(LS_Hld_BaseL, LS_HLD_ID ~ PROFIEL_TYPE, sum, value.var = "Totaal_JV")
LS_Hld_BaseL <- LS_Hld_BaseL[,-2]
LS_Hld_BaseL[,c(2:11)] <- apply(LS_Hld_BaseL[,c(2:11)], 2, function(x) ifelse(is.nan(x), 0, x))

#Aan de baseload moet ook de GV profielen worden toegevoegd, moet worden gemergd met profiel type en dan vervolgens dcast'en

LS_Hld_BaseL_GV <- Aansl_Flevoland_GV %>% group_by(LS_HLD_ID, SEGMENT) %>% summarise(Totaal_JV=sum(LEVERING))
LS_Hld_BaseL_GV <- dcast(LS_Hld_BaseL_GV, LS_HLD_ID ~ SEGMENT, sum, value.var = "Totaal_JV")
LS_Hld_BaseL_GV <- LS_Hld_BaseL_GV[,-2]
LS_Hld_BaseL[,c(2:11)] <- apply(LS_Hld_BaseL[,c(2:11)], 2, function(x) ifelse(is.nan(x), 0, x))

#Vervolgens segmenten aan totale lijst koppelen en kijken of hier een profiel voor beschikbaar is...


# #Base_L voor MSR's
# 
# MSR_BaseL <- Aansl_Flevoland %>% group_by(STATIONBEHUIZING, PROFIEL_TYPE) %>% summarise(Totaal_JV=sum(STANDAARD_JAARVERBRUIK))
# MSR_BaseL <- dcast(MSR_BaseL, STATIONBEHUIZING ~ PROFIEL_TYPE, sum, value.var = "Totaal_JV")
# MSR_BaseL <- MSR_BaseL[,-2]
# MSR_BaseL[,c(2:11)] <- apply(MSR_BaseL[,c(2:11)], 2, function(x) ifelse(is.nan(x), 0, x))
# 


# # Code PenGr Specifiek ----------------------------------------------------
# 
# 
# #Dan voor de Penetratiegraden, eerst voor LS_Hld de "basis-gegevens"
# 
# LS_Hld_PenGr <- Aansl_Flevoland %>% group_by(LS_HLD_ID, PC4, PC6) %>% summarise(Aantal_Aansl_PC6 = n_distinct(EAN))
# 
# #Dan de "basis-gegevens" voor de MSR'en
# 
# MSR_PenGr <- Aansl_Flevoland %>% group_by(STATIONBEHUIZING, PC4, PC6) %>% summarise(Aantal_Aansl_PC6 = n_distinct(EAN))
# 
# #Vervolgens wat algemene gegevens
# 
# Aansl_PC4 <- Aansl_Flevoland %>% group_by(PC4) %>% summarise(Aantal_aansl_PC4 = n_distinct(EAN))
# LS_Hld_Dubbelingen_PC6 <- LS_Hld_PenGr %>% group_by(PC6) %>% summarise(Voorkomend_PC6 = n())
# MSR_Dubbelingen_PC6 <- MSR_PenGr %>% group_by(PC6) %>% summarise(Voorkomend_PC6 = n())
# 
# LS_Hld_PenGr <- merge(LS_Hld_PenGr, LS_Hld_Dubbelingen_PC6, by = "PC6")
# LS_Hld_PenGr <- merge(LS_Hld_PenGr, Aansl_PC4, by = "PC4")
# 
# MSR_PenGr <- merge(MSR_PenGr, MSR_Dubbelingen_PC6, by = "PC6")
# MSR_PenGr <- merge(MSR_PenGr, Aansl_PC4, by = "PC4")

#Dan de specifieke penetratiegraden
# 
# #Eerst voor EV en dan vervolgens een lijst per LS_Hld maken, daarna voor de MSR'en
# 
# EV_PenGr_L <- read.table("EV_Penetratie_PC4_L.csv", sep = ";", dec = ",", header= TRUE)
# colnames(EV_PenGr_L) <- c("PC4", paste("Y", 2015:2050, sep = ""))
# EV_PenGr_M <- read.table("EV_Penetratie_PC4_M.csv", sep = ";", dec = ",", header= TRUE)
# colnames(EV_PenGr_M) <- c("PC4", paste("Y", 2015:2050, sep = ""))
# EV_PenGr_H <- read.table("EV_Penetratie_PC4_H.csv", sep = ";", dec = ",", header= TRUE)
# colnames(EV_PenGr_H) <- c("PC4", paste("Y", 2015:2050, sep = ""))
# 
# #LS_Hld
# 
# LS_Hld_PenGr_EV_L <- merge(LS_Hld_PenGr, EV_PenGr_L, by = "PC4")
# LS_Hld_PenGr_EV_L[,7:22] <- LS_Hld_PenGr_EV_L[,7:22]*LS_Hld_PenGr_EV_L$Aantal_Aansl_PC6/(LS_Hld_PenGr_EV_L$Aantal_aansl_PC4*LS_Hld_PenGr_EV_L$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_EV_L) <- c(colnames(LS_Hld_PenGr_EV_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_EV_L <- LS_Hld_PenGr_EV_L %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_EV_M <- merge(LS_Hld_PenGr, EV_PenGr_M, by = "PC4")
# LS_Hld_PenGr_EV_M[,7:22] <- LS_Hld_PenGr_EV_M[,7:22]*LS_Hld_PenGr_EV_M$Aantal_Aansl_PC6/(LS_Hld_PenGr_EV_M$Aantal_aansl_PC4*LS_Hld_PenGr_EV_M$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_EV_M) <- c(colnames(LS_Hld_PenGr_EV_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_EV_M <- LS_Hld_PenGr_EV_M %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_EV_H <- merge(LS_Hld_PenGr, EV_PenGr_H, by = "PC4")
# LS_Hld_PenGr_EV_H[,7:22] <- LS_Hld_PenGr_EV_H[,7:22]*LS_Hld_PenGr_EV_H$Aantal_Aansl_PC6/(LS_Hld_PenGr_EV_H$Aantal_aansl_PC4*LS_Hld_PenGr_EV_H$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_EV_H) <- c(colnames(LS_Hld_PenGr_EV_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_EV_H <- LS_Hld_PenGr_EV_H %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #MSR'en
# 
# MSR_PenGr_EV_L <- merge(MSR_PenGr, EV_PenGr_L, by = "PC4")
# MSR_PenGr_EV_L[,7:22] <- MSR_PenGr_EV_L[,7:22]*MSR_PenGr_EV_L$Aantal_Aansl_PC6/(MSR_PenGr_EV_L$Aantal_aansl_PC4*MSR_PenGr_EV_L$Voorkomend_PC6)
# colnames(MSR_PenGr_EV_L) <- c(colnames(MSR_PenGr_EV_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_EV_L <- MSR_PenGr_EV_L %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_EV_M <- merge(MSR_PenGr, EV_PenGr_M, by = "PC4")
# MSR_PenGr_EV_M[,7:22] <- MSR_PenGr_EV_M[,7:22]*MSR_PenGr_EV_M$Aantal_Aansl_PC6/(MSR_PenGr_EV_M$Aantal_aansl_PC4*MSR_PenGr_EV_M$Voorkomend_PC6)
# colnames(MSR_PenGr_EV_M) <- c(colnames(MSR_PenGr_EV_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_EV_M <- MSR_PenGr_EV_M %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_EV_H <- merge(MSR_PenGr, EV_PenGr_H, by = "PC4")
# MSR_PenGr_EV_H[,7:22] <- MSR_PenGr_EV_H[,7:22]*MSR_PenGr_EV_H$Aantal_Aansl_PC6/(MSR_PenGr_EV_H$Aantal_aansl_PC4*MSR_PenGr_EV_H$Voorkomend_PC6)
# colnames(MSR_PenGr_EV_H) <- c(colnames(MSR_PenGr_EV_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_EV_H <- MSR_PenGr_EV_H %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #Nu voor PV, eerst weer LS_Hld en dan MSR
# 
# PC6_PenGr <- read.table("PC6_tot.csv", sep = ",")
# 
# PV_PenGr_L <- read.table("nPV_Low.csv", sep = ",")
# PV_PenGr_L <- t(PV_PenGr_L)
# PV_PenGr_L <- PV_PenGr_L[,-c(1,2)]
# PV_PenGr_L <- cbind(PC6_PenGr, PV_PenGr_L)
# colnames(PV_PenGr_L) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# PV_PenGr_M <- read.table("nPV_Med.csv", sep = ",")
# PV_PenGr_M <- t(PV_PenGr_M)
# PV_PenGr_M <- PV_PenGr_M[,-c(1,2)]
# PV_PenGr_M <- cbind(PC6_PenGr, PV_PenGr_M)
# colnames(PV_PenGr_M) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# PV_PenGr_H <- read.table("nPV_High.csv", sep = ",")
# PV_PenGr_H <- t(PV_PenGr_H)
# PV_PenGr_H <- PV_PenGr_H[,-c(1,2)]
# PV_PenGr_H <- cbind(PC6_PenGr, PV_PenGr_H)
# colnames(PV_PenGr_H) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# #Eerst voor LS_Hld
# 
# LS_Hld_PenGr_PV_L <- merge(LS_Hld_PenGr, PV_PenGr_L, by = "PC6")
# LS_Hld_PenGr_PV_L[,7:22] <- LS_Hld_PenGr_PV_L[,7:22]*LS_Hld_PenGr_PV_L$Aantal_Aansl_PC6/(LS_Hld_PenGr_PV_L$Aantal_aansl_PC4*LS_Hld_PenGr_PV_L$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_PV_L) <- c(colnames(LS_Hld_PenGr_PV_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_PV_L <- LS_Hld_PenGr_PV_L %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_PV_M <- merge(LS_Hld_PenGr, PV_PenGr_M, by = "PC6")
# LS_Hld_PenGr_PV_M[,7:22] <- LS_Hld_PenGr_PV_M[,7:22]*LS_Hld_PenGr_PV_M$Aantal_Aansl_PC6/(LS_Hld_PenGr_PV_M$Aantal_aansl_PC4*LS_Hld_PenGr_PV_M$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_PV_M) <- c(colnames(LS_Hld_PenGr_PV_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_PV_M <- LS_Hld_PenGr_PV_M %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_PV_H <- merge(LS_Hld_PenGr, PV_PenGr_H, by = "PC6")
# LS_Hld_PenGr_PV_H[,7:22] <- LS_Hld_PenGr_PV_H[,7:22]*LS_Hld_PenGr_PV_H$Aantal_Aansl_PC6/(LS_Hld_PenGr_PV_H$Aantal_aansl_PC4*LS_Hld_PenGr_PV_H$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_PV_H) <- c(colnames(LS_Hld_PenGr_PV_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_PV_H <- LS_Hld_PenGr_PV_H %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #Nu voor MSR'en
# 
# MSR_PenGr_PV_L <- merge(MSR_PenGr, PV_PenGr_L, by = "PC6")
# MSR_PenGr_PV_L[,7:22] <- MSR_PenGr_PV_L[,7:22]*MSR_PenGr_PV_L$Aantal_Aansl_PC6/(MSR_PenGr_PV_L$Aantal_aansl_PC4*MSR_PenGr_PV_L$Voorkomend_PC6)
# colnames(MSR_PenGr_PV_L) <- c(colnames(MSR_PenGr_PV_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_PV_L <- MSR_PenGr_PV_L %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_PV_M <- merge(MSR_PenGr, PV_PenGr_M, by = "PC6")
# MSR_PenGr_PV_M[,7:22] <- MSR_PenGr_PV_M[,7:22]*MSR_PenGr_PV_M$Aantal_Aansl_PC6/(MSR_PenGr_PV_M$Aantal_aansl_PC4*MSR_PenGr_PV_M$Voorkomend_PC6)
# colnames(MSR_PenGr_PV_M) <- c(colnames(MSR_PenGr_PV_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_PV_M <- MSR_PenGr_PV_M %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_PV_H <- merge(MSR_PenGr, PV_PenGr_H, by = "PC6")
# MSR_PenGr_PV_H[,7:22] <- MSR_PenGr_PV_H[,7:22]*MSR_PenGr_PV_H$Aantal_Aansl_PC6/(MSR_PenGr_PV_H$Aantal_aansl_PC4*MSR_PenGr_PV_H$Voorkomend_PC6)
# colnames(MSR_PenGr_PV_H) <- c(colnames(MSR_PenGr_PV_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_PV_H <- MSR_PenGr_PV_H %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #Als laatste voor WP, eerst weer LS_Hld en dan MSR
# 
# WP_PenGr_L <- read.table("nWPRen_Low.csv", sep = ",")
# WP_PenGr_L <- t(WP_PenGr_L)
# WP_PenGr_L <- WP_PenGr_L[,-c(1,2)]
# WP_PenGr_L <- cbind(PC6_PenGr, WP_PenGr_L)
# colnames(WP_PenGr_L) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# WP_PenGr_M <- read.table("nWPRen_Med.csv", sep = ",")
# WP_PenGr_M <- t(WP_PenGr_M)
# WP_PenGr_M <- WP_PenGr_M[,-c(1,2)]
# WP_PenGr_M <- cbind(PC6_PenGr, WP_PenGr_M)
# colnames(WP_PenGr_M) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# WP_PenGr_H <- read.table("nWPRen_High.csv", sep = ",")
# WP_PenGr_H <- t(WP_PenGr_H)
# WP_PenGr_H <- WP_PenGr_H[,-c(1,2)]
# WP_PenGr_H <- cbind(PC6_PenGr, WP_PenGr_H)
# colnames(WP_PenGr_H) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# WP_PenGr_Max <- read.table("nWPRen_Max.csv", sep = ",")
# WP_PenGr_Max <- t(WP_PenGr_Max)
# WP_PenGr_Max <- WP_PenGr_Max[,-c(1,2)]
# WP_PenGr_Max <- cbind(PC6_PenGr, WP_PenGr_Max)
# colnames(WP_PenGr_Max) <- c("PC6", paste("Y", 2015:2030, sep = ""))
# 
# #Eerst voor LS_Hld
# 
# LS_Hld_PenGr_WP_L <- merge(LS_Hld_PenGr, WP_PenGr_L, by = "PC6")
# LS_Hld_PenGr_WP_L[,7:22] <- LS_Hld_PenGr_WP_L[,7:22]*LS_Hld_PenGr_WP_L$Aantal_Aansl_PC6/(LS_Hld_PenGr_WP_L$Aantal_aansl_PC4*LS_Hld_PenGr_WP_L$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_WP_L) <- c(colnames(LS_Hld_PenGr_WP_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_WP_L <- LS_Hld_PenGr_WP_L %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_WP_M <- merge(LS_Hld_PenGr, WP_PenGr_M, by = "PC6")
# LS_Hld_PenGr_WP_M[,7:22] <- LS_Hld_PenGr_WP_M[,7:22]*LS_Hld_PenGr_WP_M$Aantal_Aansl_PC6/(LS_Hld_PenGr_WP_M$Aantal_aansl_PC4*LS_Hld_PenGr_WP_M$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_WP_M) <- c(colnames(LS_Hld_PenGr_WP_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_WP_M <- LS_Hld_PenGr_WP_M %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_WP_H <- merge(LS_Hld_PenGr, WP_PenGr_H, by = "PC6")
# LS_Hld_PenGr_WP_H[,7:22] <- LS_Hld_PenGr_WP_H[,7:22]*LS_Hld_PenGr_WP_H$Aantal_Aansl_PC6/(LS_Hld_PenGr_WP_H$Aantal_aansl_PC4*LS_Hld_PenGr_WP_H$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_WP_H) <- c(colnames(LS_Hld_PenGr_WP_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_WP_H <- LS_Hld_PenGr_WP_H %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# LS_Hld_PenGr_WP_Max <- merge(LS_Hld_PenGr, WP_PenGr_Max, by = "PC6")
# LS_Hld_PenGr_WP_Max[,7:22] <- LS_Hld_PenGr_WP_Max[,7:22]*LS_Hld_PenGr_WP_Max$Aantal_Aansl_PC6/(LS_Hld_PenGr_WP_Max$Aantal_aansl_PC4*LS_Hld_PenGr_WP_Max$Voorkomend_PC6)
# colnames(LS_Hld_PenGr_WP_Max) <- c(colnames(LS_Hld_PenGr_WP_Max[,1:6]), paste("Y", 2015:2030, sep = ""))
# LS_Hld_PenGr_WP_Max <- LS_Hld_PenGr_WP_Max %>% group_by(LS_HLD_ID) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #Nu voor MSR'en
# 
# MSR_PenGr_WP_L <- merge(MSR_PenGr, WP_PenGr_L, by = "PC6")
# MSR_PenGr_WP_L[,7:22] <- MSR_PenGr_WP_L[,7:22]*MSR_PenGr_WP_L$Aantal_Aansl_PC6/(MSR_PenGr_WP_L$Aantal_aansl_PC4*MSR_PenGr_WP_L$Voorkomend_PC6)
# colnames(MSR_PenGr_WP_L) <- c(colnames(MSR_PenGr_WP_L[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_WP_L <- MSR_PenGr_WP_L %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_WP_M <- merge(MSR_PenGr, WP_PenGr_M, by = "PC6")
# MSR_PenGr_WP_M[,7:22] <- MSR_PenGr_WP_M[,7:22]*MSR_PenGr_WP_M$Aantal_Aansl_PC6/(MSR_PenGr_WP_M$Aantal_aansl_PC4*MSR_PenGr_WP_M$Voorkomend_PC6)
# colnames(MSR_PenGr_WP_M) <- c(colnames(MSR_PenGr_WP_M[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_WP_M <- MSR_PenGr_WP_M %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_WP_H <- merge(MSR_PenGr, WP_PenGr_H, by = "PC6")
# MSR_PenGr_WP_H[,7:22] <- MSR_PenGr_WP_H[,7:22]*MSR_PenGr_WP_H$Aantal_Aansl_PC6/(MSR_PenGr_WP_H$Aantal_aansl_PC4*MSR_PenGr_WP_H$Voorkomend_PC6)
# colnames(MSR_PenGr_WP_H) <- c(colnames(MSR_PenGr_WP_H[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_WP_H <- MSR_PenGr_WP_H %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# MSR_PenGr_WP_Max <- merge(MSR_PenGr, WP_PenGr_Max, by = "PC6")
# MSR_PenGr_WP_Max[,7:22] <- MSR_PenGr_WP_Max[,7:22]*MSR_PenGr_WP_Max$Aantal_Aansl_PC6/(MSR_PenGr_WP_Max$Aantal_aansl_PC4*MSR_PenGr_WP_Max$Voorkomend_PC6)
# colnames(MSR_PenGr_WP_Max) <- c(colnames(MSR_PenGr_WP_Max[,1:6]), paste("Y", 2015:2030, sep = ""))
# MSR_PenGr_WP_Max <- MSR_PenGr_WP_Max %>% group_by(STATIONBEHUIZING) %>% summarise(Y2015 = sum(Y2015), Y2016 = sum(Y2016), Y2017 = sum(Y2017), Y2018 = sum(Y2018), Y2019 = sum(Y2019), Y2020 = sum(Y2020), Y2021 = sum(Y2021), Y2022 = sum(Y2022), Y2023 = sum(Y2023), Y2024 = sum(Y2024), Y2025 = sum(Y2025), Y2026 = sum(Y2026), Y2027 = sum(Y2027), Y2028 = sum(Y2028), Y2029 = sum(Y2029), Y2030 = sum(Y2030))
# 
# #Als laatste er nog voor zorgen dat alle lijsten (van de Baseload etc) dezelfde lengte hebben
# 
# #De "standaard" lijst komt van LS_Hld_BaseL en MSR_BaseL
# 
# LS_Hld_StdLst <- data.frame(LS_HLD_ID = LS_Hld_BaseL[,1])
# MSR_StdLst <- data.frame(STATIONBEHUIZING = MSR_BaseL[,1])
# 
# #Vervolgens allemaal "standizeren"
# 
# #Eerst voor de LS_Hld's
# 
# LS_Hld_PenGr_EV_L <- merge(LS_Hld_StdLst, LS_Hld_PenGr_EV_L, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_EV_L[,c(2:17)] <- apply(LS_Hld_PenGr_EV_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_EV_M <- merge(LS_Hld_StdLst, LS_Hld_PenGr_EV_M, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_EV_M[,c(2:17)] <- apply(LS_Hld_PenGr_EV_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_EV_H <- merge(LS_Hld_StdLst, LS_Hld_PenGr_EV_H, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_EV_H[,c(2:17)] <- apply(LS_Hld_PenGr_EV_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# LS_Hld_PenGr_PV_L <- merge(LS_Hld_StdLst, LS_Hld_PenGr_PV_L, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_PV_L[,c(2:17)] <- apply(LS_Hld_PenGr_PV_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_PV_M <- merge(LS_Hld_StdLst, LS_Hld_PenGr_PV_M, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_PV_M[,c(2:17)] <- apply(LS_Hld_PenGr_PV_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_PV_H <- merge(LS_Hld_StdLst, LS_Hld_PenGr_PV_H, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_PV_H[,c(2:17)] <- apply(LS_Hld_PenGr_PV_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# LS_Hld_PenGr_WP_L <- merge(LS_Hld_StdLst, LS_Hld_PenGr_WP_L, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_WP_L[,c(2:17)] <- apply(LS_Hld_PenGr_WP_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_WP_M <- merge(LS_Hld_StdLst, LS_Hld_PenGr_WP_M, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_WP_M[,c(2:17)] <- apply(LS_Hld_PenGr_WP_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_WP_H <- merge(LS_Hld_StdLst, LS_Hld_PenGr_WP_H, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_WP_H[,c(2:17)] <- apply(LS_Hld_PenGr_WP_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# LS_Hld_PenGr_WP_Max <- merge(LS_Hld_StdLst, LS_Hld_PenGr_WP_Max, by = "LS_HLD_ID", all = T)
# LS_Hld_PenGr_WP_Max[,c(2:17)] <- apply(LS_Hld_PenGr_WP_Max[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# #Vervolgens voor de MSR'en
# 
# MSR_PenGr_EV_L <- merge(MSR_StdLst, MSR_PenGr_EV_L, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_EV_L[,c(2:17)] <- apply(MSR_PenGr_EV_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_EV_M <- merge(MSR_StdLst, MSR_PenGr_EV_M, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_EV_M[,c(2:17)] <- apply(MSR_PenGr_EV_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_EV_H <- merge(MSR_StdLst, MSR_PenGr_EV_H, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_EV_H[,c(2:17)] <- apply(MSR_PenGr_EV_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# MSR_PenGr_PV_L <- merge(MSR_StdLst, MSR_PenGr_PV_L, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_PV_L[,c(2:17)] <- apply(MSR_PenGr_PV_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_PV_M <- merge(MSR_StdLst, MSR_PenGr_PV_M, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_PV_M[,c(2:17)] <- apply(MSR_PenGr_PV_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_PV_H <- merge(MSR_StdLst, MSR_PenGr_PV_H, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_PV_H[,c(2:17)] <- apply(MSR_PenGr_PV_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# MSR_PenGr_WP_L <- merge(MSR_StdLst, MSR_PenGr_WP_L, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_WP_L[,c(2:17)] <- apply(MSR_PenGr_WP_L[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_WP_M <- merge(MSR_StdLst, MSR_PenGr_WP_M, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_WP_M[,c(2:17)] <- apply(MSR_PenGr_WP_M[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_WP_H <- merge(MSR_StdLst, MSR_PenGr_WP_H, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_WP_H[,c(2:17)] <- apply(MSR_PenGr_WP_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_WP_H <- merge(MSR_StdLst, MSR_PenGr_WP_H, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_WP_H[,c(2:17)] <- apply(MSR_PenGr_WP_H[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# MSR_PenGr_WP_Max <- merge(MSR_StdLst, MSR_PenGr_WP_Max, by = "STATIONBEHUIZING", all = T)
# MSR_PenGr_WP_Max[,c(2:17)] <- apply(MSR_PenGr_WP_Max[,c(2:17)], 2, function(x) ifelse(is.na(x), 0, x))
# 
# ###Vervolgens alles opslaan om ervoor te zorgen dat we dit niet elke keer hoeven te doen
# 
# write.table(LS_Hld_PenGr_EV_L, "LS_Hld_PenGr_EV_L.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_EV_M, "LS_Hld_PenGr_EV_M.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_EV_H, "LS_Hld_PenGr_EV_H.csv", sep = ";", row.names = FALSE)
# 
# write.table(LS_Hld_PenGr_PV_L, "LS_Hld_PenGr_PV_L.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_PV_M, "LS_Hld_PenGr_PV_M.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_PV_H, "LS_Hld_PenGr_PV_H.csv", sep = ";", row.names = FALSE)
# 
# write.table(LS_Hld_PenGr_WP_L, "LS_Hld_PenGr_WP_L.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_WP_M, "LS_Hld_PenGr_WP_M.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_WP_H, "LS_Hld_PenGr_WP_H.csv", sep = ";", row.names = FALSE)
# write.table(LS_Hld_PenGr_WP_Max, "LS_Hld_PenGr_WP_Max.csv", sep = ";", row.names = FALSE)
# 
# write.table(MSR_PenGr_EV_L, "MSR_PenGr_EV_L.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_EV_M, "MSR_PenGr_EV_M.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_EV_H, "MSR_PenGr_EV_H.csv", sep = ";", row.names = FALSE)
# 
# write.table(MSR_PenGr_PV_L, "MSR_PenGr_PV_L.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_PV_M, "MSR_PenGr_PV_M.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_PV_H, "MSR_PenGr_PV_H.csv", sep = ";", row.names = FALSE)
# 
# write.table(MSR_PenGr_WP_L, "MSR_PenGr_WP_L.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_WP_M, "MSR_PenGr_WP_M.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_WP_H, "MSR_PenGr_WP_H.csv", sep = ";", row.names = FALSE)
# write.table(MSR_PenGr_WP_Max, "MSR_PenGr_WP_Max.csv", sep = ";", row.names = FALSE)

#####
## Inladen van alle penetratiegraden, hoe dit is ontwikkeld staat hierboven

LS_Hld_PenGr_EV_L <- read.table("LS_Hld_PenGr_EV_L.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_EV_M <- read.table("LS_Hld_PenGr_EV_M.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_EV_H <- read.table("LS_Hld_PenGr_EV_H.csv", sep = ";", header = TRUE)

LS_Hld_PenGr_PV_L <- read.table("LS_Hld_PenGr_PV_L.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_PV_M <- read.table("LS_Hld_PenGr_PV_M.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_PV_H <- read.table("LS_Hld_PenGr_PV_H.csv", sep = ";", header = TRUE)

LS_Hld_PenGr_WP_L <- read.table("LS_Hld_PenGr_WP_L.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_WP_M <- read.table("LS_Hld_PenGr_WP_M.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_WP_H <- read.table("LS_Hld_PenGr_WP_H.csv", sep = ";", header = TRUE)
LS_Hld_PenGr_WP_Max <- read.table("LS_Hld_PenGr_WP_Max.csv", sep = ";", header = TRUE)

MSR_PenGr_EV_L <- read.table("MSR_PenGr_EV_L.csv", sep = ";", header = TRUE)
MSR_PenGr_EV_M <- read.table("MSR_PenGr_EV_M.csv", sep = ";", header = TRUE)
MSR_PenGr_EV_H <- read.table("MSR_PenGr_EV_H.csv", sep = ";", header = TRUE)

MSR_PenGr_PV_L <- read.table("MSR_PenGr_PV_L.csv", sep = ";", header = TRUE)
MSR_PenGr_PV_M <- read.table("MSR_PenGr_PV_M.csv", sep = ";", header = TRUE)
MSR_PenGr_PV_H <- read.table("MSR_PenGr_PV_H.csv", sep = ";", header = TRUE)

MSR_PenGr_WP_L <- read.table("MSR_PenGr_WP_L.csv", sep = ";", header = TRUE)
MSR_PenGr_WP_M <- read.table("MSR_PenGr_WP_M.csv", sep = ";", header = TRUE)
MSR_PenGr_WP_H <- read.table("MSR_PenGr_WP_H.csv", sep = ";", header = TRUE)
MSR_PenGr_WP_Max <- read.table("MSR_PenGr_WP_Max.csv", sep = ";", header = TRUE)


#####
#Verschillende Belasting Profielen inladen en omzetten naar uur functies, DIT IS EEN KEER GEDAAN OM TE CHECKEN DAT DE BELASTINGPROFIELEN KLOPPEN, ONDERSTAANDE IS TER REFERENTIE

TimeStamp <- read.table("TimeStamp.csv", sep = ";", stringsAsFactors=F)
EV_Profiel <- read.table("EV_profile.csv", sep = ",")
PV_Profiel <- read.table("PV_profile.csv", sep = ",")
WP_Profiel <- read.table("WP_profile.csv", sep = ",")
EDSN_profiel <-read.table("EDSN_Profielen.csv", sep = ";", header = TRUE)
GV_Belastingprofielen <- read.table("segmentprofiles.csv", sep = ",", header = TRUE)

#### WELKE DAGEN?

#zijn de dagen gelijk of niet, er moet worden geknipt in de uiteindelijke EV, PV en WP profielen om dit gelijk te krijgen
# TestDagen <- data.frame(GV_P = format(as.POSIXct(GV_Belastingprofielen$dt, format="%Y-%m-%d %H:%M:%S", usetz = FALSE), usetz = F, "%a")[1:1000], TIMESTAMP = format(as.POSIXct(TimeStamp$V1, format="%d-%m-%Y %H:%M", usetz = FALSE), usetz = F, "%a")[1:1000], EDSN_P = format(as.POSIXct(EDSN_profiel$UTC.1, format="%d-%m-%Y %H:%M", usetz = FALSE), usetz = F, "%a")[1:1000] )

#Uur gemiddeldes voor gemiddelde segment profielen bepalen

colnames(GV_Belastingprofielen)[2:25] <- paste("p_", colnames(GV_Belastingprofielen)[2:25], sep = "")

#Nu staan alle EDSN profielen achter de EDSN profielen
EDSN_profiel <- cbind(EDSN_profiel, GV_Belastingprofielen[,2:25])

#Checken of de files wel op dezelfde dagen beginnen etc...

TimeStamp <- as.data.frame(TimeStamp)

#Uit timestamp 29 februari verwijderen en naar UTC converten

TimeStamp$uur <- as.numeric(format(as.POSIXct(TimeStamp$V1, format="%d-%m-%Y %H:%M", usetz=F), usetz = F, "%H"))
TimeStamp$datum <- format(as.POSIXct(TimeStamp$V1, format="%d-%m-%Y %H:%M", usetz=F), usetz = F, "%d-%m-%Y")

TimeStamp$uur[which(is.na(TimeStamp$datum))] <- 2
TimeStamp$datum[which(is.na(TimeStamp$datum))] <- "29-03"

EDSN_profiel$uur <- as.numeric(format(as.POSIXct(EDSN_profiel$UTC.1, format="%d-%m-%Y %H:%M", usetz = FALSE), usetz = F, "%H"))
EDSN_profiel$datum <- format(as.POSIXct(EDSN_profiel$UTC.1, format="%d-%m-%Y %H:%M", usetz = FALSE), usetz = F, "%d-%m-%Y")

EDSN_profiel <- EDSN_profiel[-which(is.na(EDSN_profiel$uur)),]

#Vervolgens wordt er gesorteerd op het maximum & min per uur om de worst case te kunnen blijven benaderen

EDSN_profiel <- EDSN_profiel[-nrow(EDSN_profiel),]
EDSN_profiel_uur_max <- EDSN_profiel %>% group_by(datum, uur) %>% summarise(E1A = max(E1A), E1B = max(E1B), E1C = max(E1C), E2A = max(E2A), E2B = max(E2B), E3A = max(E3A), E3B = max(E3B), E3C = max(E3C), E3D = max(E3D), E4A = max(E4A), p_A = max(p_A), p_X = max(p_X), p_G = max(p_G), p_K = max(p_K), p_C = max(p_C), p_M = max(p_M), p_G1 = max(p_G1), p_G2 = max(p_G2), p_L = max(p_L), p_H = max(p_H), p_I = max(p_I), p_Q = max(p_Q), p_S = max(p_S), p_O = max (p_O), p_P = max(p_P), p_R = max(p_R), p_J = max(p_J), p_E = max(p_E), p_F = max(p_F), p_N = max(p_N), p_R2 = max(p_R2), p_D = max(p_D), p_R1 = max(p_R1), p_G3 = max(p_G3))
EDSN_profiel_uur_max <- EDSN_profiel_uur_max[order(as.Date(EDSN_profiel_uur_max$datum, format="%d-%m-%Y")),]

EDSN_profiel_uur_min <- EDSN_profiel %>% group_by(datum, uur) %>% summarise(E1A = min(E1A), E1B = min(E1B), E1C = min(E1C), E2A = min(E2A), E2B = min(E2B), E3A = min(E3A), E3B = min(E3B), E3C = min(E3C), E3D = min(E3D), E4A = min(E4A), p_A = min(p_A), p_X = min(p_X), p_G = min(p_G), p_K = min(p_K), p_C = min(p_C), p_M = min(p_M), p_G1 = min(p_G1), p_G2 = min(p_G2), p_L = min(p_L), p_H = min(p_H), p_I = min(p_I), p_Q = min(p_Q), p_S = min(p_S), p_O = min (p_O), p_P = min(p_P), p_R = min(p_R), p_J = min(p_J), p_E = min(p_E), p_F = min(p_F), p_N = min(p_N), p_R2 = min(p_R2), p_D = min(p_D), p_R1 = min(p_R1), p_G3 = min(p_G3))
EDSN_profiel_uur_min <- EDSN_profiel_uur_min[order(as.Date(EDSN_profiel_uur_min$datum, format="%d-%m-%Y")),]

# 
# EV_Profiel <- cbind(TimeStamp[,2:3], EV_Profiel)
# PV_Profiel <- cbind(TimeStamp[,2:3], PV_Profiel)
# WP_Profiel <- cbind(TimeStamp[,2:3], WP_Profiel)
# EV_Profiel <- EV_Profiel[-which(EV_Profiel$datum == "29-02-2020"),]
# PV_Profiel <- PV_Profiel[-which(PV_Profiel$datum == "29-02-2020"),]
# WP_Profiel <- WP_Profiel[-which(WP_Profiel$datum == "29-02-2020"),]
# 
# #Eerst voor de maxima in de functie
# 
# EV_Profiel_uur_max <- aggregate(EV_Profiel, list(Datum = EV_Profiel$datum, Uur = EV_Profiel$uur), max)
# EV_Profiel_uur_max <- EV_Profiel_uur_max[,-c(1:2)]
# EV_Profiel_uur_max <- EV_Profiel_uur_max[order(as.Date(EV_Profiel_uur_max$datum, format="%d-%m-%Y")),]
# 
# PV_Profiel_uur_max <- aggregate(PV_Profiel, list(Datum = PV_Profiel$datum, Uur = PV_Profiel$uur), max)
# PV_Profiel_uur_max <- PV_Profiel_uur_max[,-c(1:2)]
# PV_Profiel_uur_max <- PV_Profiel_uur_max[order(as.Date(PV_Profiel_uur_max$datum, format="%d-%m-%Y")),]
# 
# WP_Profiel_uur_max <- aggregate(WP_Profiel, list(Datum = WP_Profiel$datum, Uur = WP_Profiel$uur), max)
# WP_Profiel_uur_max <- WP_Profiel_uur_max[,-c(1:2)]
# WP_Profiel_uur_max <- WP_Profiel_uur_max[order(as.Date(WP_Profiel_uur_max$datum, format="%d-%m-%Y")),]
# 
# #Nu voor de minima in de functie
# 
# EV_Profiel_uur_min <- aggregate(EV_Profiel, list(Datum = EV_Profiel$datum, Uur = EV_Profiel$uur), min)
# EV_Profiel_uur_min <- EV_Profiel_uur_min[,-c(1:2)]
# EV_Profiel_uur_min <- EV_Profiel_uur_min[order(as.Date(EV_Profiel_uur_min$datum, format="%d-%m-%Y")),]
# 
# PV_Profiel_uur_min <- aggregate(PV_Profiel, list(Datum = PV_Profiel$datum, Uur = PV_Profiel$uur), min)
# PV_Profiel_uur_min <- PV_Profiel_uur_min[,-c(1:2)]
# PV_Profiel_uur_min <- PV_Profiel_uur_min[order(as.Date(PV_Profiel_uur_min$datum, format="%d-%m-%Y")),]
# 
# WP_Profiel_uur_min <- aggregate(WP_Profiel, list(Datum = WP_Profiel$datum, Uur = WP_Profiel$uur), min)
# WP_Profiel_uur_min <- WP_Profiel_uur_min[,-c(1:2)]
# WP_Profiel_uur_min <- WP_Profiel_uur_min[order(as.Date(WP_Profiel_uur_min$datum, format="%d-%m-%Y")),]
# 
# 
# `%ni%` <- Negate(`%in%`) 
# 
# TimeStamp_Tijd <- paste(substr(EV_Profiel_uur$datum, 1, 5), EV_Profiel_uur_max$uur, sep = " ")
# EDSN_Tijd <- paste(substr(EDSN_profiel_uur$datum, 1, 5), EDSN_profiel_uur_max$uur, sep = " ")
# 
# EDSN_profiel_uur_max <- EDSN_profiel_uur_max[-which(EDSN_Tijd %ni% TimeStamp_Tijd),]
# EV_Profiel_uur_max <- EV_Profiel_uur_max[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# PV_Profiel_uur_max <- PV_Profiel_uur_max[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# WP_Profiel_uur_max <- WP_Profiel_uur_max[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# 
# EDSN_profiel_uur_min <- EDSN_profiel_uur_min[-which(EDSN_Tijd %ni% TimeStamp_Tijd),]
# EV_Profiel_uur_min <- EV_Profiel_uur_min[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# PV_Profiel_uur_min <- PV_Profiel_uur_min[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# WP_Profiel_uur_min <- WP_Profiel_uur_min[-which(TimeStamp_Tijd %ni% EDSN_Tijd),]
# 
# #Opslaan om te voorkomen dat dit nog een keer moet gebeuren
# 
# write.table(EDSN_profiel_uur_max, "EDSN_Profiel_uur_final_max.csv", sep = ";", row.names = FALSE)
# write.table(EV_Profiel_uur_max, "EV_Profiel_uur_final_max.csv", sep = ";", row.names = FALSE)
# write.table(PV_Profiel_uur_max, "PV_Profiel_uur_final_max.csv", sep = ";", row.names = FALSE)
# write.table(PV_Profiel_uur_max, "WP_Profiel_uur_final_max.csv", sep = ";", row.names = FALSE)
# 
# 
# write.table(EDSN_profiel_uur_min, "EDSN_Profiel_uur_final_min.csv", sep = ";", row.names = FALSE)
# write.table(EV_Profiel_uur_min, "EV_Profiel_uur_final_min.csv", sep = ";", row.names = FALSE)
# write.table(PV_Profiel_uur_min, "PV_Profiel_uur_final_min.csv", sep = ";", row.names = FALSE)
# write.table(PV_Profiel_uur_min, "WP_Profiel_uur_final_min.csv", sep = ";", row.names = FALSE)

####

## Final belastingprofielen om in model te gebruiken

EDSN_profiel_uur_max <- read.table("EDSN_Profiel_uur_final_max.csv", sep = ";", header = TRUE)
EV_Profiel_uur_max <- read.table("EV_Profiel_uur_final_max.csv", sep = ";", header = TRUE)
PV_Profiel_uur_max <- read.table("PV_Profiel_uur_final_max.csv", sep = ";", header = TRUE)
WP_Profiel_uur_max <- read.table("WP_Profiel_uur_final_max.csv", sep = ";", header = TRUE)

EDSN_profiel_uur_min <- read.table("EDSN_Profiel_uur_final_min.csv", sep = ";", header = TRUE)
EV_Profiel_uur_min <- read.table("EV_Profiel_uur_final_min.csv", sep = ";", header = TRUE)
PV_Profiel_uur_min <- read.table("PV_Profiel_uur_final_min.csv", sep = ";", header = TRUE)
WP_Profiel_uur_min <- read.table("WP_Profiel_uur_final_min.csv", sep = ";", header = TRUE)

####Als laatste gegevens voor GV'ers inkloppen, in

##BEGIN MODEL PER NETVLAK

#Array prealloceren om de Resultaten voor LS_Hld in op te slaan, twee kolommen per jaar voor de min en de max + de base, EV en PV toevoeging (6 kolommen per jaar dus), de verschillende scenario's moeten we denk ik in verschillende resultaatbestanden doen om het overzicht te bewaren

#GENERIEKE FUNCTIE MAKEN OM DE NETVLAKKEN DOOR TE REKENEN
#TODO: -Per netvlak functie ook de kwaliteitsaspecten meenemen (kijken hoe lang een bepaalde belasting optreed en kijken of deze niet te lang optreed voor bepaald netcomponenten)


EDSN_profiel_uur_max_bare <- sapply(EDSN_profiel_uur_max[,3:12], function(x) as.numeric(x))/0.9 #delen door 0.9 is vanwege de conversie naar kVA
EDSN_profiel_uur_min_bare <- sapply(EDSN_profiel_uur_min[,3:12], function(x) as.numeric(x))/0.9 #delen door 0.9 is vanwege de conversie naar kVA


NetVlakFunctieMax <- function(AantalComponenten, BaseL, EVPenGr, PVPenGr, WPPenGr) {

  #INPUTS
  #AantalComponenten; is een lijst met het aantal componenten waarover moet worden geitereerd
  #BaseL; data.frame met BaseL gegevens per Netvlak
  #EVPenGr, PVPenGr, WPPenGr; data.frame met PenetratieGraden per scenario en Netvlak
  
  #Deze functie gaat ervan uit dat de EDSN, EV, PV, en WP belastingprofielen al zijn ingeladen
  
  #OUTPUT
  #Deze functie geeft als Output een matrix met daarin alle voorspellingen per jaar per netvlak component
  
#   BaseL <- sapply(BaseL[,c(2:11)], function(x) as.numeric(x)) 
  OutputMatrix <- matrix(ncol = 5*16, nrow = length(AantalComponenten))
  
  #Beginnen met het maken van de BaseL, omdat deze maar een keer hoeft te worden gemaakt
  for (i in 1:length(AantalComponenten)) {
    Baseload <- rowSums(EDSN_profiel_uur_max_bare%*%diag(BaseL[i,2:11]))
    for (Jr in 1:16) {
      Tot_Profiel <- Baseload + EVPenGr[i,Jr+1]*EV_Profiel_uur_max$V1 + PVPenGr[i,Jr+1]*PV_Profiel_uur_max$V1 + WPPenGr[i,Jr+1]*WP_Profiel_uur_max$V1
      
      #Eerste kolom is voor de max
      OutputMatrix[i,5*(Jr-1)+1] <- max(Tot_Profiel) #Jr begint bij 3 dus -2-1 = -3
      #Daarna voor de bijdragen van Baseload, EV, PV, en WP
      MaxIndex <- which.max(Tot_Profiel)
      OutputMatrix[i,5*(Jr-1)+2] <- Baseload[MaxIndex]
      OutputMatrix[i,5*(Jr-1)+3] <- EV_Profiel_uur$V1[MaxIndex]
      OutputMatrix[i,5*(Jr-1)+4] <- PV_Profiel_uur$V1[MaxIndex]
      OutputMatrix[i,5*(Jr-1)+5] <- WP_Profiel_uur$V1[MaxIndex]
      
      #Hier komt ook nog een stuk waarbij de kwaliteitsfactoren van kabels kan worden gecheckt
      
      #Opruimen
#       rm(Tot_Profiel)
#       rm(MaxIndex)
    }
#     rm(Baseload)
  }

  return(OutputMatrix)
  
}
NetVlakFunctieMin <- function(AantalComponenten, BaseL, EVPenGr, PVPenGr, WPPenGr) {
  
  #INPUTS
  #AantalComponenten; is een lijst met het aantal componenten waarover moet worden geitereerd
  #BaseL; data.frame met BaseL gegevens per Netvlak
  #EVPenGr, PVPenGr, WPPenGr; data.frame met PenetratieGraden per scenario en Netvlak
  
  #Deze functie gaat ervan uit dat de EDSN, EV, PV, en WP belastingprofielen al zijn ingeladen
  
  #OUTPUT
  #Deze functie geeft als Output een matrix met daarin alle voorspellingen per jaar per netvlak component
  
  #   BaseL <- sapply(BaseL[,c(2:11)], function(x) as.numeric(x)) 
  OutputMatrix <- matrix(ncol = 5*16, nrow = length(AantalComponenten))
  
  #Beginnen met het maken van de BaseL, omdat deze maar een keer hoeft te worden gemaakt
  for (i in 1:length(AantalComponenten)) {
    Baseload <- rowSums(EDSN_profiel_uur_min_bare%*%diag(BaseL[i,2:11]))
    for (Jr in 1:16) {
      Tot_Profiel <- Baseload + EVPenGr[i,Jr+1]*EV_Profiel_uur_min$V1 + PVPenGr[i,Jr+1]*PV_Profiel_uur_min$V1 + WPPenGr[i,Jr+1]*WP_Profiel_uur_min$V1
      
      #Eerste kolom is voor de max
      OutputMatrix[i,5*(Jr-1)+1] <- min(Tot_Profiel) #Jr begint bij 3 dus -2-1 = -3
      #Daarna voor de bijdragen van Baseload, EV, PV, en WP
      MinIndex <- which.min(Tot_Profiel)
      OutputMatrix[i,5*(Jr-1)+2] <- Baseload[MinIndex]
      OutputMatrix[i,5*(Jr-1)+3] <- EV_Profiel_uur$V1[MinIndex]
      OutputMatrix[i,5*(Jr-1)+4] <- PV_Profiel_uur$V1[MinIndex]
      OutputMatrix[i,5*(Jr-1)+5] <- WP_Profiel_uur$V1[MinIndex]
      
      #Hier komt ook nog een stuk waarbij de kwaliteitsfactoren van kabels kan worden gecheckt
      
      #Opruimen
      #       rm(Tot_Profiel)
      #       rm(MaxIndex)
    }
    #     rm(Baseload)
  }
  
  return(OutputMatrix)
  
}

#Rprof is om te testen wat de performance van een bepaalde functie is

Rprof("Netvlak_func.out")
TestNetVlak <- NetVlakFunctieMax(c(1:1000), LS_Hld_BaseL, LS_Hld_PenGr_EV_L, LS_Hld_PenGr_PV_L, LS_Hld_PenGr_WP_L)
Rprof()
summaryRprof("Netvlak_func.out")
##
## EERST LS_KABELS
##


# Stukje om de kolomnamen van de tabel toe te voegen uit de functie

KolomNamenJaren <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030")
KolomNamenMaxMin <- c("_max_belasting", "_max_BaseL_bijdrage", "_max_EV_bijdrage", "_max_PV_bijdrage", "_min_belasting", "_min_BaseL_bijdrage", "_min_EV_bijdrage", "_min_PV_bijdrage")

KolomNamenTabel <- as.vector(sapply(KolomNamenJaren, function(x) paste(x, KolomNamenMaxMin, sep = "")))

colnames(Resultaten_LS_H) <- KolomNamenTabel


#TODO: -Als de resultaten klaar zijn moeten de de gegevens van de netcomponenten er nog worden aangeplakt om te kijken of er overschrijdingen etc optreden etc, aanzet hiervoor gebeurt in onderstaande code



###Stuk voor MSR'en

##Data van MSR'en
setwd("\\\\fspaka01/userdata3b$/AL8368/Documents/Advies Groep/NieuwConnectiviteitsmodel/RuweData/")
MS_Stations <- read.csv("MS_stations_Flevoland.csv", sep = ";", dec = ",")

Unieke_MSR <- data.frame(MSR_nummer = unique(LS_Hld_PC6$MSR_BEHUIZING))

ML_Unieke_MSR_MS_Stations <- sapply(Unieke_MSR$MSR_nummer, function(x) match(x, MS_Stations$NRBEHUIZIN))

Unieke_MSR$Naam_Ruimte <- MS_Stations$NAAMRUIMTE[ML_Unieke_MSR_MS_Stations]
Unieke_MSR$Vermogen <- MS_Stations$TOTVERMOGE[ML_Unieke_MSR_MS_Stations]
Unieke_MSR$TypeMSR <- MS_Stations$TYPEMSR[ML_Unieke_MSR_MS_Stations]
Unieke_MSR$FunctieMSR <- MS_Stations$GEMEENTE[ML_Unieke_MSR_MS_Stations]
Unieke_MSR$Postcode <- MS_Stations$POSTCODE[ML_Unieke_MSR_MS_Stations]

#Aan PC6 koppelen zoals met Ls_Hld

ML_MSR_NR_aan_LS_Hld_PC6 <- sapply(Unieke_MSR$MSR_nummer, function(x) which(LS_Hld_PC6$MSR_BEHUIZING %in% x == TRUE))

#Zijn er nog PC6 die dubbel onder een MSR staan? Zo ja, maakt niet uit want deze worden door de normering meegenomen

#Match lijst maken tussen de PC6'en van ls_hld en van de K&M prognoses

ML_LS_Hld_PC6_aan_Postcode6 <- sapply(LS_Hld_PC6$PC6, function(x) which(Postcode6$V1 %in% x  == TRUE))

## PLAKSTUK LS_HLD
#Met dit stuk kunnen overbelastingen etc worden bepaald


LS_Hld_PC6 <- read.table("ls_hld_pc6.csv", sep = ";", header = TRUE, colClasses= "character")
LS_Alle_Ld <- read.csv("MSR_FLEVO_LSHLD_Aansl.csv", sep = ";", dec = ",")
KabelTypen <- read.csv("kabelgegevens.csv", sep = ";", dec = ",")
KabelMatch <- read.csv("kabelgegevens_compl.csv", sep = ";", dec =",")

# Stuk om de missende MSR'en te koppelen aan de LS_Hld_PC6

null_lijst <- which(LS_Hld_PC6$MSR_BEHUIZING == "") 
LS_Hld_PC6$MSR_BEHUIZING[null_lijst] <- unlist(sapply(LS_Hld_PC6$ls_hld_id[null_lijst], function(x) LS_Alle_Ld$NR_Behuizing[which(LS_Alle_Ld$ls_hld_id == x)[1]]))


#Sorteren per meest belastbare kabel

KabelMatch <- KabelMatch[with(KabelMatch, order(-Inom)), ]

#Vervolgens meest belastbare kabel selecteren, voor methode ordering zie http://stackoverflow.com/questions/1568511/how-do-i-sort-one-vector-based-on-values-of-another

BelastbareKabelFunctie <- function (x) {
  TestLijst <- sapply(strsplit(as.character(LS_Alle_Ld$uitvoering[which(LS_Alle_Ld$ls_hld_id %in% x == TRUE)]), "\\+"), "[[", 1)
  if(length(TestLijst) == 0) { return("Geen Match")}
  else {return(as.vector(unlist(TestLijst[order(match(TestLijst, KabelMatch$KabelType))][1])))}
}


LS_Hld_PC6$KabelType <- sapply(LS_Hld_PC6$ls_hld_id, BelastbareKabelFunctie)

## y[sort(order(y)[x])] zie http://stackoverflow.com/questions/1568511/how-do-i-sort-one-vector-based-on-values-of-another

LS_Hld_PC6$MaxInom <- sapply(LS_Hld_PC6$KabelType, function(x) KabelMatch$Inom[which(KabelMatch$KabelType == x)[1]])


# Lijst unieke HD kabels maken, met gegevens van de LS_hld's 

Unieke_Ls_Hld <- data.frame(ls_hld_id = unique(LS_Hld_PC6$ls_hld_id))

Match_Lijst_LS_hd <- sapply(Unieke_Ls_Hld$ls_hld_id, function(x) match(x, LS_Hld_PC6$ls_hld_id))

Unieke_Ls_Hld$hoofdleiding <- LS_Hld_PC6$hoofdleiding[Match_Lijst_LS_hd]
Unieke_Ls_Hld$lengte <- LS_Hld_PC6$lengte[Match_Lijst_LS_hd]
Unieke_Ls_Hld$aansluitingen <- LS_Hld_PC6$aansluitingen[Match_Lijst_LS_hd]
Unieke_Ls_Hld$MSR_BEHUIZING <- LS_Hld_PC6$MSR_BEHUIZING[Match_Lijst_LS_hd]
Unieke_Ls_Hld$VELD_NR <- LS_Hld_PC6$VELD_NR[Match_Lijst_LS_hd]
Unieke_Ls_Hld$BEV_NORM <- LS_Hld_PC6$BEV_NORM[Match_Lijst_LS_hd]
Unieke_Ls_Hld$BMR_FUNCTIE <- LS_Hld_PC6$BMR_FUNCTIE[Match_Lijst_LS_hd]
Unieke_Ls_Hld$BMR_SUBFUNCTIE <- LS_Hld_PC6$BMR_SUBFUNCTIE[Match_Lijst_LS_hd]
Unieke_Ls_Hld$PC6 <- LS_Hld_PC6$PC6[Match_Lijst_LS_hd]
Unieke_Ls_Hld$KabelType <- LS_Hld_PC6$KabelType[Match_Lijst_LS_hd]
Unieke_Ls_Hld$MaxInom <- LS_Hld_PC6$MaxInom[Match_Lijst_LS_hd]

#Deze "basis" tabel opslaan zodat deze niet drie keer in R hoeft te worden cbind()

write.table(Unieke_Ls_Hld, "Unieke_LS_Hld.csv", sep = ";", dec = ",")

###TODO: -Er moet ook nog een stuk in de tabel dat er rekening wordt gehouden met het verschil tussen het aantal huishoudens uit CBS data en aansluitingen data

ML_Unieke_Hld_aan_LS_Hld_PC6 <- sapply(Unieke_Ls_Hld$ls_hld_id, function(x) which(LS_Hld_PC6$ls_hld_id %in% x == TRUE))

#Match lijst maken tussen de PC6'en van ls_hld en van de K&M prognoses

ML_LS_Hld_PC6_aan_Postcode6 <- sapply(LS_Hld_PC6$PC6, function(x) which(Postcode6$V1 %in% x  == TRUE))

#Nu kunnen de prognoses aan de assets worden gekoppeld,

##NORMALISEREN PER PC6 (Een PC6 kan meerdere hoofdleidingen door zich heen hebben lopen, dus dit moet worden genormaliseerd per PC6)

#Checken of er in de Ls_hld lijst dubbelingen zitten, ZIJN ER NIET

