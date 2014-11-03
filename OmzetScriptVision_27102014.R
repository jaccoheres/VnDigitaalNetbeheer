# SCRIPT OM MSR opbossing uit het model naar Vision inputs om te zetten
# Geschreven door Tim Lucas (tim.lucas@alliander.com)
# In het Vision Input bestand moeten per knooppunt en belastingtype de belastingen in kVA staan

# rm(list=ls(all=TRUE))

# Zet de directory waarin moet worden gewerkt hieronder, hierin moeten de Vision bestanden staan en de 
setwd("\\\\fspaka01/userdata3b$/AL8368/Documents/Advies Groep/Analyses Werkgroep AM/Risicopunten/")

#Mocht het nodig zijn dan wordt de Eindlijst_MSR hieronder ingeladen, dit is de final lijst waarin alle scenario's staan van de MSR'en

Eindlijst_MSR <- MSR_Totaal #In dit geval hebben we dus geen gebruik gemaakt van de resultaten uit het compleet_model script

# Eindlijst_MSR <- read.table("Eindlijst_MSR_METGVMSR_METZAKKV_08102014.csv", sep = ";", dec = ",", header = TRUE)

# Hieronder worden alle koppel bestanden met Vision ingeladen, deze moeten dus nog wel worden aangepast aan de bestanden van Noord-Holland

Vision_Namen <- read.csv("Vision_Nieuwe_Namen.csv", sep = ";", dec =",", stringsAsFactors = FALSE) #In dit bestand worden de Vision Namen met de Ruimte nummers ingeladen
Vision_Belastingen <-  read.table("Vision_Oude_Belastingen.csv", sep = ";", dec = ",", stringsAsFactors= FALSE, header = TRUE) #Dit bestand kan worden gebruikt om de belastingen over te zetten naar het input bestand van Vision
Vision_Belastingen_2 <-  read.table("Vision_Oude_Belastingen_2.csv", sep = ";", dec = ",", stringsAsFactors= FALSE, header = TRUE) #Dit bestand kan worden gebruikt om de belastingen over te zetten 
Vision_Belastingen <- rbind(Vision_Belastingen, Vision_Belastingen_2)
Vision_Belastingen$kVA <- sqrt((as.numeric(as.character(Vision_Belastingen$P))^2)+(as.numeric(as.character(Vision_Belastingen$Q))^2))*1000 #Als de belastingen worden overgezet moet er wel worden gerekend met de juiste grootheden, hier worden de P en Q waarden omgezet naar S (in kVA)

#De onderste twee lijsten koppelen de MSR namen uit de resultaten aan de Vision namen, soms wordt in het Vision_Namen bestand echter gebruik gemaakt van het NR_behuizing ipv NR_ruimte, op beiden worden dus geprobeerd te matchen

ML_1 <- sapply(Eindlijst_MSR$MSR_nummer, function(x) match(x, Vision_Namen$NR_RUIMTE))
ML_2 <- sapply(Eindlijst_MSR$NummerRuimte, function(x) match(x, Vision_Namen$NR_RUIMTE))

#Waar er geen match kan worden gemaakt in de eerste lijst (die als leidend wordt genomen), wordt er getracht toch een match te krijgen door de NA's in de eerste lijst te verplaatsen met de waardes in de tweede ML (MatchLijst)

ind_lijst <- which(is.na(ML_1))
ML_1[ind_lijst] <- ML_2[ind_lijst]

#Aan de Eindlijst wordt een kolom met Vision Namen toegevoegd om de match compleet te kunnen maken

Eindlijst_MSR$VisionNaam <- Vision_Namen$Naam.Vision[ML_1]

#Mocht er nu nog geen match bestaan dan wordt er geprobeerd nog via een behuizingsnaam een match te maken, daarna wordt de lijst compleet gemaakt

ind_lijst2 <- which(is.na(Eindlijst_MSR[,"VisionNaam"]))

Eindlijst_MSR$VisionNaam[ind_lijst2] <- as.character(Eindlijst_MSR$Naam_Ruimte[ind_lijst2])
ML_Namen_Belasting <- sapply(Eindlijst_MSR$VisionNaam, function(x) match(x, Vision_Belastingen$Knooppunt.Naam))
Eindlijst_MSR$VisionBelasting <- Vision_Belastingen$kVA[ML_Namen_Belasting]

#Vervolgens moeten we selecteren welke scenario's we willen analyseren en deze omzetten in een input die Vision kan lezen, hieronder kunnen de jaren en de scenario's worden geselecteerd
#Vision verwacht een bestand met daarin de scenario's als jaren in de eerste kolom, met daarna een uur indicatie en alle volgende kolommen zijn de belastingen per knooppunt

KolomNamenResultaten <- colnames(Eindlijst_MSR)
# ScenarioSelect <- c("LoW_EV_LoW_PV_LoW_WP", "Med_EV_Med_PV_Med_WP", "Max_EV_High_PV_High_WP", "High_EV_High_PV_High_WP", "LoW_EV_High_PV_LoW_WP")
ScenarioSelect <- c("Laag_Scenario", "Med_Scenario", "Hoog_Scenario")
Jaar <- c("2015", "2016", "2017","2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030")
JarenNamen <- sapply(ScenarioSelect, function(x) paste(x, Jaar, sep = "_"))
SubLijst <- KolomNamenResultaten[sapply(sapply(JarenNamen, function(x) grep(x, KolomNamenResultaten)), function(y) y[[1]][1])] #Deze lijst maakt een voorselectie van alle kolomnamen van de resultaatstabel die we willen selecteren
setwd("\\\\fspaka01/userdata3b$/AL8368/Documents/Advies Groep/Analyses Werkgroep AM/Final Gegevens/")

  
  #De VisionMatrix is de uiteindelijke vorm waarin de data wordt opgeleverd, het aantal kolommen is dus wel afhankelijk van het aantal type belastingen per knooppunt in Vision (.EV, .PV, .WP, etc)
  #Het aantal rijen is daarbij een vermenigvuldiging van het aantal Jaren maal het aantal Scenario's. Elk jaar krijgt namelijk zijn eigen scenario

  VisionMatrix <- matrix(ncol = 4*nrow(Eindlijst_MSR), nrow = length(Jaar)*length(ScenarioSelect)) 
  
  # De eerste loop wordt een selectie van het Scenario gemaakt
  for (k in 1:length(ScenarioSelect)){
    SubLijst2 <- SubLijst[grep(ScenarioSelect[k], SubLijst)] #SubLijst2 wordt 
    for (j in 1:length(Jaar)) {
      KolomNamenSelecteren <- as.vector(sapply(c("_BaseL_bijdrage", "_EV_bijdrage", "_PV_bijdrage", "_WP_bijdrage"), function(x) SubLijst2[grep(x, SubLijst2[grep(Jaar[j], SubLijst2)])])) #If is == 0 hierbij
      #Vervolgens moeten hieronder de gegevens komen waarbij we telkens een rij per verschillend scenario krijgen met eerste de baseload en dan de EV, PV en WP bijdrage, hierbij is de base load dus .bel1 en NIET het totaal!!!
      #Het jaar wordt geselecteerd dmv de teller j+(length(ScenarioSelect)-k)*length(Jaar), dit betekent dus dat het eerste scenario het laatste jaar is!
      VisionMatrix[j+(length(ScenarioSelect)-k)*length(Jaar),] <- as.vector(sapply(KolomNamenSelecteren, function(x) (Eindlijst_MSR[,x])))
      
    }
  }
  #Nu de complete VisionMatrix is gevuld moeten de kolom namen worden toegevoegd zodat Vision de namen als juist herkent
  colnames(VisionMatrix) <- as.vector(sapply(c(".Bel1", ".EV", ".PV", ".WP"), function(x) paste(Eindlijst_MSR$VisionNaam, x, sep = "")))
  #Nu de jaren toevoegen (in de eerste kolom om Vision te vertellen welke scenario's er moeten worden gebruikt)
  Jaar2 <- as.vector(sapply(1:length(ScenarioSelect), function(x) (x-1)*100+as.numeric(Jaar))) #Hier nog vermenigvuldigingen voor verzinnen
  VisionMatrix <- cbind(cbind(paste("01-01-", Jaar2, sep = ""), rep("09.00", length(Jaar))), VisionMatrix)
  colnames(VisionMatrix) <- c(c("Datum", "Tijd"), colnames(VisionMatrix[,-c(1,2)]))
  VisionMatrix <- gsub(".", ",", VisionMatrix, fixed = TRUE)
  write.table(VisionMatrix, paste("Alle_Stations", "_VisionImport_max.csv"), sep = ";", dec = ",", row.names = FALSE)



#Ik probeer hier alleen nog de MSR'en toe te voegen die wel een belasting hebben maar kennelijk niet te matchen zijn aan de namen oid, dit kan dus handig zijn om het bestand op te vullen als er nog belastingen ontbreken
`%ni%` <- Negate(`%in%`)
OntbrekendeBel <- which(Vision_Belastingen$Knooppunt.Naam %ni% Eindlijst_MSR$VisionNaam)

OntbrekendeBelMat <- matrix(ncol = length(OntbrekendeBel), nrow = nrow(VisionMatrix))

for(i in 1:nrow(VisionMatrix)){
OntbrekendeBelMat[i,] <- Vision_Belastingen$kVA[OntbrekendeBel]
}

colnames(OntbrekendeBelMat) <- paste(Vision_Belastingen$Knooppunt.Naam[OntbrekendeBel], ".Bel1", sep = "")
VisionMatrix <- cbind(VisionMatrix, OntbrekendeBelMat)

VisionMatrix <- gsub(".", ",", VisionMatrix, fixed = TRUE)
write.table(VisionMatrix, paste("Alle_Stations", "_VisionImport_max.csv"), sep = ";", dec = ",", row.names = FALSE)