# ##############################################################################################################
#
#    Definiere globale Variablen
#
# ##############################################################################################################

# temporär zur Fehlerbehandlung
mytest <- NULL

# --------------------------------------------------------------------------------------------------------------

# Fuer die Berechnungen eines Jahres
einJahr <- 365

# Dataframe fuer alle eingelesenen Daten aus den Trainingseinheiten
viewportDF <- NULL

# Vokabelliste als Dataframe
dic <- read.csv2("languages.csv", header=TRUE, stringsAsFactors = FALSE, row.names=1, fileEncoding="UTF-8")
availableLang <- names(dic)
numDics <- length(availableLang)
iLang <- 1                                                      # TEMP: Abwaertskompatibilitaet zu alter Version

# Variablen für die Herzfrequenzeinstellungen festelegen
hfMaxGeneral <- 220
hfBereiche <- list("minimal" = 1,"leicht" = 2, "moderat" = 3, "schwer" = 4, "sehr schwer" = 5, "maximal" = 6)
heartRateLimits <- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (hfMaxGeneral - 40) + 40

# Header-Vorgabe für Datenimport
myHeader <- c("Time", "Id", "HeartRateBpm", "LatitudeDegrees", "LongitudeDegrees",
              "AltitudeMeters", "DistanceMeters")
basicCol <- c("#ffcc00", "#00ff00", "#ff0000")

# --------------------------------------------------------------------------------------------------------------



# ##############################################################################################################
#
#    Definiere globale Funktionen (ohne Shiny-Interaktion/Reactivity)
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

# stellt HTML aus Datei zum Rendern bereit
getPage <- function(filename) {
  return(includeHTML(filename))
}

# --------------------------------------------------------------------------------------------------------------

# stellt zu keyword und Sprache die passende Vokabel bereit; wird keyword nicht gefunden -> return keyword
translate <- function(keyword, languageID = 0) {
  i <- ifelse(languageID == 0, iLang, languageID)               # TEMP: Abwaertskompatibilitaet zu alter Version
  word <- dic[keyword, availableLang[i]]
  return(ifelse(is.na(word), keyword, word))
}

# --------------------------------------------------------------------------------------------------------------

showMessage <- function(msg, languageID) {
  part <- strsplit(msg, "@")[[1]]
  showModal(modalDialog(title = translate(part[2], languageID), translate(part[3], languageID),
                        easyClose = TRUE, footer = NULL))
}

# --------------------------------------------------------------------------------------------------------------

# extrahiert aus CSV-Datei einen Dataframe
importDataCSV <- function(myFile) {
  # TODO
  return(NULL)
}

# --------------------------------------------------------------------------------------------------------------

# extrahiert dataframe aus gpxfile; bei fehlerhaften Daten wird insgesamt NULL zurückgeliefert
importDataGPX <- function(gpxfile) {
  newData <- NULL
  doc <- xmlParse(gpxfile)
  nodes <- getNodeSet(doc, "//ns:trkpt", "ns")
  totalsize <- xmlSize(nodes)
  if (totalsize>0) {
    for (i in 1:totalsize) {
      node <- nodes[[i]]
      x <- c(Time = xmlValue(node[["time"]]), LatitudeDegrees = xmlAttrs(node)[["lat"]],
             LongitudeDegrees = xmlAttrs(node)[["lon"]], AltitudeMeters = xmlValue(node[["ele"]]))
      newData <- rbind(newData, x)
    }
    
    # konvertiere Matrix zu Dataframe
    newData <- as.data.frame(newData, stringsAsFactors = FALSE)
    row.names(newData) <- NULL
    
    # erweitere fehlende Spalten und sortiert neu
    newData$Id <- NA
    newData$HeartRateBpm <- NA
    newData$DistanceMeters <- NA
    newData <- newData[myHeader]
  }
  
  return(newData)
}

# --------------------------------------------------------------------------------------------------------------

# testet Dateityp und liefert Typ oder Fehlermeldung als String
checkFileformat <- function(myFile) {

  if (myFile$type == "text/csv" & sub(".*\\.", "", myFile$name)=="csv") {
    
    # Typ ist CSV-Datei, teste auf Polar-Format
    polarheader1 <- paste0("Name,Sport,Date,Start time,Duration,Total distance (km),Average heart rate (bpm),A",
                           "verage speed (km/h),Max speed (km/h),Average pace (min/km),Max pace (min/km),Calor",
                           "ies,Fat percentage of calories(%),Average cadence (rpm),Average stride length (cm)",
                           ",Running index,Training load,Ascent (m),Descent (m),Notes,Height (cm),Weight (kg),",
                           "HR max,HR sit,VO2max,")
    polarheader2 <- paste0("Sample rate,Time,HR (bpm),Speed (km/h),Pace (min/km),Cadence,Altitude (m),Stride l",
                           "ength (m),Distances (m),Temperatures (C),Power (W),")
    
    conn <- file(myFile$datapath, open="r")
    rawdata <- readLines(conn)
    close(conn)
    n <- length(rawdata)
    
    if (n == 0) {
      msg <- "@File is empty;"
    } else if (n >= 3 && rawdata[1] == polarheader1 && rawdata[3] == polarheader2) {
      msg <- "CSV"
    } else {
      msg <- "@Wrong CSV format;"
    }
    
  } else if (myFile$type == "" & sub(".*\\.", "", myFile$name)=="tcx") {
    
    # Typ ist TCX-Datei
    msg <- "TCX"
    
  } else if (myFile$type == "" & sub(".*\\.", "", myFile$name)=="gpx") {
    
    # Typ ist GPX-Datei
    msg <- "GPX"
    
  } else {
    
    # Typ ist ... keine Ahnung
    msg <- "@Wrong file format. coroVis supports TCX, GPX, CSV only;"
  }
  
  return(msg)
}

# --------------------------------------------------------------------------------------------------------------
