hfMaxGeneral <- 200

# ##############################################################################################################
#
#    Definiere globale Variablen
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

# Dataframe fuer alle eingelesenen Daten aus den Trainingseinheiten
viewportDF <- NULL

# Vokabelliste als Dataframe
dic <- read.csv2("languages.csv", header=TRUE, stringsAsFactors = FALSE, row.names=1)
availableLang <- names(dic)
numDics <- length(availableLang)
iLang <- 1                                                      # TEMP: Abwaertskompatibilitaet zu alter Version

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

# stellt zu keyword und aktueller Sprache die passende Vokabel bereit
translate <- function(keyword, languageID = 0) {
  i <- ifelse(languageID == 0, iLang, languageID)               # TEMP: Abwaertskompatibilitaet zu alter Version
  return(dic[keyword, availableLang[i]])
}

# --------------------------------------------------------------------------------------------------------------

# extrahiert aus CSV-Datei einen Dataframe
importDataCSV <- function(myFile) {
  # TODO
  return(NULL)
}

# --------------------------------------------------------------------------------------------------------------

# extrahiert aus GPX-Datei einen Dataframe
importDataGPX <- function(myFile) {
  # TODO
  return(NULL)
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
      msg <- "@Import error@File is empty"
    } else if (n >= 3 && rawdata[1] == polarheader1 && rawdata[3] == polarheader2) {
      msg <- "CSV"
    } else {
      msg <- "@Import error@Wrong CSV format"
    }
    
  } else if (myFile$type == "" & sub(".*\\.", "", myFile$name)=="tcx") {
    
    # Typ ist TCX-Datei
    msg <- "TCX"
    
  } else if (myFile$type == "" & sub(".*\\.", "", myFile$name)=="gpx") {
    
    # Typ ist GPX-Datei
    msg <- "GPX"
    
  } else {
    
    # Typ ist ... keine Ahnung
    msg <- "@Import error@Wrong file format. coroVis supports TCX, GPX, CSV only."
  }
  
  return(msg)
}

# --------------------------------------------------------------------------------------------------------------
