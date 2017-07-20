# ##############################################################################################################
#
#    Definiere globale Variablen
#
# ##############################################################################################################

# temporär zur Fehlerbehandlung
mytest <- NULL

# --------------------------------------------------------------------------------------------------------------

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
polarheader1 <- paste0("Name,Sport,Date,Start time,Duration,Total distance (km),Average heart rate (bpm),A",
                       "verage speed (km/h),Max speed (km/h),Average pace (min/km),Max pace (min/km),Calor",
                       "ies,Fat percentage of calories(%),Average cadence (rpm),Average stride length (cm)",
                       ",Running index,Training load,Ascent (m),Descent (m),Notes,Height (cm),Weight (kg),",
                       "HR max,HR sit,VO2max,")
polarheader2 <- paste0("Sample rate,Time,HR (bpm),Speed (km/h),Pace (min/km),Cadence,Altitude (m),Stride l",
                       "ength (m),Distances (m),Temperatures (C),Power (W),")
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

# berechnet BMI aus Groesse [cm] und Gewicht [kg]
calculateBMI <- function(groesse, gewicht) {
  return(round(gewicht/((groesse/100)^2), 1))
}

# --------------------------------------------------------------------------------------------------------------

# berechnet Alter aus date-of-birth
calculateAge <- function(dob) {
  if (is.null(dob)) {return(NULL)} else {return(round(difftime(Sys.time(), dob)/365 , 1))}
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

# helper function: converts HH:MM:SS to seconds
getSeconds <- function(raw) {
  elem <- unlist(strsplit(raw, ":"))
  return(strtoi(elem[1], base = 10)*3600 + strtoi(elem[2], base = 10)*60 + strtoi(elem[3], base = 10))
}

# helper function: converts relative time to absolute date-time
# @startdate: DD-MM-YYYY
# @starttime: HH:MM:SS
getDateTime <- function(startdate, starttime, reltime, difftime=0) {
  start <- as.POSIXct(paste(startdate, starttime, sep="T"), format = "%d-%m-%YT%H:%M:%S", tz="UTC")
  return(as.character(start + getSeconds(reltime) + 1 - (difftime*3600)))
}

# --------------------------------------------------------------------------------------------------------------

mergeDF <- function(old, new, mergeBy) {
  
  #temporary:
  timeZ <-   grepl("(\\.[0-9]{3})?Z|(\\.[0-9]{3}?\\+[0-9]{2}:[0-9]{2})", new$Time)
  new$Time <- sub("(\\.[0-9]{3})?Z|(\\.[0-9]{3}?\\+[0-9]{2}:[0-9]{2})", "", new$Time)
  new$Time <- as.POSIXct(new$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  
  
  # assertion: ncol(old)==ncol(new); colnames(old)==colnames(new)
  i <- which(colnames(old)==mergeBy)
  cols <- ncol(old)
  
  # merge dataframes
  merged <- merge(old, new, by = mergeBy, all = TRUE)
  mytest <<- merged
  for (k in 2:cols) {
    merged[,k] <- ifelse(is.na(merged[,k]), merged[,k+cols-1], merged[,k])
  }
  
  # keep only first ncol(old) columns
  merged <- merged[,1:cols]
  
  # rename colnames: mergeby, colnames without mergeby
  colnames(merged) <- c(mergeBy, colnames(old)[-i])
  
  return(merged[colnames(old)])
}

# --------------------------------------------------------------------------------------------------------------

# extrahiert aus CSV-Datei einen Dataframe
importDataCSV <- function(myFile) {
  # to be safe: read data as raw textfile
  conn <- file(myFile, open="r")
  rawdata <- readLines(conn)
  close(conn)
  n <- length(rawdata)
  
  
  
  # evaluate rawdata
  if (n == 0) {
    return(NULL) # "File is empty"
  } else if (n >= 3) {
    if (rawdata[1] == polarheader1 & rawdata[3] == polarheader2) {
      # Polar CSV detected
      mydf <- read.csv(text=paste(rawdata[1], rawdata[2], sep = "\n"), stringsAsFactors = FALSE)
      startdate <- mydf[1, c(3:4)]
      mydf <- read.csv(myFile, skip = 2, header = TRUE, stringsAsFactors = FALSE)
      mydf <- mydf[,c(2:3,7,9)]
      colnames(mydf) <- c("Time", "HeartRateBpm", "AltitudeMeters", "DistanceMeters")
      mydf$Time <- sapply(mydf$Time, function(x) getDateTime(startdate[1], startdate[2], x, -4))
      
      mydf$Id <- NA
      mydf$LatitudeDegrees <- NA
      mydf$LongitudeDegrees <- NA
      mydf$Time <- as.POSIXct(mydf$Time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
      
      
    } else {
      return(NULL) # "Data error"
    }
  } else {
    return(NULL) # "Data error"
  }
  
  
  return(mydf[myHeader])
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

  if (myFile$type %in% c("text/csv", "application/vnd.ms-excel") & sub(".*\\.", "", myFile$name)=="csv") {
    
    # Typ ist CSV-Datei, teste auf Polar-Format
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
