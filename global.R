# ###################################################################################################################
#                                                                                                                   #
#    CoroVis - global.R                                                                                             #
#    Coding vom ....                                                                                                #
#    Copyright by Shiny-AG                                                                                          #
#                                                                                                                   #
# ###################################################################################################################


# ##############################################################################################################
#
#    Definiere globale Variablen
#
# ##############################################################################################################

# temporär zur Fehlerbehandlung --------------------------------------------------------------------------------

mytest <- NULL

# --------------------------------------------------------------------------------------------------------------

options(digits=10)
# Dataframe fuer alle eingelesenen Daten aus den Trainingseinheiten
viewportDF <- NULL

# Vokabelliste als Dataframe
dic <- read.csv2("languages.csv", header=TRUE, stringsAsFactors = FALSE, row.names=1, fileEncoding="UTF-8")
availableLang <- names(dic)
numDics <- length(availableLang)
iLang <- 1                                                      # TEMP: Abwaertskompatibilitaet zu alter Version

# Variablen für die Herzfrequenzeinstellungen festelegen
# hfMaxGeneral <- 220
# hfMinGeneral <- 60
# hfBereiche <- list("minimal" = 1,"leicht" = 2, "moderat" = 3, "schwer" = 4, "sehr schwer" = 5, "maximal" = 6)
# heartRateLimits <- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (hfMaxGeneral - 40) + 40

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

# Basisfarben für die Darstellung des Referenzbereiches
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
# berechnet default-Wert für Zeitzonenverschiebung aus Systemdatum
getTZshift <- function() {
  tz<- round(as.numeric(as.POSIXct(strftime(Sys.time(),
                                            format = "%Y-%m-%d %H:%M:%S"), tz = "UTC") - Sys.time()),0)
  if (is.na(tz)) {tz <- 2}
  return(tz)
}

# --------------------------------------------------------------------------------------------------------------
# berechnet den optimalen Herzfrequenzbereich nach der vorgegebenen Belastungsintensitaet
getRange <- function(minHR=60, maxHR=180, level="5", HRmax=FALSE) {
  intensity <- strtoi(level)
  myRange <- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0)
  if (HRmax) {
    lowerLimit <- myRange[intensity]*maxHR
    upperLimit <- myRange[intensity+1]*maxHR
  } else {
    lowerLimit <- myRange[intensity]*(maxHR - minHR) + minHR
    upperLimit <- myRange[intensity+1]*(maxHR - minHR) + minHR
  }
  
  return(c(lowerLimit, upperLimit))
}

# --------------------------------------------------------------------------------------------------------------

cleanData <- function(df) {
  df <- df[!is.na(df$Time),]
  df$Time <- sub("(\\.[0-9]{3})?Z|(\\.[0-9]{3}?\\+[0-9]{2}:[0-9]{2})", "", df$Time)
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  df$HeartRateBpm <- round(as.numeric(df$HeartRateBpm), 0)
  df$LatitudeDegrees <- round(as.numeric(df$LatitudeDegrees), 6)
  df$LongitudeDegrees <- round(as.numeric(df$LongitudeDegrees), 6)
  df$AltitudeMeters <- round(as.numeric(df$AltitudeMeters), 1)
  df$DistanceMeters <- round(as.numeric(df$DistanceMeters), 1)
  
  return(df)
}



# ##############################################################################
#' Berechnet BMI aus Groesse [cm] und Gewicht [kg]
#'
#' @param cbGroesse checkbox-Status Groesse
#' @param cbGewicht checkbox-Status Gewicht
#' @param groesse Koerpergroesse
#' @param gewicht Koerpergewicht
#' @param lang aktuelle Sprache
#'
#' @return BMI bzw. Keine Angaben
#'
calculateBMI <- function(cbGroesse, cbGewicht, groesse, gewicht, lang) {
  if (cbGroesse==TRUE && cbGewicht==TRUE) {
    return(round(gewicht/((groesse/100)^2), 1))
  } else {
    return(translate("nd", lang))
  }
}
# ##############################################################################



# ##############################################################################
#' Berechnet Alter aus date-of-birth
#'
#' @param cbDOB checkbox-Status Geburtsdatum
#' @param DOB Geburtsdatum
#' @param lang aktuelle Sprache
#'
#' @return Alter bzw. Keine Angaben
#'
calculateAge <- function(cbDOB, DOB, lang) {
  if (cbDOB==TRUE && !is.null(DOB)) {
    return(round(difftime(Sys.time(), DOB)/365 , 1))
  } else {
    return(translate("nd", lang))
  }
}
# ##############################################################################



# ##############################################################################
#' Liefert Anrede oder Geschlecht zurueck
#'
#' @param cbSex checkbox-Status Geschlecht
#' @param sex Geschlecht
#' @param lang aktuelle Sprache
#' @param short Mann/Frau oder maennlich/weiblich
#'
#' @return Anrede/Geschlecht bzw. Keine Angaben
#'
getSalutation <- function(cbSex, sex, lang, short=TRUE) {
  if (cbSex==TRUE && !is.null(sex)) {
    if (sex=="f") {
      if (short==TRUE) {
        return(translate("salutationF", lang))
      } else {
        return(translate("weiblich", lang))
      }
    } else {
      if (short==TRUE) {
        return(translate("salutationM", lang))
      } else {
        return(translate("maennlich", lang))
      }      
    }
  } else {
    return(translate("nd", lang))
  }
}
# ##############################################################################



# ##############################################################################
#' Liefert Eingabewert oder Keine Angaben zurueck
#'
#' @param cbValue checkbox-Status zu Eingabewert
#' @param value Eingabewert
#' @param lang aktuelle Sprache
#'
#' @return Groesse bzw. Keine Angaben
#'
getValue <- function(cbValue, value, lang) {
  if (class(value)=="Date" && cbValue==TRUE) {
    format <- translate("formatDate", lang)
    return(format(value, format=format))
  } else {
    if (cbValue==TRUE && value!="") {
      return(as.character(value))
    } else {
      return(translate("nd", lang))
    }   
  }
}
# ##############################################################################

# --------------------------------------------------------------------------------------------------------------
# stellt zu keyword und Sprache die passende Vokabel bereit; wird keyword nicht gefunden -> return keyword
translate <- function(keyword, languageID = 0) {
  i <- ifelse(languageID == 0, iLang, languageID)               # TEMP: Abwaertskompatibilitaet zu alter Version
  word <- dic[keyword, availableLang[i]]
  return(ifelse(is.na(word), keyword, word))
}

# --------------------------------------------------------------------------------------------------------------

getNewSelection <- function(oldSel, newLang, choices) {
  myVector <- NULL
  for (i in 1:numDics) {
    myVector <- c(myVector, translate(choices, i))
  }
  i <- match(oldSel, myVector) - 1
  if (is.na(i)) {
    newSel <- translate(choices[1], newLang)
  } else {
    newSel <- translate(choices[(i %% length(choices)) + 1], newLang)
  }
  return(newSel)
}

# --------------------------------------------------------------------------------------------------------------

showMessage <- function(msg, languageID) {
  part <- strsplit(msg, "@")[[1]]
  showModal(modalDialog(title = translate(part[2], languageID), translate(part[3], languageID),
                        easyClose = TRUE, footer = NULL))
}

# --------------------------------------------------------------------------------------------------------------
# Hilfsfuntkion: konvertiert HH:MM:SS zu Sekunden
getSeconds <- function(raw) {
  elem <- unlist(strsplit(raw, ":"))
  return(strtoi(elem[1], base = 10)*3600 + strtoi(elem[2], base = 10)*60 + strtoi(elem[3], base = 10))
}

# Hilfsfunktion: konvertiert relative Zeit absolute Date-Time
# @startdate: DD-MM-YYYY
# @starttime: HH:MM:SS
getDateTime <- function(startdate, starttime, reltime, difftime=0) {
  start <- as.POSIXct(paste(startdate, starttime, sep="T"), format = "%d-%m-%YT%H:%M:%S", tz="UTC")
  start <- start + getSeconds(reltime) + 1 - (difftime*3600)
  start <- format(start, format = "%Y-%m-%dT%H:%M:%S")
  return(start)
}

# --------------------------------------------------------------------------------------------------------------

mergeDF <- function(old, new, mergeBy) {
  # assertion: ncol(old)==ncol(new); colnames(old)==colnames(new)
  i <- which(colnames(old)==mergeBy)
  cols <- ncol(old)
  
  # merge dataframes
  merged <- merge(old, new, by = mergeBy, all = TRUE)
  for (k in 2:cols) {
    merged[,k] <- ifelse(is.na(merged[,k]), merged[,k+cols-1], merged[,k])
  }
  
  # keep only first ncol(old) columns
  merged <- merged[,1:cols]
  
  # rename colnames: mergeby, colnames without mergeby
  colnames(merged) <- c(mergeBy, colnames(old)[-i])
  
  return(merged[colnames(old)])
}
# ##############################################################################



# ##############################################################################
#' Extrahiert aus CSV-Datei einen Dataframe
#'
#' @param myFile datapath zu CSV-Datei
#' @param timezoneshift Stunden vor/nach UTC zur Zeitkorrektur
#'
#' @return Dataframe mit standardisierter Datenstruktur
#'
importDataCSV <- function(myFile, timezoneshift) {
  # to be safe: read data as raw textfile
  conn <- file(myFile, open="r")
  raw <- readLines(conn)
  close(conn)
  n <- length(raw)
  
  # evaluate raw
  if (n == 0) {
    return(NULL) # "File is empty"
  } else if (n >= 3) {
    if (raw[1] == polarheader1 & raw[3] == polarheader2) {
      # Polar CSV detected
      mydf <- read.csv(text=paste(raw[1], raw[2], sep = "\n"),
                       stringsAsFactors = FALSE)
      startdate <- mydf[1, c(3:4)]
      mydf <- read.csv(myFile, skip = 2, header = TRUE,
                       stringsAsFactors = FALSE)
      mydf <- mydf[,c(2:3,7,9)]
      colnames(mydf) <- c("Time", "HeartRateBpm", "AltitudeMeters",
                          "DistanceMeters")
      mydf$Time <- sapply(mydf$Time, function(x) getDateTime(startdate[1],
                                                             startdate[2], x,
                                                             timezoneshift))
      mydf$Id <- NA
      mydf$LatitudeDegrees <- NA
      mydf$LongitudeDegrees <- NA
      
    } else {
      return(NULL) # "Data error"
    }
  } else {
    return(NULL) # "Data error"
  }
  
  return(mydf[myHeader])
}
# ##############################################################################



# ##############################################################################
#' Extrahiert dataframe aus gpxfile; bei fehlerhaften Daten wird insgesamt NULL
#' zurückgeliefert
#'
#' @param gpxfile datapath zu GPX-Datei
#'
#' @return Dataframe mit standardisierter Datenstruktur
#'
importDataGPX <- function(gpxfile) {
  newData <- NULL
  doc <- xmlParse(gpxfile)
  nodes <- getNodeSet(doc, "//ns:trkpt", "ns")
  totalsize <- xmlSize(nodes)
  if (totalsize>0) {
    for (i in 1:totalsize) {
      node <- nodes[[i]]
      x <- c(Time = xmlValue(node[["time"]]),
             LatitudeDegrees = xmlAttrs(node)[["lat"]],
             LongitudeDegrees = xmlAttrs(node)[["lon"]],
             AltitudeMeters = xmlValue(node[["ele"]]))
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
# ##############################################################################



# ##############################################################################
#' Testet Dateityp und liefert Typ oder Fehlermeldung als String
#'
#' @param myFile Zeile aus inputFile-Dataframe
#'
#' @return String mit Dateityp oder Fehlermeldung
#'
checkFileformat <- function(myFile) {
  MIMEtype <- c("text/csv", "application/vnd.ms-excel")
  if (myFile$type %in% MIMEtype & sub(".*\\.", "", myFile$name)=="csv") {
    
    # Typ ist CSV-Datei, teste auf Polar-Format
    conn <- file(myFile$datapath, open="r")
    raw <- readLines(conn)
    close(conn)
    n <- length(raw)
    
    if (n == 0) {
      msg <- "@File is empty;"
    } else if (n >= 3 && raw[1] == polarheader1 && raw[3] == polarheader2) {
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
# ##############################################################################



# ##############################################################################
#' Simuliert den nächsten Herzfrequenzwert basierend auf dem Eingabewert
#'
#' @param value preceeding heartrate value
#'
#' @return new heartrate value between 60 .. 150 bpm
#'
simu <- function(value) {
  newvalue <- as.integer(runif(1, min = value-3, max = value+4))
  newvalue <- ifelse(newvalue<60, 60, ifelse(newvalue>150, 150, newvalue))
  return(newvalue)
}
# ##############################################################################



# ##############################################################################
#' Erstellt einen Testdatensatz (simuliert prinzipiell coroRawData)
#'
#' @param onlyOne return only third group?
#' @param g1 datapoints per group 1
#' @param g2 datapoints per group 2
#' @param g3 datapoints per group 3
#'
#' @return data.frame of testdata
#'
createTestdata <- function(onlyOne=FALSE, g1=400, g2=500, g3=300) {
  dateint <- c(1500010001:(1500010001+g1-1),
               1500100001:(1500100001+g2-1),
               1500190001:(1500190001+g3-1))
  DTG <- as.POSIXct(dateint, origin = "1970-01-01")
  start1 <- c(120)
  for (i in 1:(g1-1)) {
    start1 <- c(start1, simu(start1[length(start1)]))
  }
  start2 <- c(80)
  for (i in 1:(g2-1)) {
    start2 <- c(start2, simu(start2[length(start2)]))
  }
  start3 <- c(140)
  for (i in 1:(g3-1)) {
    start3 <- c(start3, simu(start3[length(start3)]))
  }
  HR <- c(start1, start2, start3)
  td <- data.frame(dateint, DTG, HR)
  td$deltaTime <- c(0, td$dateint[2:nrow(td)]-td$dateint[1:nrow(td)-1])
  
  # more than 1200 sec = 20 min ist a new session
  td$deltaTime <- ifelse(td$deltaTime>1200, 0, td$deltaTime)
  td$Date <- as.Date(td$DTG)
  
  if (onlyOne==TRUE) {
    td <- td[td$dateint>1500190000,]
  }
  
  return(td)
}
# ##############################################################################



# ##############################################################################
#' creates summary as PDF report
#'
#' @param patData structured patient data
#' @param param structured parameters (ranges, colors)
#' @param hfData data to analyze
#' @param file fileobject from downloadHandler
#' @param lang aktuelle Sprache
#'
#' @return void
#'
createPDF <- function(patData, param, hfData, file, lang=1) {
  
  # --- calculate some additional data -----------------------------------------
  unitHF <- translate("bpm", lang)
  hfData$cs <- cumsum(hfData$deltaTime)
  hfData$dup <- duplicated(hfData$cs)
  hfData$label <- rawToChar(as.raw(65))
  dekl <- grep(TRUE, hfData$dup)
  for (i in dekl) {
    hfData[i:nrow(hfData), "label"] <-  rawToChar(as.raw(65 + grep(i, dekl)))
  }
  hfData$diff <- hfData$deltaTime
  if (max(hfData$cs)>600) {
    hfData$cs <- hfData$cs/60
    hfData$diff <- hfData$diff/60
    unitT <- translate("minutes", lang)
    if (max(hfData$cs)>180) {
      hfData$cs <- hfData$cs/60
      hfData$diff <- hfData$diff/60
      unitT <- translate("hours", lang)
    }
  } else {
    unitT <- translate("seconds", lang)
  }
  tab <- aggregate(diff ~ label, hfData, sum)
  tab$diff <- round(tab$diff, 1)
  hfData$sessionTime <- hfData$label
  for (i in 1:nrow(tab)) {
    hfData[hfData$sessionTime==tab$label[i], "sessionTime"] <- tab$diff[i]
  }
  hfData$group <- ifelse(hfData$HR<as.integer(param[1]), "lOpt",
                         ifelse(hfData$HR>as.integer(param[2]), "uOpt",
                                "iOpt"))
  hfData$groupCol <- ifelse(hfData$HR<as.integer(param[1]), param[3],
                            ifelse(hfData$HR>as.integer(param[2]), param[5],
                                   param[4]))
  myGroups <- aggregate(deltaTime ~ group, hfData, NROW)
  rownames(myGroups) <- myGroups$group
  lOpt <- myGroups["lOpt", "deltaTime"]
  iOpt <- myGroups["iOpt", "deltaTime"]
  uOpt <- myGroups["uOpt", "deltaTime"]
  aOpt <- lOpt + iOpt + uOpt
  
  
  hfData$label <- paste0(hfData$label, " (", hfData$Date, ")\n",
                         hfData$sessionTime, " ", unitT)
  
  # --- prepare PDF document ---------------------------------------------------
  pdf(file, width = 210/25.4, height = 297/25.4)
  layout(matrix(c(1, 1, 2, 3, 4, 4, 5, 6), 4, 2, byrow = TRUE),
         widths=c(1, 1), heights=c(1, lcm(5), 5, 5))
  
  # --- title area -------------------------------------------------------------
  par(mai = c(0, 25/25.4, 15/25.4, 25/25.4))
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), xaxs = "i", yaxs = "i",
       axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")
  rect(0, 0, 20, 10, col = "skyblue", border = FALSE)
  title(main = paste(translate("reportHeader", lang),
                     strftime(Sys.Date(), format = "%d.%m.%Y")),
        line = -1.5, cex = 2)
  
  # --- patient area -----------------------------------------------------------
  par(mai = c(0.2, 25/25.4, 0.2, 0.1))
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), xaxs = "i", yaxs = "i",
       axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")
  txtX <- rep(1, 9)
  txtY <- seq(9, 1, -1)
  txtT <- c(
    translate("patientData", lang),
    paste0(translate("salutation", lang), ":"),
    paste0(translate("Name", lang), ":"),
    paste0(translate("Vorname", lang), ":"),
    paste0(translate("Geburtsdatum", lang), ":"),
    paste0(translate("Alter", lang), ":"),
    paste0(translate("Groesse", lang), ":"),
    paste0(translate("Gewicht", lang), ":"),
    paste0(translate("bmi", lang), ":")
  )
  text(txtX, txtY, txtT, adj = c(0, 0.5))
  txtX <- rep(9, 8)
  txtT <- c("", patData)
  text(txtX, txtY, txtT, adj = c(0, 0.5))
  abline(h = c(9.5, 8.5, 0.5), col = "skyblue")
  
  # --- summary area -----------------------------------------------------------
  par(mai = c(0.2, 0.1, 0.2, 25/25.4))
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), xaxs = "i", yaxs = "i",
       axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")
  txtX <- rep(1, 9)
  txtY <- seq(9, 1, -1)
  txtT <- c(
    translate("parameterHeader", lang),
    paste0(translate("Maximale Herzfrequenz", lang), ":"),
    paste0(translate("Belastungsintensitaet", lang), ":"),
    paste0(translate("Frequenzbereich", lang), ":"),
    paste0(translate("trainingSessions", lang), ":"),
    paste0(translate("trainingTime", lang), ":"),
    paste0(translate("belowOpt", lang), ":"),
    paste0(translate("inOpt", lang), ":"),
    paste0(translate("aboveOpt", lang), ":")
  )
  text(txtX, txtY, txtT, adj = c(0, 0.5))
  txtX <- rep(12, 8)
  txtT <- c(
    "",
    paste(param[6], "...", param[7]),
    param[8],
    paste(param[1], "...", param[2]),
    length(unique(hfData$label)),
    paste(round(max(hfData$cs), 1), unitT),
    paste(lOpt, "/", aOpt, paste0("(", round(100*lOpt/(aOpt), 1), "%)")),
    paste(iOpt, "/", aOpt, paste0("(", round(100*iOpt/(aOpt), 1), "%)")),
    paste(uOpt, "/", aOpt, paste0("(", round(100*uOpt/(aOpt), 1), "%)"))
  )
  text(txtX, txtY, txtT, adj = c(0, 0.5))
  abline(h = c(9.5, 8.5, 0.5), col = "skyblue")
  
  # --- summary plot -----------------------------------------------------------
  par(mai = c(0.8, 37.5/25.4, 0.2, 25/25.4))
  xlab <- paste0(translate("trainingTime", lang), " [", unitT, "]")
  ylab <- translate("HFq", lang)
  main <- translate("Zusammenfassung", lang)
  plot(hfData$cs, hfData$HR, xaxs = "i", yaxs = "i", axes = TRUE,
       frame.plot = TRUE, xlab = xlab, ylab = ylab, pch = 16,
       col = hfData$groupCol, type = "n", main = main)
  rect(min(hfData$cs), as.numeric(param[1])-0.5, max(hfData$cs),
       as.numeric(param[2])+0.5, col = "aliceblue", border = NA)
  for(i in 1:(length(hfData$cs)-1)){
    if (hfData$dup[i+1]==FALSE) {
      segments(hfData$cs[i], hfData$HR[i], hfData$cs[i+1], hfData$HR[i+1],
               col=hfData$groupCol[i])
    }
  }
  abline(v = hfData$cs[grep(TRUE, hfData$dup)])
  box()
  
  # --- summary histogram ------------------------------------------------------
  par(mai = c(25/25.4, 37.5/25.4, 0.2, 0.2))
  myBreaks <- seq(floor(min(hfData$HR)/10)*10,
                  ceiling(max(hfData$HR)/10)*10, 10)
  h <- hist(hfData$HR, breaks = myBreaks, plot=FALSE)
  cuts <- cut(h$breaks, c(-Inf, as.numeric(param[1])-0.5,
                          as.numeric(param[2])-0.5, Inf))
  levels(cuts)[1] <- param[3]
  levels(cuts)[2] <- param[4]
  levels(cuts)[3] <- param[5]
  plot(h, col=as.character(cuts), xlab = translate("HFq", lang),
       ylab = translate("yaxis", lang),
       main = translate("heartRateClasses", lang))
  box()
  
  # --- summary stacked barplot ------------------------------------------------
  par(mai = c(25/25.4, 1, 0.2, 25/25.4))
  myData <- t(table(hfData$label, hfData$groupCol))
  for (i in 1:ncol(myData)) {
    s <- sum(myData[, i])
    myData[, i] <- 100*myData[, i]/s
  }
  n <- length(unique(hfData$label))
  if (n==1) {
    myBarData <- myData
  } else {
    myBarData <- myData[,n:1]
  }
  myOrder <- c()
  for (i in c(param[3], param[4], param[5])) {
    myOrder <- c(myOrder, grep(i, rownames(myBarData)))
  }
  myBarData <- myBarData[myOrder, , drop=FALSE]
  barplot(myBarData, horiz = TRUE, las = 1, col = rownames(myBarData),
          xlab = translate("percentage", lang),
          main = translate("trainingSessions", lang))
  box()
  
  # --- finish PDF document ----------------------------------------------------
  dev.off()
}
# ##############################################################################

# *** ENDE ***
