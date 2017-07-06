hfMaxGeneral <- 200

# ########################################################################################################################
#
#    Definiere globale Variablen
#
# ########################################################################################################################

# ------------------------------------------------------------------------------------------------------------------------

viewportDF <- NULL      # Dataframe fuer alle eingelesenen Daten aus den Trainingseinheiten

# ------------------------------------------------------------------------------------------------------------------------



# ########################################################################################################################
#
#    Definiere globale Funktionen (ohne Shiny-Interaktion)
#
# ########################################################################################################################

# ------------------------------------------------------------------------------------------------------------------------

importDataCSV <- function(myFile) {
  # TODO
  return(NULL)
}

# ------------------------------------------------------------------------------------------------------------------------

importDataGPX <- function(myFile) {
  # TODO
  return(NULL)
}

# ------------------------------------------------------------------------------------------------------------------------

checkFileformat <- function(myFile) {

  if (myFile$type == "text/csv" & sub(".*\\.", "", myFile$name)=="csv") {
    
    # vermutlich CSV-Datei, teste auf Polar-Format
    polarheader1 <- paste0("Name,Sport,Date,Start time,Duration,Total distance (km),Average heart rate (bpm),Average sp",
                           "eed (km/h),Max speed (km/h),Average pace (min/km),Max pace (min/km),Calories,Fat percentage",
                           " of calories(%),Average cadence (rpm),Average stride length (cm),Running index,Training loa",
                           "d,Ascent (m),Descent (m),Notes,Height (cm),Weight (kg),HR max,HR sit,VO2max,")
    polarheader2 <- paste0("Sample rate,Time,HR (bpm),Speed (km/h),Pace (min/km),Cadence,Altitude (m),Stride length (m)",
                           ",Distances (m),Temperatures (C),Power (W),")
    
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
    
    # vermutlich TCX-Datei
    msg <- "TCX"
    
  } else if (myFile$type == "" & sub(".*\\.", "", myFile$name)=="gpx") {
    
    # vermutlich GPX-Datei
    msg <- "GPX"
    
  } else {
    
    # vermutlich ... keine Ahnung
    msg <- "@Import error@Wrong file format. coroVis supports TCX, GPX, CSV only."
  }
  
  return(msg)
}

# ------------------------------------------------------------------------------------------------------------------------
