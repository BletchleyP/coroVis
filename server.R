library(shiny)    # Shiny selbst...
library(shinyjs)  # ShinyJS zur Unterstuetzung ...
library(chron)    # Handling von Zeit und Datum...
library(XML)      # Einlesen von XML-Dateien ...
library(leaflet)  # Darstellung der Karten ...
library(colourpicker)
#library(ggplot2)  # Darstellung der Plots ...

# -------------------------------------------------------------------
# Allgemeine Translate-Funktionen fuer die unterschiedlichen Sprachen
# -------------------------------------------------------------------

# die Funktion sucht fuer die eingegebene Übersetzung das Schluesselwort ...
rev_translate <- function(word) {
  return(dic[which(dic[availableLang[iLang]]==word), 1])
}

# ----------------------------------
# Shiny-Server-Funktion beginnt ....
# ----------------------------------

shinyServer(function(input, output, session) {
  
  # Max upload Filegroesse bis 15 MB
  options(shiny.maxRequestSize=15*1024^2)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # --------------------------------------------------------------------------------------
  # Fuer das Merken von Einstellungen der UI (Geburtsdatum, Geschlecht, Status InputPanel) 
  # --------------------------------------------------------------------------------------
  currentBirthDate <- NULL
  currentSex <- NULL
  iPanel <- TRUE
  
  # Fuer die Herzfrequenzeingaben, hier auf Session-Ebene
  rVal <- reactiveValues(hfBo=0, hfBu=0)
  hfMaxOut <- 0
  maxFr <- 0
  

  
  # ----------------------------------------------------------------
  # Einmaliges Festlegen der Reaktion auf Klick auf die Flagge, usw.
  # ----------------------------------------------------------------
  shinyjs::onclick("decrease", iPanelOn)
  shinyjs::onclick("increase", iPanelOn)

  # ----------------------------------------------------------------
  # Allgemeine Variablen - fuer die extrahierten Mess- und GPS-Daten
  # ----------------------------------------------------------------
  exerciseData <- NULL    # Liste für die Daten der Trainingseinheiten
  unitList <- NULL        # Liste der Namen für die Trainingseinheiten
  
  # ------------------------------------------
  # Vorbereitungsfunktionen zum Rendern des UI
  # ------------------------------------------
  
  # Fuer das Geburtsdatum
  renderGebDat <- function(){
    output$gebdat <- renderUI({
      tagList(
        dateInput("inpAlter", value = NULL, label = translate("Geburtsdatum"), format = translate("dd.mm.yyyy"), language = translate("de"))
      )
    })
  }
  
  # Fuer das Geschlecht
  renderGesch <- function(){
    output$gesch <- renderUI({
      mask <- as.character(translate("maennlich"))
      fem <- as.character(translate("weiblich"))
      tagList(
        radioButtons("inpGesch", label = translate("Geschlecht"), choiceNames = list(mask, fem), choiceValues = list('m', 'f'), inline = TRUE, width = '100%')
      )
    })
  }
  

  
  # Fuer die Risikoklasse
  renderRiskClass <- function() {
    output$riskclass <- renderUI({
      tagList(
        radioButtons("inpRiskClass", label = a(href = 'LL_KActiv.pdf', translate("Risikoklasse"), target = "_blank"), choiceNames = list('A', 'B', 'C', 'D'), choiceValues = list(hfMaxGeneral, hfMaxGeneral*0.8, hfMaxGeneral*0.6, hfMaxGeneral*0.5), inline = TRUE)
      )
    })
  }
  
  # Fuer die Belastungsintensitaet
  renderStrIntensity <- function() {
    output$intensity <- renderUI({
      tagList(
        selectInput("risk", label = textOutput("risiko_t"), choices = list(translate("minimal"), 
                                                                           translate("leicht"),
                                                                           translate("moderat"),
                                                                           translate("schwer"),
                                                                           translate("sehr schwer"),
                                                                           translate("maximal")), selected = 1)
      )      
    })
  }

  # -------------------
  # Hilfsfunktionen ...
  # -------------------
  
  # Sprachwechsel bei Click auf die Fahne
  change_language <- function() {
    # Die entsprechende Sprache einstellen...
    iLang <<- ifelse(iLang > numDics-1, 1, iLang+1)
    setFlag()
  }
  

  
  # Funktion zum An- und Ausschalten des Inputpanels
  iPanelOn <- function() {
    iPanel <- input$iPanelOn
    updateCheckboxInput(session, "iPanelOn", value = !iPanel)
  }

  # -------------------------------------------------------
  # Sprache auf dem UI einstellen bzw. auf Klick hin setzen
  # -------------------------------------------------------
  
  setFlag <- function() {
    # Input-Panel
    renderGebDat()
    renderGesch()
        
    # Sidebar
    renderRiskClass()
    renderStrIntensity()
    output$risiko_t <- renderText({ paste(translate("Belastungsintensitaet")) })
    
    # mainPanel
    output$selAxisX <- renderText({ paste(translate("XAchse"))})
    output$selAxis <- renderText({ paste(translate("YAchse")) })

    # Wieder Einstellen von bestimmten ausgewaehlten Parametern nach dem Sprachwechsel...
    updateDateInput(session, "inpAlter", value = NULL)        # Alter wieder einstellen
    updateRadioButtons(session, "inpGesch", selected = currentSex)        # Geschlecht wieder einstellen
  }

  # ------------------------------------------------------
  # Funktionen fuer die Aufbereitung der Daten zur Ausgabe
  # ------------------------------------------------------
  
  # Dateien für die Trainingseinheit einer Polar M200 - Watch zusammenstellen (d.h. ...)
  lookForExerciseUnitPolarM200 <- function(datInfo) {
    
    unitList <<- NULL
    exerciseData <<- NULL
    viewportDF <<- NULL
    
    dN <- list()
    necData <- list()
    endings <- c("gpx", "csv")
    vp_df <- NULL
    readFiles <- 0
    givenFiles <- nrow(datInfo)
    
    withProgress(message = translate("Bereite Daten auf"), {
    
    for(k in endings) {
      dI <- which(grepl(paste0("[:print:]*.", k), datInfo$name))
      prog <- length(endings) * length(dI)
      
      for(i in dI){
        liIt <- gsub(k, "", datInfo$name[i])
        switch(k,
               
               csv = {
                 
                 tryCatch({
                   if (datInfo$type[i] == "text/csv") {
                     
                     # Allgemeine Patientendaten einlesen
                     pData <- read.csv(datInfo$datapath[i], header = TRUE, nrows = 1)
                     # Trainingsdaten einlesen
                     dN[[liIt]] <- read.csv(datInfo$datapath[i], header = TRUE, skip = 2)
                     
                     # Update der Felder mit den Patientendaten
                     splitname <- strsplit(as.character(pData$Name), " ")[[1]]
                     updateTextInput(session, "nachname", value = splitname[[2]][1])
                     updateTextInput(session, "vorname", value =  splitname[[1]][1])
                     updateSliderInput(session, "groesse", value=pData$Height..cm.)
                     updateSliderInput(session, "gewicht", value=pData$Weight..kg.)
                  
                     # Dataframe herrichten
                     tData <- data.frame(dN[[liIt]]$Time, 
                                         dN[[liIt]]$HR..bpm., 
                                         dN[[liIt]]$Altitude..m.,
                                         dN[[liIt]]$Distances..m.)

                     filename <- rep(liIt, len = nrow(tData))
                     id <- rep(pData$Start.time, len = nrow(tData))
                     tData <- cbind(id, tData)
                     tData <- cbind(filename, tData)
                     colnames(tData) <- c("Filename", "Id", "Time", "HeartRateBpm", "AltitudeMeters", "DistanceMeters")
                     
                     if (!is.null(necData[[liIt]]$Time[1])) {
                       startTime <- necData[[liIt]]$Time[1]
                       tData$Time <- times(tData$Time) + times(startTime)
                       necData[[liIt]] <- merge(necData[[liIt]], tData, all = FALSE) # Einen Join der gleichen Elemente erzeugen
                     } else {
                       necData[[liIt]] <- tData # Einen Eintrag der CSV-Daten erzeugen
                     }
                     
                   }
                   incProgress(1/prog, detail = paste(translate("aus"), liIt, k))
                   readFiles <- readFiles + 1
                 },
                 
                 error = function(e) { 
                   m = modalDialog(
                     h2(translate("Fehlerhafte Daten")),
                     title = translate("Bitte korrigieren"),
                     footer = modalButton(translate("Weiter"))
                   )
                   showModal(m)
                 })
                 
               },
               
               gpx = {
                 
                 tab <- xmlParse(datInfo$datapath[i])
                 nodes <- getNodeSet(tab, "//ns:trkpt", "ns")
                 asTab <- plyr::ldply(nodes, as.data.frame(xmlToList))
                 
                 choice <- duplicated(asTab$value.time)                        # Gedoppelte Zeiten finden
                 gpx_time <- times(substr(asTab$value.time[choice], 12, 19))   # Zeiten aus dem String ausschneiden und als times-Objekt speichern
                 #gpx_time <- times(asTab$value.time[choice])
                 gpx_y <- asTab$value..attrs[choice]                           # Laenge...
                 gpx_x <- asTab$value..attrs[which(choice)-1]                  # ... und Breite merken
                 
                 tData <- data.frame(gpx_time, gpx_x, gpx_y)                   # Hilfsdataframe entwickeln
                 colnames(tData) <- c("Time", "LatitudeDegrees", "LongitudeDegrees")  # Spalten bezeichnen und zwar so, das 'Time' in beiden Dataframes Gleiches bezeichnet
                 
                 if (!is.null(necData[[liIt]]$Time[1])) {
                   necData[[liIt]] <- merge(necData[[liIt]], tData, all = FALSE) # Einen Join der gleichen Elemente erzeugen
                 } else {
                   necData[[liIt]] <- tData # Einen Eintrag der CSV-Daten erzeugen
                 }                 
                 incProgress(1/prog, detail = paste(translate("aus"), liIt, k))
                 readFiles <- readFiles + 1
               })
      }      
    }
    })
    
    if (givenFiles > readFiles) {
      
      m = modalDialog(
        h2(paste(givenFiles - readFiles), translate("Dateien nicht eingelesen")),
        title = translate("Falsches Format"),
        footer = modalButton(translate("Weiter"))
      )
      showModal(m)
      
    }
    
    if (readFiles > 0) {
      
      for(i in 1:length(necData)) {
        vp_df <- rbind(vp_df, necData[[i]])
      }
    
      gt <- paste(names(necData), "#", 'Alle Daten')
      unitList <<- gt                 # Damit werden die Namen der Trainingseinheiten angezeigt, die geladen sind.
                                      # Bereits geladene werden nur ueberschrieben, neue hinzugefuegt, egal
                                      # wieviele bereits ausgewaehlt oder geladen wurden
      exerciseData <<- necData
      viewportDF <<- vp_df
    }
  }
  
  # Dateien für die Trainingseinheit einer Garmin-Watch (im TCX-Format) zusammenstellen
  lookForExerciseUnitGarmin <- function(datInfo) {
    
    unitList <<- NULL
    exerciseData <<- NULL
    viewportDF <<- NULL
    
    necData <- list()
    endings <- c("tcx")
    vp_df <- NULL
    readFiles <- 0
    givenFiles <- nrow(datInfo)
    selList <- NULL
    
    withProgress(message = translate("Bereite Daten auf"), {
      
      for(k in endings) {
        dI <- which(grepl(paste0("[:print:]*.", k), datInfo$name))
        # Hier Test auf Vorhandensein einer TCX-Datei
        prog <- length(endings) * length(dI)
        for(i in dI){
          
          liIt <- gsub(k, "", datInfo$name[i])
          switch(k,
                 tcx = {
                   ret_list <- importDataTCX(datInfo[i,])
                   necData[[datInfo$name[i]]] <- ret_list[[2]]
                   vp_df <- rbind(vp_df, necData[[datInfo$name[i]]])
                   incProgress(1/prog, detail = paste(translate("aus"), liIt, k))
                   readFiles <- readFiles + 1
                   
                   ft <- rep(names(necData), length(ret_list[[1]]))
                   ft <- paste(ft, "#", ret_list[[1]])
                   gt <- paste(names(necData), "#", 'Alle Daten')
                   ft <- c(gt, ft)

                   selList <- c(selList, ft)
                 })
        }
        
      }

    })
    
    if (givenFiles > readFiles) {
      m = modalDialog(
        h2(paste(givenFiles - readFiles), translate("Dateien nicht eingelesen")),
        title = translate("Falsches Format"),
        footer = modalButton(translate("Weiter"))
      )
      showModal(m)
    }
    
    if (readFiles > 0) {
      unitList <<- selList
      exerciseData <<- necData
      viewportDF <<- vp_df    
    }
  }

  # -----------------------
  # UI - Beschriftungen ...
  # -----------------------
  
  setFlag()

  # ----------------------------------------------------
  # Bestimmte Variablen und Konstellation beobachten ...
  # ----------------------------------------------------
  
  # Reaktive Variablen festelegen
  rVal <- reactiveValues(hfBo=0, hfBu=0, hfMaxOut = 0, maxFr = 0)
    
  observe({
    
    # Immer wieder ...
    currentBirthDate <<- input$inpAlter  # Geburtsdatum merken ...
    currentSex <<- input$inpGesch  # Geschlecht merken, um beim Sprachwechsel das wieder aktualisieren zu koennen (ansonsten waere in der
                            # Konstellation mit dem JS-Script ein Update der Infos nicht so leicht moeglich)

    # Die ausgewaehlten Panels (hinterlegt in einer CheckboxGroup) werden mit folgender Anweisung dargestellt
    # Die Tabs muessen als integer-Vektor uebergeben werden. Innerhalb der Observe-Funktion werden Aenderungen
    # der CheckboxGroup wahrgenommen und das TabsetPanel aktualisiert.
    showTabsInTabset("tP", as.integer(input$tabsToShow))
    
  })
  
  # ----------------------
  # Die Events beachten...
  # ----------------------

  # Wenn die Risikoklasse, die Belastungsintensität oder die Maximalherzfrequenz eingestellt wird

  observeEvent(input$inpRiskClass, {
    
    heartRateLimits <<- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (as.numeric(input$inpRiskClass) - hfMinGeneral) + hfMinGeneral
    maxFr <<- as.numeric(input$inpRiskClass)

    updateSliderInput(session, "overrideMaxHF", value = maxFr)
    updateSliderInput(session, "hfMax", max = maxFr, value = maxFr)
    
  })
  
  observeEvent(input$hfMax, {
    
    heartRateLimits <<- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (input$hfMax - hfMinGeneral) + hfMinGeneral
    limit <- hfBereiche[[ifelse(is.null(input$risk), 1, input$risk)]]
    rVal$hfBu <- heartRateLimits[limit]
    rVal$hfBo <- heartRateLimits[limit+1]
    
    updateSliderInput(session, "hfBer", max = heartRateLimits[7], value = c(rVal$hfBu, rVal$hfBo))
    
  })
  
  observeEvent(input$risk, {
    
    limit <- hfBereiche[[ifelse(is.null(input$risk), 1, input$risk)]]
    rVal$hfBu <- heartRateLimits[limit]
    rVal$hfBo <- heartRateLimits[limit+1]
 
    updateSliderInput(session, "hfBer", max = heartRateLimits[7], value = c(rVal$hfBu, rVal$hfBo))

  })
  
  observeEvent(input$hfBer, {
    rVal$hfBu <- input$hfBer[1]
    rVal$hfBo <- input$hfBer[2]
  })
  


  
  observeEvent(input$overrideMaxHF, {
    updateSliderInput(session, "hfMax", value = input$hfMax, max = input$overrideMaxHF)
  })
  
# ########################################################################################################################
#
#    Definiere Funktionen mit Shiny-Interaktionen
# 
# ########################################################################################################################

# ------------------------------------------------------------------------------------------------------------------------
  
  # Die Funktion importiert Trainingsdaten aus einer TCX-Datei und liefert Dataframe
  importDataTCX <- function(tcxFile) {

    doc <- xmlParse(tcxFile)
    
    # ermittle Gesamtanzahl <Trackpoint>-tags
    totalSize <- xmlSize(getNodeSet(doc, "//ns:Trackpoint", "ns"))
    if (totalSize>0) {
      if (file.exists("temp.csv")) {file.remove("temp.csv")}
      catTemp <- file("temp.csv", open = "a")
      cat(paste0(paste(myHeader, collapse = ","), "\n"), file=catTemp)
      
      # ermittle Anzahl <Activity>-tags
      nodes <- getNodeSet(doc, "//ns:Activities", "ns")
      size <- xmlSize(nodes[[1]])
      counter <- 0
      
      withProgress(message = sprintf(translate("importDataMsg", isolate(input$language)), totalSize), value = 0, {
        
        # foreach <Activity>-tag
        for (i in 1:size) {
          startNode <- nodes[[1]][[i]]
          label <- c(Id = xmlValue(startNode[["Id"]]))
          subNodes <- getNodeSet(doc, paste0("(//ns:Activity)[", i, "]//ns:Trackpoint"), "ns")
          subSize <- xmlSize(subNodes)
          counter <- counter + subSize
          
          # foreach <Trackpoint>-tag
          if (subSize>0) {
            for (j in 1:subSize) {
              v = getChildrenStrings(subNodes[[j]])
              if (length(subNodes[[j]]["Position"])==1) {
                v <- c(v, getChildrenStrings(subNodes[[j]]["Position"][[1]]))
              }
              v <- c(label, v)
              cat(paste0(paste(v[myHeader], collapse = ","), "\n"), file=catTemp)
            }
          }
          
          # aktualisiere Fortschrittsanzeige
          incProgress(counter/totalSize)
        }
      })
      
      # beende den Datenstrom und importiere die Daten aus catTemp
      close(catTemp)
      df_trackpoints <- read.csv("temp.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
      
    } else {
      df_trackpoints <- NULL
    }
    
    if (file.exists("temp.csv")) {file.remove("temp.csv")}

    return(df_trackpoints)
  }
  
# --------------------------------------------------------------------------------------------------------------
  
  # Funktion kontrolliert den Datenimport nach Datei-Upload
  importFiles <- function(fileDF) {
    if (is.null(fileDF)) {return(NULL)}
    
    newDataAll <- NULL
    errormsg <- NULL
    n <- nrow(fileDF)
    withProgress(message = sprintf(translate("importFileMsg", isolate(input$language)), n), value = 0, {
      # durchlaufe alle zu importierenden Dateien
      for (i in 1:n) {
        currentFile <- fileDF[i,]
        result <- checkFileformat(currentFile)
        if (substr(result, 1, 1) == "@") {
          # speichere Fehlermeldung und weiter zur nächsten Datei
          errormsg <- paste(errormsg, substring(result, 2))
  
        } else {
          # starte formatspezifische Konversion in Dataframe
          if (result == "TCX") {
            newData <- importDataTCX(currentFile$datapath)
          } else if (result == "CSV") {
            newData <- importDataCSV(currentFile$datapath)
          } else {
            newData <- importDataGPX(currentFile$datapath)
          }
  
          if (is.null(newData)) {
            # speichere Fehlermeldung und weiter
            errormsg <- paste(errormsg, paste0("No data in file: ", currentFile$name, ";"))
  
          } else {
            # merge neue Daten mit newDataAll
            if (is.null(newDataAll)) {
              newDataAll <- newData
            } else {
              newDataAll <- mergeDF(newDataAll, newData, "Time")
            }
          }
        }
        
        # aktualisiere Fortschrittsanzeige
        incProgress(i/n)
      }
    })
    
    # if (result != "CSV") {
    #   # entferne Millisekunden und ZULU-timezone tag
    #   timeZ <-   grepl("(\\.[0-9]{3})?Z|(\\.[0-9]{3}?\\+[0-9]{2}:[0-9]{2})", newDataAll$Time)
    #   # timeZ <- grepl("(\\.[0-9]{3})?Z", newDataAll$Time)
    #   if (sum(timeZ, na.rm=TRUE) == nrow(newDataAll)) {
    #     newDataAll$Time <- sub("(\\.[0-9]{3})?Z|(\\.[0-9]{3}?\\+[0-9]{2}:[0-9]{2})", "", newDataAll$Time)
    #     newDataAll$Time <- as.POSIXct(newDataAll$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    #   } else {
    #     newDataAll <- NULL
    #     
    #     errormsg <- paste(errormsg, "Fehler bei Datum-Zeit-Konversion")
    #   }
    # }
    
    
    # konvertiere chr zu numeric mit 6 (Lat/Lon), 0 (HeartRate) bzw. 1 (Altitude, Distance) Nachkommastelle
    options(digits=10)
    newDataAll$LatitudeDegrees <- round(as.numeric(newDataAll$LatitudeDegrees), 6)
    newDataAll$LongitudeDegrees <- round(as.numeric(newDataAll$LongitudeDegrees), 6)
    newDataAll$AltitudeMeters <- round(as.numeric(newDataAll$AltitudeMeters), 1)
    newDataAll$DistanceMeters <- round(as.numeric(newDataAll$DistanceMeters), 1)
    newDataAll$HeartRateBpm <- round(as.numeric(newDataAll$HeartRateBpm), 0)
    
    newDataAll$GPS <- ifelse(is.na(newDataAll$LatitudeDegrees) | is.na(newDataAll$LongitudeDegrees), "-", "+")
    newDataAll$Id <- ifelse(is.na(newDataAll$Id), "???", newDataAll$Id)
    
    if (!is.null(newDataAll)) {
      # entferne Zeilen ohne datetime-Signatur
      # entferne Dupletten auf Basis datetime-Signatur
      # order by datetime-Signatur
      # berechne weitere Spalten
    }

    # Meldung zu Fehlern behandeln
    if (!is.null(errormsg)) {
      errormsg <- paste0("@Fehlermeldung@", errormsg)
      showMessage(errormsg, input$language)
    }
    
    # umbennen und erweitern von Spalten
    colnames(newDataAll)[match("Time", colnames(newDataAll))] <- "DTG"
    colnames(newDataAll)[match("Id", colnames(newDataAll))] <- "Label"
    newDataAll$Date <- format(newDataAll$DTG, "%Y-%m-%d")
    newDataAll$Time <- format(newDataAll$DTG, "%H:%M:%S")
    
    # entferne bei neu geladenen Daten immer alle globalen Filter
    updateCheckboxInput(session, "filterById", value = FALSE)
    updateSelectInput(session, "filterByIdSelect",
                      choices = unique(newDataAll[,match("Label", colnames(newDataAll))]))
    updateCheckboxInput(session, "filterByDate", value = FALSE)
    updateSelectInput(session, "filterByDateSelect",
                      choices = unique(newDataAll[,match("Date", colnames(newDataAll))]))
    
    if (is.null(newDataAll)) {
      updateCheckboxInput(session, "dataAvailable", value = FALSE)
    } else {
      updateCheckboxInput(session, "dataAvailable", value = TRUE)
    }
    
    return(newDataAll)
  }
  
# -------------------------------------------------------------------------------------------------------------- 

  # Wrapper-Funktion fuer das Verstecken eines TabPanels
  hideTab <- function(mytabsetName, child, selectInstead = NULL, status){
    session$sendCustomMessage(type = "hideTab", message = list(tabsetName = mytabsetName, number = child, hide = status))
    # 
    if (!is.null(selectInstead)) {
      updateTabsetPanel(session, inputId = mytabsetName, selected = selectInstead)
    } else {
      updateTabsetPanel(session, inputId = mytabsetName)
    }
  }
  
  # Funktion um in einem ganzen TabSet Panels an- oder abzuschalten. tabsToHide ist ein Vektor mit dem die Nummern
  # der tabPanels uebergeben werden koennen. Die Funktion ist abgestimmt auf den tabPanel-Namen 'tPx mit x, das der Nummer
  # entspricht. Im tabsetPanel muessen dann die Panels den value 'tPx' besitzen.
  # Das erste verbleibende Panel wird dann aktiv geschaltet.
  showTabsInTabset <- function(tabSet, tabsToShow) {
    allTabs = c(0, 1, 2, 3, 4, 5)
    for(i in allTabs) {
      hideTab(mytabsetName = tabSet, child = i, status = 0)
    }
    newTabToSelect <- (which(allTabs %in% tabsToShow) - 1)[1]
    for(i in tabsToShow) {
        hideTab(mytabsetName = tabSet, child = i, status = 1, selectInstead = paste0("tP",newTabToSelect))
    }
  }
  
# -------------------------------------------------------------------------------------------------------------- 
  
  modifyDF <- function(df, lang, filterColId, filterId, filterColDate, filterDate) {
    if (is.null(df)) {
      return(NULL)
    } else {
      colnames(df)[match("Time", colnames(df))] <- translate("Time", lang)
      colnames(df)[match("HeartRateBpm", colnames(df))] <- translate("HeartRateBpm", lang)
      colnames(df)[match("LatitudeDegrees", colnames(df))] <- translate("LatitudeDegrees", lang)
      colnames(df)[match("LongitudeDegrees", colnames(df))] <- translate("LongitudeDegrees", lang)
      colnames(df)[match("AltitudeMeters", colnames(df))] <- translate("AltitudeMeters", lang)
      colnames(df)[match("DistanceMeters", colnames(df))] <- translate("DistanceMeters", lang)



      updateCheckboxInput(session, "filterById", translate("filterId", lang))
      if (filterColId) {
        df <- df[which(df$Label==filterId),]
      }
      colnames(df)[match("Label", colnames(df))] <- translate("Label", lang)
      
      updateCheckboxInput(session, "filterByDate", translate("filterDate", lang))
      if (filterColDate) {
        df <- df[which(df$Date==filterDate),]
      }
      colnames(df)[match("Date", colnames(df))] <- translate("Date", lang)
      
      return(df)
    }
  }
# -------------------------------------------------------------------------------------------------------------- 
  
  assignGroups <- function(df, hrmin, hrmax, col1, col2, col3, lang) {
    if (is.null(df)) {return(NULL)}
    df$Group <- ifelse(df[,c(translate("HeartRateBpm", lang))]<hrmin, col1,
                       ifelse(df[,c(translate("HeartRateBpm", lang))]>hrmax, col3, col2))
    return(df)
  }
  
# --------------------------------------------------------------------------------------------------------------
  
  calculateSummary <- function(df, hrmin, hrmax) {
    if (is.null(df)) {return(NULL)}
    df$Group1 <- ifelse(df[,c("HeartRateBpm")]<hrmin, 1, 0)
    df$Group2 <- ifelse(df[,c("HeartRateBpm")]<=hrmax, 1, 0)
    df$Group2 <- df$Group2 - df$Group1
    df$Group3 <- ifelse(df[,c("HeartRateBpm")]>hrmax, 1, 0)
     
    mydf <- aggregate(df[,c("Group1", "Group2", "Group3")], by=list(Label=df$Label), FUN=sum, na.rm=TRUE)
    
    return(mydf)
  }
  
# -------------------------------------------------------------------------------------------------------------- 
  
  
# ##############################################################################################################
#
#    Erzeuge Reactive values & conductors
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

  values <- reactiveValues(reactiveDF = NULL)
  
  coroRawData <- reactive({
    importFiles(input$userfiles)
  })
  
  coroData <- reactive({
    modifyDF(coroRawData(), input$language, input$filterById, input$filterByIdSelect,
             input$filterByDate, input$filterByDateSelect)
  })
  
  coroDataPlot <- reactive({
    assignGroups(coroData(), input$hfBer[1], input$hfBer[2], input$cpUnder, input$cpRight, input$cpAbove,
                 input$language)
  })
  
  coroDataSummary <- reactive({
    calculateSummary(coroRawData(), input$hfBer[1], input$hfBer[2])
  })

# --------------------------------------------------------------------------------------------------------------
  
  
  
# ##############################################################################################################
#
#    Verbinde Reactive sources mit Reactive endpoints
#
# ##############################################################################################################
  
# --------------------------------------------------------------------------------------------------------------
  # Anpassung bei Sprachwechsel
  
  # Flag
  output$flag <- renderUI({
    tags$img(src = paste0("data:image/png;base64,",translate("flag", input$language)), id = "languageflag")
  })
  
  # headerPanel
  # TODO
  output$hilfe_t <- renderText({translate("Hilfe", input$language)})
  output$impressum_t <- renderText({translate("Impressum", input$language)})
  output$pers0_t <- renderText({translate("PersonenDaten2", input$language)})
  output$pers1_t <- renderText({translate("PersonenDaten2", input$language)})
  
  # titlePanel
  output$title_t <- renderText({translate("CoroVisTitle", input$language)})
  
  # workingPanel > patientPanel
  # TODO
  output$name_t <- renderText({translate("Name", input$language)})
  output$vorname_t <- renderText({translate("Vorname", input$language)})
  output$groesse_t <- renderText({translate("Groesse", input$language)})
  output$gewicht_t <- renderText({translate("Gewicht", input$language)})
  
  # workingPanel > sidebarPanel
  output$fileTitle <- renderText({translate("filesHeader", input$language)})
  output$fileLabel <- renderText({translate("filesButton", input$language)})
  output$fileCounter <- renderText({
    if (is.null(input$userfiles)) {
      return (translate("filesLoaded0", input$language))
    } else if (nrow(input$userfiles)>1) {
      return(paste(nrow(input$userfiles), translate("filesLoaded2", input$language)))
    } else {
      return (translate("filesLoaded1", input$language))
    }
  })
  output$filterTitle <- renderText({translate("filterTitle", input$language)})
  output$bmi <- renderText({calculateBMI(input$groesse, input$gewicht)})
  output$ageTitle <- renderText({translate("Alter", input$language)})
  output$alterausgabe <- renderText({calculateAge(input$inpAlter)})
  
  
  
  # TODO
  output$hfMax_t <- renderText({translate("Maximale Herzfrequenz", input$language)})
  output$frequenzbereich_t <- renderText({translate("Frequenzbereich", input$language)})
  
  
  
  
  
  # workingPanel > mainPanel > tp0
  output$start <- renderText({translate("Start", input$language)})
  output$startTitle <- renderText({translate("greeting", input$language)})
  output$startSubtitle <- renderText({translate("greetingSubtitle", input$language)})
  
  # workingPanel > mainPanel > tp1 TABLE
  output$daten_t <- renderText({translate("Daten", input$language)})
  tableReRenderer <- function(languageStyle) {
    output$coroTable <- renderDataTable({
      lang <- input$language
      cols <- c(translate("Label", lang), translate("Date", lang), translate("Time", lang),
                translate("HeartRateBpm", lang), "GPS", translate("AltitudeMeters", lang),
                translate("DistanceMeters", lang))
      coroData()[cols]
    }, options = list(
      lengthMenu = c(10, 25, 100),
      pageLength = 10,
      language = languageStyle
    ))
  }
  
  # workingPanel > mainPanel > tp2 PLOT
  # TODO
  output$zeit_t <- renderText({translate("Plot", input$language)})
  output$axisXSelectLabel <- renderText({translate("XAchse", input$language)})
  output$axisYSelectLabel <- renderText({translate("YAchse", input$language)})
  output$explorationPlot <- renderPlot({
    if (is.null(coroDataPlot())) {return(NULL)}
    x <- times(coroDataPlot()[,input$axisXSelect])
    y <- as.numeric(coroDataPlot()[,input$axisYSelect])
    z <- coroDataPlot()[,c("Group")]
    ymin <- ifelse("1" %in% input$plotInclude0 && 0<input$axisYZoom[1], 0, input$axisYZoom[1])
    
    par(mar = c(5, 5, 0.2, 2))
    plot(x, y, pch = 16, col=z, main = NULL, xlab = input$axisXSelect, ylab = input$axisYSelect,
         xlim = c(input$axisXZoom[1], input$axisXZoom[2]), ylim = c(ymin, input$axisYZoom[2]),
         cex = input$plotPointsize, las = 1,
         cex.lab = input$plotTextsize, cex.main = input$plotTextsize)
    if ("2" %in% input$plotInclude0) {lines(x, y, pch=16)}
  })
  
  
  # workingPanel > mainPanel > tp3 MAP
  # TODO
  output$karte_t <- renderText({translate("Karte", input$language)})
  output$tOut1 <- renderLeaflet({
    if (is.null(coroDataPlot())) {
      m <- leaflet() %>% addTiles() 
    } else {
      cols <- colnames(coroDataPlot())
      i <- match(translate("LatitudeDegrees", input$language), cols)
      myLat <- coroDataPlot()[,i]
      i <- match(translate("LongitudeDegrees", input$language), cols)
      myLng <- coroDataPlot()[,i]
      i <- match(translate("HeartRateBpm", input$language), cols)
      myHR <- coroDataPlot()[,i]
      m <- leaflet() %>% addProviderTiles(input$mapTilesSelect) %>% addCircles(data = coroDataPlot(),
                                               lat = ~myLat, lng = ~myLng, popup= ~myHR, radius=5,
                                               color= ~Group, stroke = TRUE, fillOpacity = 1)
    }
  })
  
  
  
  # workingPanel > mainPanel > tp4 SUMMARY
  # TODO
  output$gesamt_t <- renderText({translate("Zusammenfassung", input$language)})
  output$summaryTitle <- renderText({translate("Statistik", input$language)})
  output$tOut2 <- renderTable(coroDataSummary(), digits = 0)
  plotReRenderer <- function(hsize) {
    output$summaryPlot <- renderPlot({
      
      
      counts <- t(as.matrix(coroDataSummary()[2:4]))
      counts <- counts[,ncol(counts):1]
      
      par(mar = c(5, 12, 0.2, 2))
      barplot(counts, horiz = TRUE, names.arg = rev(coroDataSummary()$Label), las=1,
              col = c(input$cpUnder, input$cpRight, input$cpAbove))
    }, height = hsize)    
  }

  
  # workingPanel > mainPanel > tp5 SETTINGS
  # TODO
  output$settings <- renderText({translate("Einstellungen", input$language)})
  
  output$settingsColorTitle <- renderText({translate("Farbwerte", input$language)})
  output$settingsColor1Label <- renderText({translate("UnterRef", input$language)})
  output$settingsColor2Label <- renderText({translate("ImRef", input$language)})
  output$settingsColor3Label <- renderText({translate("ÜberRef", input$language)})
  output$settingsColorResetLabel <- renderText({translate("FarbenZurück", input$language)})
  
  output$settingsHFmaxTitle <- renderText({translate("maximaleHF", input$language)})
  
  # helpPanel
  output$helpTitle <- renderText({translate("Hilfe", input$language)})
  output$helpBackLabel <- renderText({translate("Zur?ck", input$language)})
  
  # imprintPanel
  output$imprintTitle <- renderText({translate("Impressum", input$language)})
  output$imprintHTML <- renderUI({getPage(translate("IncImpressum", input$language))})
  output$imprintBackLabel <- renderText({translate("Zur?ck", input$language)})
  
# --------------------------------------------------------------------------------------------------------------
  

  
# ##############################################################################################################
#
#    Definiere javascript-Events
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

  # Sprachwechsel bei Klick auf Flagge
  shinyjs::onclick("languageflag", {
    change_language()                                           # TEMP: Abwaertskompatibilitaet zu alter Version
    updateSliderInput(session, inputId = "language", value = (input$language %% numDics) + 1)
  })
  
  # Einblenden der Hilfe
  shinyjs::onclick("hil", {
    updateSelectInput(session, inputId = "currentPanel", selected = "helpPanel")
  })
  
  # Einblenden des Impressums
  shinyjs::onclick("imp", {
    updateSelectInput(session, inputId = "currentPanel", selected = "imprintPanel")
  })
  
# ##############################################################################################################
#
#    Registriere Observer-Ereignisse
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$helpBack, {
    updateSelectInput(session, inputId = "currentPanel", selected = "workingPanel")
  })
  
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$imprintBack, {
    updateSelectInput(session, inputId = "currentPanel", selected = "workingPanel")
  })
  
# --------------------------------------------------------------------------------------------------------------

  observeEvent(input$language, {
    languageList <- list("paginate" = list("next" = translate("next", input$language),
                                      "previous" = translate("previous", input$language)),
                    "search" = translate("search", input$language),
                    "lengthMenu" = translate("lengthMenu", input$language),
                    "loadingRecords" = translate("loadingRecords", input$language),
                    "processing" = translate("processing", input$language),
                    "info" = translate("info", input$language),
                    "infoEmpty" = translate("infoEmpty", input$language),
                    "zeroRecords" = translate("zeroRecords", input$language),
                    "emptyTable" = translate("emptyTable", input$language),
                    "infoFiltered" = translate("infoFiltered", input$language))
    tableReRenderer(languageList)
  })
  
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$userfiles, {
    updateTabsetPanel(session, "tP", selected = "tP1")
  })
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$resetColors, {
    #TODO Farben zurücksetzen
    updateColourInput(session, "cpUnder", value = basicCol[1])
    updateColourInput(session, "cpRight", value = basicCol[2])
    updateColourInput(session, "cpAbove", value = basicCol[3])
  })
  
  observeEvent(input$summaryPlotExpander, {
    plotReRenderer(input$summaryPlotExpander*400)
  })
})
