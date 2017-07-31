# ###################################################################################################################
#                                                                                                                   #
#    CoroVis - server.R                                                                                             #
#    Coding vom ....                                                                                                #
#    Copyright by Shiny-AG                                                                                          #
#                                                                                                                   #
# ###################################################################################################################

# Benoetigte Packages -----------------------------------------------------------------------------------------------

library(shiny)    
library(shinyjs)  
library(chron)    
library(XML)     
library(leaflet)
library(colourpicker)
library(rmarkdown)

# Server-Funktion ---------------------------------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # Max upload Filegroesse bis 15 MB
  options(shiny.maxRequestSize=15*1024^2)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Fuer die Herzfrequenzeingaben, hier auf Session-Ebene
  #rVal <- reactiveValues(hfBo=0, hfBu=0)
  #hfMaxOut <- 0
  # maxFr <- 0

  
  

  # ----------------------------------------------------------------
  # Allgemeine Variablen - fuer die extrahierten Mess- und GPS-Daten
  # ----------------------------------------------------------------
  exerciseData <- NULL    # Liste für die Daten der Trainingseinheiten
  unitList <- NULL        # Liste der Namen für die Trainingseinheiten
  
  # ------------------------------------------
  # Vorbereitungsfunktionen zum Rendern des UI
  # ------------------------------------------
  
  # # Fuer die Risikoklasse
  # renderRiskClass <- function() {
  #   output$riskclass <- renderUI({
  #     tagList(
  #       radioButtons("inpRiskClass", label = a(href = 'LL_KActiv.pdf', translate("Risikoklasse"), target = "_blank"), choiceNames = list('A', 'B', 'C', 'D'), choiceValues = list(hfMaxGeneral, hfMaxGeneral*0.8, hfMaxGeneral*0.6, hfMaxGeneral*0.5), inline = TRUE)
  #     )
  #   })
  # }
  
  # # Fuer die Belastungsintensitaet
  # renderStrIntensity <- function() {
  #   output$intensity <- renderUI({
  #     tagList(
  #       selectInput("risk", label = textOutput("risiko_t"), choices = list(translate("minimal"), 
  #                                                                          translate("leicht"),
  #                                                                          translate("moderat"),
  #                                                                          translate("schwer"),
  #                                                                          translate("sehr schwer"),
  #                                                                          translate("maximal")), selected = 1)
  #     )      
  #   })
  # }

  # -------------------
  # Hilfsfunktionen ...
  # -------------------
  
  # Sprachwechsel bei Click auf die Fahne
  change_language <- function() {
    # Die entsprechende Sprache einstellen...
    iLang <<- ifelse(iLang > numDics-1, 1, iLang+1)
    setFlag()
  }

  # -------------------------------------------------------
  # Sprache auf dem UI einstellen bzw. auf Klick hin setzen
  # -------------------------------------------------------
  
  setFlag <- function() {

    # # Sidebar
    # # renderRiskClass()
    # renderStrIntensity()
    output$risiko_t <- renderText({ paste(translate("Belastungsintensitaet")) })
    
    # mainPanel
    output$selAxisX <- renderText({ paste(translate("XAchse"))})
    output$selAxis <- renderText({ paste(translate("YAchse")) })

    # Wieder Einstellen von bestimmten ausgewaehlten Parametern nach dem Sprachwechsel...
    updateDateInput(session, "inpAlter", value = NULL)        # Alter wieder einstellen

  }

  # -----------------------
  # UI - Beschriftungen ...
  # -----------------------
  
  setFlag()

  # ----------------------------------------------------
  # Bestimmte Variablen und Konstellation beobachten ...
  # ----------------------------------------------------
  
  # Reaktive Variablen festelegen
  #rVal <- reactiveValues(hfBo=0, hfBu=0, hfMaxOut = 0, maxFr = 0)
    
  observe({
    # Die ausgewaehlten Panels (hinterlegt in einer CheckboxGroup) werden mit folgender Anweisung dargestellt
    # Die Tabs muessen als integer-Vektor uebergeben werden. Innerhalb der Observe-Funktion werden Aenderungen
    # der CheckboxGroup wahrgenommen und das TabsetPanel aktualisiert.
    #showTabsInTabset("tP", as.integer(input$tabsToShow))
    
  })
  
  # ----------------------
  # Die Events beachten...
  # ----------------------

  # Wenn die Risikoklasse, die Belastungsintensität oder die Maximalherzfrequenz eingestellt wird

  # observeEvent(input$inpRiskClass, {
  #   
  #   heartRateLimits <<- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (as.numeric(input$inpRiskClass) - hfMinGeneral) + hfMinGeneral
  #   maxFr <<- as.numeric(input$inpRiskClass)
  # 
  #   updateSliderInput(session, "overrideMaxHF", value = maxFr)
  #   updateSliderInput(session, "hfMax", max = maxFr, value = maxFr)
  #   
  # })
  
  observeEvent(input$hfMax, {
    
    # heartRateLimits <<- c(0, 0.34, 0.54, 0.69, 0.89, 0.97, 1.0) * (input$hfMax - hfMinGeneral) + hfMinGeneral
    # limit <- hfBereiche[[ifelse(is.null(input$risk), 1, input$risk)]]
    # rVal$hfBu <- heartRateLimits[limit]
    # rVal$hfBo <- heartRateLimits[limit+1]
    
    updateSliderInput(session, "hfBer", min = input$hfMax[1], max = input$hfMax[2], value = getRange(input$hfMax[1], input$hfMax[2], input$intensity))
    
  })
  
  observeEvent(input$intensity, {
    
    # limit <- hfBereiche[[ifelse(is.null(input$risk), 1, input$risk)]]
    # rVal$hfBu <- heartRateLimits[limit]
    # rVal$hfBo <- heartRateLimits[limit+1]
 
    updateSliderInput(session, "hfBer", value = getRange(input$hfMax[1], input$hfMax[2], input$intensity))

  })
  
  # observeEvent(input$hfBer, {
  #   rVal$hfBu <- input$hfBer[1]
  #   rVal$hfBo <- input$hfBer[2]
  # })
  


  
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
            newData <- importDataCSV(currentFile$datapath, isolate(input$timezoneSlider))
          } else {
            newData <- importDataGPX(currentFile$datapath)
          }
  
          if (is.null(newData)) {
            # speichere Fehlermeldung und weiter
            errormsg <- paste(errormsg, paste0("No data in file: ", currentFile$name, ";"))
  
          } else {
            # bereinige und merge neue Daten mit newDataAll
            newData <- cleanData(newData)
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
    
    # sortieren, umbennen und erweitern von Spalten
    if (!is.null(newDataAll)) {
      newDataAll <- newDataAll[!is.na(newDataAll$HeartRateBpm),]
      newDataAll <- newDataAll[order(newDataAll$Time),]
      row.names(newDataAll) <- NULL
      newDataAll$GPS <- ifelse(is.na(newDataAll$LatitudeDegrees) | is.na(newDataAll$LongitudeDegrees), "-", "+")
      newDataAll$Id <- ifelse(is.na(newDataAll$Id), "???", newDataAll$Id)
      colnames(newDataAll)[match("Time", colnames(newDataAll))] <- "DTG"
      colnames(newDataAll)[match("Id", colnames(newDataAll))] <- "Label"
      colnames(newDataAll)[match("HeartRateBpm", colnames(newDataAll))] <- "HR"
      colnames(newDataAll)[match("LatitudeDegrees", colnames(newDataAll))] <- "lat"
      colnames(newDataAll)[match("LongitudeDegrees", colnames(newDataAll))] <- "lon"
      colnames(newDataAll)[match("AltitudeMeters", colnames(newDataAll))] <- "alt"
      colnames(newDataAll)[match("DistanceMeters", colnames(newDataAll))] <- "dist"
      newDataAll$Date <- format(newDataAll$DTG, "%Y-%m-%d")
      newDataAll$Time <- format(newDataAll$DTG, "%H:%M:%S")
      newDataAll <- newDataAll[!duplicated(newDataAll),]
      
      # Zusatzfilter: DTG-Dupletten entfernen
      DTGduplicates <- grep(TRUE, duplicated(newDataAll$DTG))
      if (length(DTGduplicates)>0) {
        newDataAll <- newDataAll[-DTGduplicates, ]
      }

      # wenn durch quality filter keine Daten verbleiben:
      if (nrow(newDataAll)==0) {
        newDataAll <- NULL
        errormsg <- paste(errormsg, "After quality filter no data left!")
      } else {
        newDataAll$deltaTime <- c(0, as.numeric(newDataAll$DTG[2:nrow(newDataAll)] - newDataAll$DTG[1:nrow(newDataAll)-1]))
        
        # berechne Entfernung zwischen aufeinanderfolgenden GPS-Koordinaten, wenn nicht als DistanceMeters verfügbar
        k <- ncol(newDataAll)
        l <- nrow(newDataAll)
        newDataAll$latDiff <- c(NA, newDataAll$lat[2:l]-newDataAll$lat[1:l-1]) * 60 * 1852
        newDataAll$lonDiff <- c(NA, newDataAll$lon[2:l]-newDataAll$lon[1:l-1]) * 60 * 1852 * cos(newDataAll$lat*pi/180)
        newDataAll$delta <- sqrt(newDataAll$latDiff^2 + newDataAll$lonDiff^2)
        newDataAll$delta <- ifelse(is.na(newDataAll$delta), 0, newDataAll$delta)
        newDataAll$absDist <- c(NA, newDataAll$dist[2:l]-newDataAll$dist[1:l-1])
        newDataAll$absDist <- ifelse(newDataAll$absDist<=0, NA, newDataAll$absDist)                     # filter out 0
        newDataAll$absDist <- ifelse(newDataAll$absDist/newDataAll$delta > 100, NA, newDataAll$absDist) # filter out wrong input data
        newDataAll$deltaDist <- ifelse(is.na(newDataAll$absDist), newDataAll$delta, newDataAll$absDist)
        newDataAll$cumDist <- round(cumsum(newDataAll$deltaDist), 1)
        newDataAll$speed <- ifelse(newDataAll$deltaTime == 0, NA, 3.6 * newDataAll$deltaDist / newDataAll$deltaTime)
        newDataAll <- newDataAll[-seq(k+1,k+4,1)]
      }
    }

    # Meldung zu Fehlern behandeln
    if (!is.null(errormsg)) {
      errormsg <- paste0("@Fehlermeldung@", errormsg)
      showMessage(errormsg, input$language)
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
      # Sprachvariabler Header
      colnames(df)[match("Time", colnames(df))] <- translate("Time", lang)
      colnames(df)[match("HR", colnames(df))] <- translate("HeartRateBpm", lang)
      colnames(df)[match("lat", colnames(df))] <- translate("LatitudeDegrees", lang)
      colnames(df)[match("lon", colnames(df))] <- translate("LongitudeDegrees", lang)
      colnames(df)[match("alt", colnames(df))] <- translate("AltitudeMeters", lang)
      colnames(df)[match("cumDist", colnames(df))] <- translate("DistanceMeters", lang)
      colnames(df)[match("speed", colnames(df))] <- translate("Speed", lang)



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
    df$Group1 <- ifelse(df[,c("HR")]<hrmin, 1, 0)
    df$Group2 <- ifelse(df[,c("HR")]<=hrmax, 1, 0)
    df$Group2 <- df$Group2 - df$Group1
    df$Group3 <- ifelse(df[,c("HR")]>hrmax, 1, 0)
    
    mydf <- aggregate(df[,c("Group1", "Group2", "Group3")], by=list(Date=df$Date), FUN=sum, na.rm=TRUE)
    
    return(mydf)
  }
  
# -------------------------------------------------------------------------------------------------------------- 
  
  
# ##############################################################################################################
#
#    Erzeuge Reactive values & conductors
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

  values <- reactiveValues(coroRawData = NULL)
  
  coroData <- reactive({
    modifyDF(values$coroRawData, input$language, input$filterById, input$filterByIdSelect,
             input$filterByDate, input$filterByDateSelect)
  })
  
  coroDataPlot <- reactive({
    assignGroups(coroData(), input$hfBer[1], input$hfBer[2], input$cpUnder, input$cpRight, input$cpAbove,
                 input$language)
  })
  
  coroDataSummary <- reactive({
    calculateSummary(values$coroRawData, input$hfBer[1], input$hfBer[2])
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
  
  output$gebdat_t <- renderText({translate("Geburtsdatum", input$language)})
  output$gebdat_f <- renderUI({translate("dd.mm.yyyy", input$language)})
  output$gebdat_l <- renderUI({translate("de", input$language)})
  dateReRenderer <- function(lang) {
    output$gebdat <- renderUI({dateInput("inpAlter", value = isolate(input$inpAlter), label = NULL,
                                         format = translate("dd.mm.yyyy", lang), language = translate("de", lang))
    })
  }
  output$gesch_t <- renderText({translate("Geschlecht", input$language)})
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
  output$riskfactorTitle <- renderText({translate("riskfactorTitle", input$language)})
  #output$hrTitle <- renderText({translate("hrTitle", input$language)})
  output$bmiTitle <- renderText({translate("bmi", input$language)})
  output$bmi <- renderText({
      calculateBMI(input$groesseKA, input$gewichtKA,
                   input$groesse, input$gewicht, input$language)
  })
  output$sex_t <- renderText({translate("Geschlecht", input$language)})
  output$sex <- renderText({
    getSalutation(input$geschKA, input$inpGesch, input$language, short=FALSE)
  })
  output$ageTitle <- renderText({translate("Alter", input$language)})
  output$alterausgabe <- renderText({
      calculateAge(input$inpAlterKA, input$inpAlter, input$language)
  })
  
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
    #ymin <- ifelse("1" %in% input$plotInclude0 && 0<input$axisYZoom[1], 0, input$axisYZoom[1])
    ylim <- ifelse("1" %in% input$plotInclude0 && 0<min(y, na.rm = TRUE), 0, min(y, na.rm = TRUE))
    ylim <- c(ylim, max(y, na.rm = TRUE))

    par(mar = c(5, 5, 0.2, 2))
    plot(x, y, pch = 16, col=z, main = NULL, xlab = input$axisXSelect, ylab = input$axisYSelect,
         #xlim = c(input$axisXZoom[1], input$axisXZoom[2]), ylim = c(ymin, input$axisYZoom[2]),
         ylim = ylim,
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
  output$dlReport <- renderText({translate("Report", input$language)})
  output$chPlotSize <- renderText({translate("PlotSize", input$language)})
  output$tOut2 <- renderTable({
    if (is.null(coroDataSummary())) {
      return(NULL)
    } else {
      mydf <- coroDataSummary()
      colnames(mydf) <- translate(c("Date", "UnterRef", "ImRef", "ÜberRef"), input$language)
      return(mydf)
    }
    
    
    }, digits = 0)
  plotReRenderer <- function(hsize) {
    output$summaryPlot <- renderPlot({
      
      
      counts <- t(as.matrix(coroDataSummary()[2:4]))
      counts <- counts[,ncol(counts):1]
      
      par(mar = c(5, 12, 0.2, 2))
      barplot(counts, horiz = TRUE, names.arg = rev(coroDataSummary()$Date), las=1,
              col = c(input$cpUnder, input$cpRight, input$cpAbove))
    }, height = hsize)    
  }

  # --- Generiere PDF Report ---------------------------------------------------
  output$report <- downloadHandler(
    filename = "coroVisReport.pdf",
    content = function(file) {
      myPatient <- c(
        getSalutation(input$geschKA, input$inpGesch, input$language),
        getValue(TRUE, input$nachname, input$language),
        getValue(TRUE, input$vorname, input$language),
        getValue(input$inpAlterKA, input$inpAlter, input$language),
        calculateAge(input$inpAlterKA, input$inpAlter, input$language),
        getValue(input$groesseKA, input$groesse, input$language),
        getValue(input$gewichtKA, input$gewicht, input$language),
        calculateBMI(input$gewichtKA, input$gewichtKA,
                     input$groesse, input$gewicht, input$language)
      )
      myParameter <- c(
        input$hfBer[1],  # lLimit
        input$hfBer[2],  # uLimit
        input$cpUnder,  # lCol
        input$cpRight,  # iCol
        input$cpAbove,  # uCol
        input$hfMax[1],  # HFmin
        input$hfMax[2],  # HFmax
        input$intensity  # intensity
      )
      createPDF(myPatient, myParameter, values$coroRawData, file, input$language)
    }
  )

  
  # workingPanel > mainPanel > tp5 SETTINGS
  # TODO
  output$settings <- renderText({translate("Einstellungen", input$language)})
  
  output$settingsColorTitle <- renderText({translate("Farbwerte", input$language)})
  output$settingsColor1Label <- renderText({translate("UnterRef", input$language)})
  output$settingsColor2Label <- renderText({translate("ImRef", input$language)})
  output$settingsColor3Label <- renderText({translate("ÜberRef", input$language)})
  output$settingsColorResetLabel <- renderText({translate("FarbenZurück", input$language)})
  
  output$settingsHFmaxTitle <- renderText({translate("maximaleHF", input$language)})
  
  output$SelectMapLayer <- renderText({ translate("SelectMapLayer", input$language)})
  output$timeShiftCSV <- renderText({ translate("ZeitVCSV", input$language)})
  output$spotSize <- renderText({ translate("PunktGr", input$language)})#
  output$textSize <- renderText({ translate("TextGr", input$language)})
  
  # helpPanel
  output$helpTitle <- renderText({translate("Hilfe", input$language)})
  output$helpHTML <- renderUI({getPage(translate("IncHilfe", input$language))})
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
  
  # Ein-/Ausblenden des patientPanels
  shinyjs::onclick("decrease", updateCheckboxInput(session, "iPanelOn", value = !input$iPanelOn))
  shinyjs::onclick("increase", updateCheckboxInput(session, "iPanelOn", value = !input$iPanelOn))
  
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
    lang <- input$language
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
    dateReRenderer(lang)
    updateRadioButtons(session, "inpGesch", choiceValues = list("m", "f"), inline = TRUE, selected = input$inpGesch,
        choiceNames = list(translate("maennlich", lang), translate("weiblich", lang)))
    
    myChoices <- c("Time", "DistanceMeters")
    newSelection <- getNewSelection(input$axisXSelect, lang, myChoices)
    newChoices <- c(translate(myChoices, lang))
    updateSelectInput(session, "axisXSelect", choices = newChoices, selected = newSelection)
    
    myChoices <- c("HeartRateBpm", "AltitudeMeters", "DistanceMeters", "Speed")
    newSelection <- getNewSelection(input$axisYSelect, lang, myChoices)
    newChoices <- c(translate(myChoices, lang))
    updateSelectInput(session, "axisYSelect", choices = newChoices, selected = newSelection)
    
    newChoices <- list(1,2,3,4,5,6)
    names(newChoices) <- translate(c("level1", "level2", "level3", "level4", "level5", "level6"), lang)
    updateSelectInput(session, "intensity", choices = newChoices, selected = input$intensity)
    
  })
  
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$userfiles, {
    values$coroRawData <- importFiles(input$userfiles)
    # entferne bei neu geladenen Daten immer alle globalen Filter
    updateCheckboxInput(session, "filterById", value = FALSE)
    updateCheckboxInput(session, "filterByDate", value = FALSE)
    updateSelectInput(session, "filterByIdSelect",
                      choices = unique(values$coroRawData[,match("Label", colnames(values$coroRawData))]))
    updateSelectInput(session, "filterByDateSelect",
                      choices = unique(values$coroRawData[,match("Date", colnames(values$coroRawData))]))
    
    if (!is.null(values$coroRawData)) {
      updateCheckboxInput(session, "dataAvailable", value = TRUE)
      
      
      
      showModal(modalDialog(title = translate("question", isolate(input$language)),
                            translate("PersonenDaten", isolate(input$language)),
                            footer = tagList(
                              modalButton(translate("yes", isolate(input$language))),
                              actionButton("patientNew", translate("no", isolate(input$language)))
                            )
      ))
    } else {
      updateCheckboxInput(session, "dataAvailable", value = FALSE)
    }
  })
# --------------------------------------------------------------------------------------------------------------
  
  observe({
    if (is.null(values$coroRawData)) {
      updateTabsetPanel(session, "tP", selected = "tP0")
      for (i in 1:4) {hideTab(mytabsetName = "tP", child = i, status = 0)}
    } else {
      for (i in 1:4) {hideTab(mytabsetName = "tP", child = i, status = 1)}
      updateTabsetPanel(session, "tP", selected = "tP1")
    }
  })
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$patientNew, {
    updateTextInput(session, "nachname", value = "")
    updateTextInput(session, "vorname", value = "")
    updateCheckboxInput(session, "inpAlterKA", value = FALSE)
    updateCheckboxInput(session, "geschKA", value = FALSE)
    updateCheckboxInput(session, "groesseKA", value = FALSE)
    updateCheckboxInput(session, "gewichtKA", value = FALSE)
    removeModal()
  })
  
# --------------------------------------------------------------------------------------------------------------
  
  # observeEvent(input$axisXSelect, {
  #   updateSliderInput(session, "axisXZoom", min = myMin, max = myMax, value = c(myMin, myMax))
  # })
  
# --------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$resetColors, {
    # Farben zurücksetzen
    updateColourInput(session, "cpUnder", value = basicCol[1])
    updateColourInput(session, "cpRight", value = basicCol[2])
    updateColourInput(session, "cpAbove", value = basicCol[3])
  })

# --------------------------------------------------------------------------------------------------------------
    
  observeEvent(input$summaryPlotExpander, {
    plotReRenderer(input$summaryPlotExpander*400)
  })
})
