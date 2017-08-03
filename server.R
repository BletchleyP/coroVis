# ############################################################################ #
#                                                                              #
#    CoroVis - server.R                                                        #
#    Coding vom 30.07.2017                                                     #
#    Copyright by Gerhard Fuechsl, Enrico Georgi, Janina Rexin                 #
#                                                                              #
# ############################################################################ #

library(shiny)    
library(shinyjs)  
library(chron)    
library(XML)     
library(leaflet)
library(colourpicker)

# --- Server-Funktion ----------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # Max upload Filegroesse bis 15 MB
  options(shiny.maxRequestSize=15*1024^2)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
# ############################################################################ #
#                                                                              #
#    Definiere Funktionen mit Shiny-Interaktionen                              #
#                                                                              #
# ############################################################################ #


  
  # ############################################################################
  #' Importiert Trainingsdaten einer TCX-Datei und liefert Dataframe zurueck
  #'
  #' @param tcxFile datapath zu TCX-Datei
  #'
  #' @return strukturierter Dataframe
  #'
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
      
      withProgress(message = sprintf(translate("importDataMsg",
                                               isolate(input$language)),
                                     totalSize), value = 0, {
        
        # foreach <Activity>-tag
        for (i in 1:size) {
          startNode <- nodes[[1]][[i]]
          label <- c(Id = xmlValue(startNode[["Id"]]))
          subNodes <- getNodeSet(doc, paste0("(//ns:Activity)[", i,
                                             "]//ns:Trackpoint"), "ns")
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
              cat(paste0(paste(v[myHeader], collapse = ","), "\n"),
                  file=catTemp)
            }
          }
          
          # aktualisiere Fortschrittsanzeige
          incProgress(counter/totalSize)
        }
      })
      
      # beende den Datenstrom und importiere die Daten aus catTemp
      close(catTemp)
      df_trackpoints <- read.csv("temp.csv", header = TRUE,
                                 stringsAsFactors = FALSE,
                                 colClasses = "character")
      
    } else {
      df_trackpoints <- NULL
    }
    
    if (file.exists("temp.csv")) {file.remove("temp.csv")}
    
    return(df_trackpoints)
  }
  # ############################################################################
  

  
  # ############################################################################
  #' Funktion kontrolliert den Datenimport nach Datei-Upload
  #'
  #' @param fileDF alle hochgeladenen Dateien als Dataframe
  #'
  #' @return strukturierter Dataframe
  #'
  importFiles <- function(fileDF) {
    if (is.null(fileDF)) {return(NULL)}
    
    new <- NULL  # sammle alle Daten in einem gemeinsamen Dataframe
    errormsg <- NULL
    n <- nrow(fileDF)
    withProgress(message = sprintf(translate("importFileMsg",
                                             isolate(input$language)), n),
                 value = 0, {
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
            newData <- importDataCSV(currentFile$datapath,
                                     isolate(input$timezoneSlider))
          } else {
            newData <- importDataGPX(currentFile$datapath)
          }
  
          if (is.null(newData)) {
            # speichere Fehlermeldung und weiter
            errormsg <- paste(errormsg, paste0("No data in file: ",
                                               currentFile$name, ";"))
  
          } else {
            # bereinige und merge neue Daten mit new
            newData <- cleanData(newData)
            if (is.null(new)) {
              new <- newData
            } else {
              new <- mergeDF(new, newData, "Time")
            }
          }
        }
        
        # aktualisiere Fortschrittsanzeige
        incProgress(i/n)
      }
    })
    
    # sortieren, umbennen und erweitern von Spalten
    if (!is.null(new)) {
      new <- new[!is.na(new$HeartRateBpm),]
      new <- new[order(new$Time),]
      row.names(new) <- NULL
      isNAlat <- is.na(new$LatitudeDegrees)
      isNAlon <- is.na(new$LongitudeDegrees)
      new$GPS <- ifelse(isNAlat | isNAlon, "-", "+")
      new$Id <- ifelse(is.na(new$Id), "???", new$Id)
      colnames(new)[match("Time", colnames(new))] <- "DTG"
      colnames(new)[match("Id", colnames(new))] <- "Label"
      colnames(new)[match("HeartRateBpm", colnames(new))] <- "HR"
      colnames(new)[match("LatitudeDegrees",
                                 colnames(new))] <- "lat"
      colnames(new)[match("LongitudeDegrees",
                                 colnames(new))] <- "lon"
      colnames(new)[match("AltitudeMeters",
                                 colnames(new))] <- "alt"
      colnames(new)[match("DistanceMeters",
                                 colnames(new))] <- "dist"
      new$Date <- format(new$DTG, "%Y-%m-%d")
      new$Time <- format(new$DTG, "%H:%M:%S")
      new <- new[!duplicated(new),]
      
      # Zusatzfilter: DTG-Dupletten entfernen
      DTGduplicates <- grep(TRUE, duplicated(new$DTG))
      if (length(DTGduplicates)>0) {
        new <- new[-DTGduplicates, ]
      }

      # wenn durch quality filter keine Daten verbleiben:
      if (nrow(new)==0) {
        new <- NULL
        errormsg <- paste(errormsg, "After quality filter no data left!")
      } else {
        two <- new$DTG[2:nrow(new)]
        one <- new$DTG[1:nrow(new)-1]
        new$deltaTime <- c(0, as.numeric(two - one))
        
        # berechne Entfernung zwischen aufeinanderfolgenden GPS-Koordinaten
        k <- ncol(new)
        l <- nrow(new)
        fak <- cos(new$lat*pi/180)
        new$latDiff <- c(NA, new$lat[2:l]-new$lat[1:l-1]) * 60 * 1852
        new$lonDiff <- c(NA, new$lon[2:l]-new$lon[1:l-1]) * 60 * 1852 * fak
        new$delta <- sqrt(new$latDiff^2 + new$lonDiff^2)
        new$delta <- ifelse(is.na(new$delta), 0, new$delta)
        new$absDist <- c(NA, new$dist[2:l]-new$dist[1:l-1])
        new$absDist <- ifelse(new$absDist<=0, NA, new$absDist)   # filter out 0
        # filter out wrong input data
        new$absDist <- ifelse(new$absDist/new$delta > 100, NA, new$absDist)
        new$deltaDist <- ifelse(is.na(new$absDist), new$delta, new$absDist)
        new$cumDist <- round(cumsum(new$deltaDist), 1)
        new$speed <- ifelse(new$deltaTime == 0, NA,
                                   3.6 * new$deltaDist / new$deltaTime)
        new <- new[-seq(k+1,k+4,1)]
      }
    }

    # Meldung zu Fehlern behandeln
    if (!is.null(errormsg)) {
      errormsg <- paste0("@Fehlermeldung@", errormsg)
      showMessage(errormsg, input$language)
    }
    return(new)
  }
  # ############################################################################
  
  
  
  # ############################################################################
  #' Wrapper-Funktion fuer das Ausblenden eines TabPanels
  #'
  #' @param mytabsetName TabSetID
  #' @param child Kindelement
  #' @param selectInstead 
  #' @param status Ein- oder Ausblenden
  #'
  #' @return
  #' @export
  #'
  #' @examples
  hideTab <- function(mytabsetName, child, selectInstead = NULL, status = 0) {
    session$sendCustomMessage(type = "hideTab",
                              message = list(tabsetName = mytabsetName,
                                             number = child, hide = status))
    if (!is.null(selectInstead)) {
      updateTabsetPanel(session, inputId = mytabsetName,
                        selected = selectInstead)
    } else {
      updateTabsetPanel(session, inputId = mytabsetName)
    }
  }
  # ############################################################################
  
  
  
  # ############################################################################
  #' Modifiziert Header nach aktueller Sprache und Filterungen
  #'
  #' @param df Dataframe
  #' @param lang aktuelle Sprache
  #' @param byColId Filter by Id-Spalte
  #' @param filterId Id-Wert
  #' @param byColDate Filter by Date-Spalte
  #' @param filterDate Date-Wert
  #'
  #' @return modifizierter Dataframe
  #'
  modifyDF <- function(df, lang, byColId, filterId, byColDate, filterDate) {
    if (is.null(df)) {
      return(NULL)
    } else {
      # Sprachvariabler Header
      colnames(df)[match("Time",
                         colnames(df))] <- translate("Time", lang)
      colnames(df)[match("HR",
                         colnames(df))] <- translate("HeartRateBpm", lang)
      colnames(df)[match("lat",
                         colnames(df))] <- translate("LatitudeDegrees", lang)
      colnames(df)[match("lon",
                         colnames(df))] <- translate("LongitudeDegrees", lang)
      colnames(df)[match("alt",
                         colnames(df))] <- translate("AltitudeMeters", lang)
      colnames(df)[match("cumDist",
                         colnames(df))] <- translate("DistanceMeters", lang)
      colnames(df)[match("speed",
                         colnames(df))] <- translate("Speed", lang)

      updateCheckboxInput(session, "filterById", translate("filterId", lang))
      if (byColId) {
        df <- df[which(df$Label==filterId),]
      }
      colnames(df)[match("Label", colnames(df))] <- translate("Label", lang)
      
      updateCheckboxInput(session, "filterByDate",
                          translate("filterDate", lang))
      
      if (byColDate) {
        df <- df[which(df$Date==filterDate),]
      }
      colnames(df)[match("Date", colnames(df))] <- translate("Date", lang)
      
      return(df)
    }
  }
  # ############################################################################

  
  
# ############################################################################ #
#                                                                              #
#    Erzeuge Reactive values & conductors                                      #
#                                                                              #
# ############################################################################ #

  values <- reactiveValues(coroRawData = NULL)
  
  coroData <- reactive({
    modifyDF(values$coroRawData, input$language, input$filterById,
             input$filterByIdSelect, input$filterByDate,
             input$filterByDateSelect)
  })
  
  coroDataPlot <- reactive({
    assignGroups(coroData(), input$hfBer[1], input$hfBer[2], input$cpUnder,
                 input$cpRight, input$cpAbove,
                 input$language)
  })
  
  coroDataSummary <- reactive({
    calculateSummary(values$coroRawData, input$hfBer[1], input$hfBer[2])
  })


  
# ############################################################################ #
#                                                                              #
#    Verbinde Reactive sources mit Reactive endpoints                          #
#                                                                              #
# ############################################################################ #
  
  # --- Anpassung bei Sprachwechsel --------------------------------------------
  output$flag <- renderUI({
    tags$img(src = paste0("data:image/png;base64,",
                          translate("flag", input$language)),
             id = "languageflag")
  })
  
  # --- headerPanel ------------------------------------------------------------
  output$hilfe_t <- renderText({translate("Hilfe", input$language)})
  output$impressum_t <- renderText({translate("Impressum", input$language)})
  output$pers0_t <- renderText({translate("PersonenDaten2", input$language)})
  output$pers1_t <- renderText({translate("PersonenDaten2", input$language)})
  
  # --- titlePanel -------------------------------------------------------------
  output$title_t <- renderText({translate("CoroVisTitle", input$language)})
  
  # --- workingPanel > patientPanel --------------------------------------------
  output$name_t <- renderText({translate("Name", input$language)})
  output$vorname_t <- renderText({translate("Vorname", input$language)})
  output$gebdat_t <- renderText({translate("Geburtsdatum", input$language)})
  output$gebdat_f <- renderUI({translate("dd.mm.yyyy", input$language)})
  output$gebdat_l <- renderUI({translate("de", input$language)})
  dateReRenderer <- function(lang) {
    output$gebdat <- renderUI({dateInput("inpAlter",
                                         value = isolate(input$inpAlter),
                                         label = NULL,
                                         format = translate("dd.mm.yyyy", lang),
                                         language = translate("de", lang))
    })
  }
  output$gesch_t <- renderText({translate("Geschlecht", input$language)})
  output$groesse_t <- renderText({translate("Groesse", input$language)})
  output$gewicht_t <- renderText({translate("Gewicht", input$language)})
  
  # --- workingPanel > sidebarPanel --------------------------------------------
  output$fileTitle <- renderText({translate("filesHeader", input$language)})
  output$fileLabel <- renderText({translate("filesButton", input$language)})
  output$fileCounter <- renderText({
    if (is.null(input$userfiles)) {
      return (translate("filesLoaded0", input$language))
    } else if (nrow(input$userfiles)>1) {
      return(paste(nrow(input$userfiles),
                   translate("filesLoaded2", input$language)))
    } else {
      return (translate("filesLoaded1", input$language))
    }
  })
  output$filterTitle <- renderText({translate("filterTitle", input$language)})
  output$riskfactorTitle <- renderText({translate("riskfactorTitle",
                                                  input$language)})
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
  output$hfMax_t <- renderText({translate("Maximale Herzfrequenz",
                                          input$language)})
  output$frequenzbereich_t <- renderText({translate("Frequenzbereich",
                                                    input$language)})
  
  # --- workingPanel > mainPanel > tp0 HOME ------------------------------------
  output$start <- renderText({translate("Start", input$language)})
  output$startTitle <- renderText({translate("greeting", input$language)})
  output$startSubtitle <- renderText({translate("greetingSubtitle",
                                                input$language)})
  
  # --- workingPanel > mainPanel > tp1 TABLE -----------------------------------
  output$daten_t <- renderText({translate("Daten", input$language)})
  tableReRenderer <- function(languageStyle) {
    output$coroTable <- renderDataTable({
      lang <- input$language
      cols <- c(translate("Label", lang), translate("Date", lang),
                translate("Time", lang), translate("HeartRateBpm", lang),
                "GPS", translate("AltitudeMeters", lang),
                translate("DistanceMeters", lang))
      coroData()[cols]
    }, options = list(
      lengthMenu = c(10, 25, 100),
      pageLength = 10,
      language = languageStyle
    ))
  }
  
  # --- workingPanel > mainPanel > tp2 PLOT ------------------------------------
  output$zeit_t <- renderText({translate("Plot", input$language)})
  output$axisXSelectLabel <- renderText({translate("XAchse", input$language)})
  output$axisYSelectLabel <- renderText({translate("YAchse", input$language)})
  output$explorationPlot <- renderPlot({
    if (is.null(coroDataPlot())) {return(NULL)}
    x <- times(coroDataPlot()[,input$axisXSelect])
    y <- as.numeric(coroDataPlot()[,input$axisYSelect])
    z <- coroDataPlot()[,c("Group")]
    ylim <- ifelse("1" %in% input$plotInclude0 && 0<min(y, na.rm = TRUE), 0,
                   min(y, na.rm = TRUE))
    ylim <- c(ylim, max(y, na.rm = TRUE))

    par(mar = c(5, 5, 2, 2))
    plot(x, y, pch = 16, col=z, main = NULL, xlab = input$axisXSelect,
         ylab = input$axisYSelect, ylim = ylim,
         cex = input$plotPointsize, las = 1,
         cex.lab = input$plotTextsize, cex.main = input$plotTextsize)
    legend("top", legend=c(translate("UnterRef", input$language),
                               translate("ImRef", input$language),
                               translate("ÜberRef", input$language)),
           col=c(input$cpUnder, input$cpRight, input$cpAbove), pch = 16,
           xpd = TRUE, horiz = TRUE, bty = 'n', cex = input$plotTextsize,
           inset = c(0, -0.1))
    if ("2" %in% input$plotInclude0) {lines(x, y, pch=16)}
  })
  
  # --- workingPanel > mainPanel > tp3 MAP -------------------------------------
  output$karte_t <- renderText({translate("Karte", input$language)})
  output$tOut1 <- renderLeaflet({
    if (is.null(coroDataPlot())) {
      m <- leaflet() %>% addTiles() 
    } else {
      cols <- colnames(coroDataPlot())
      i <- match(translate("LatitudeDegrees", input$language), cols)
      myLat <- coroDataPlot()[coroDataPlot()$GPS=="+",i]
      i <- match(translate("LongitudeDegrees", input$language), cols)
      myLng <- coroDataPlot()[coroDataPlot()$GPS=="+",i]
      i <- match(translate("HeartRateBpm", input$language), cols)
      myHR <- coroDataPlot()[coroDataPlot()$GPS=="+",i]
      m <- leaflet() %>% addProviderTiles(input$mapTilesSelect) %>%
        addCircles(data = coroDataPlot(),
                   lat = ~myLat, lng = ~myLng, popup= ~myHR, radius=5,
                   color= ~Group, stroke = TRUE, fillOpacity = 1)
    }
  })
  
  # --- workingPanel > mainPanel > tp4 SUMMARY ---------------------------------
  output$gesamt_t <- renderText({translate("Zusammenfassung", input$language)})
  output$summaryTitle <- renderText({translate("Statistik", input$language)})
  output$dlReport <- renderText({translate("Report", input$language)})
  output$chPlotSize <- renderText({translate("PlotSize", input$language)})
  output$tOut2 <- renderTable({
    if (is.null(coroDataSummary())) {
      return(NULL)
    } else {
      mydf <- coroDataSummary()
      colnames(mydf) <- translate(c("Date", "UnterRef", "ImRef", "ÜberRef"),
                                  input$language)
      return(mydf)
    }
  }, digits = 0)
  plotReRenderer <- function(hsize) {
    output$summaryPlot <- renderPlot({
      counts <- t(as.matrix(coroDataSummary()[2:4]))
      counts <- counts[,ncol(counts):1]
      par(mar = c(5, 12, 0.2, 2))
      barplot(counts, horiz = TRUE, names.arg = rev(coroDataSummary()$Date),
              las=1, col = c(input$cpUnder, input$cpRight, input$cpAbove))
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
      createPDF(myPatient, myParameter, values$coroRawData, file,
                input$language)
    }
  )

  # --- workingPanel > mainPanel > tp5 SETTINGS --------------------------------
  output$settings <- renderText({translate("Einstellungen", input$language)})
  output$settingsColorTitle <- renderText({translate("Farbwerte",
                                                     input$language)})
  output$settingsColor1Label <- renderText({translate("UnterRef",
                                                      input$language)})
  output$settingsColor2Label <- renderText({translate("ImRef", input$language)})
  output$settingsColor3Label <- renderText({translate("ÜberRef",
                                                      input$language)})
  output$settingsColorResetLabel <- renderText({translate("FarbenZurück",
                                                          input$language)})
  output$settingsHFmaxTitle <- renderText({translate("maximaleHF",
                                                     input$language)})
  output$SelectMapLayer <- renderText({ translate("SelectMapLayer",
                                                  input$language)})
  output$timeShiftCSV <- renderText({ translate("ZeitVCSV", input$language)})
  output$spotSize <- renderText({ translate("PunktGr", input$language)})#
  output$textSize <- renderText({ translate("TextGr", input$language)})
  
  # --- helpPanel --------------------------------------------------------------
  output$helpTitle <- renderText({translate("Hilfe", input$language)})
  output$helpHTML <- renderUI({getPage(translate("IncHilfe", input$language))})
  output$helpBackLabel <- renderText({translate("Zur?ck", input$language)})
  
  # --- imprintPanel -----------------------------------------------------------
  output$imprintTitle <- renderText({translate("Impressum", input$language)})
  output$imprintHTML <- renderUI({getPage(translate("IncImpressum", 
                                                    input$language))})
  output$imprintBackLabel <- renderText({translate("Zur?ck", input$language)})
  

  
# ############################################################################ #
#                                                                              #
#    Definiere Javascript-Events                                               #
#                                                                              #
# ############################################################################ # 

  # --- Sprachwechsel bei Klick auf Flagge -------------------------------------
  shinyjs::onclick("languageflag", {
    updateSliderInput(session, inputId = "language",
                      value = (input$language %% numDics) + 1)
  })
  
  # --- Einblenden der Hilfe ---------------------------------------------------
  shinyjs::onclick("hil", {
    updateSelectInput(session, inputId = "currentPanel", selected = "helpPanel")
  })
  
  # --- Einblenden des Impressums ----------------------------------------------
  shinyjs::onclick("imp", {
    updateSelectInput(session, inputId = "currentPanel",
                      selected = "imprintPanel")
  })
  
  # --- Ein-/Ausblenden des patientPanels --------------------------------------
  shinyjs::onclick("decrease", updateCheckboxInput(session, "iPanelOn",
                                                   value = !input$iPanelOn))
  shinyjs::onclick("increase", updateCheckboxInput(session, "iPanelOn",
                                                   value = !input$iPanelOn))
  


# ############################################################################ #
#                                                                              #
#    Registriere Observer-Ereignisse                                           #
#                                                                              #
# ############################################################################ #
  
  # --- Hilfe -> Zurueck -------------------------------------------------------
  observeEvent(input$helpBack, {
    updateSelectInput(session, inputId = "currentPanel",
                      selected = "workingPanel")
  })
  
  # --- Impressum -< Zurueck ---------------------------------------------------
  observeEvent(input$imprintBack, {
    updateSelectInput(session, inputId = "currentPanel",
                      selected = "workingPanel")
  })
  
  # --- Sprachwechsel ----------------------------------------------------------
  observeEvent(input$language, {
    lang <- input$language
    languageList <- list("paginate" = list("next" = translate("next", lang),
                                      "previous" = translate("previous", lang)),
                    "search" = translate("search", lang),
                    "lengthMenu" = translate("lengthMenu", lang),
                    "loadingRecords" = translate("loadingRecords", lang),
                    "processing" = translate("processing", lang),
                    "info" = translate("info", lang),
                    "infoEmpty" = translate("infoEmpty", lang),
                    "zeroRecords" = translate("zeroRecords", lang),
                    "emptyTable" = translate("emptyTable", lang),
                    "infoFiltered" = translate("infoFiltered", lang))
    tableReRenderer(languageList)
    dateReRenderer(lang)
    updateRadioButtons(session, "inpGesch", choiceValues = list("m", "f"),
                       inline = TRUE, selected = input$inpGesch,
        choiceNames = list(translate("maennlich", lang),
                           translate("weiblich", lang)))
    
    myChoices <- c("Time", "DistanceMeters")
    newSelection <- getNewSelection(input$axisXSelect, lang, myChoices)
    newChoices <- c(translate(myChoices, lang))
    updateSelectInput(session, "axisXSelect", choices = newChoices,
                      selected = newSelection)
    
    myChoices <- c("HeartRateBpm", "AltitudeMeters", "DistanceMeters", "Speed")
    newSelection <- getNewSelection(input$axisYSelect, lang, myChoices)
    newChoices <- c(translate(myChoices, lang))
    updateSelectInput(session, "axisYSelect", choices = newChoices,
                      selected = newSelection)
    
    newChoices <- list(1,2,3,4,5,6)
    names(newChoices) <- translate(c("level1", "level2", "level3", "level4",
                                     "level5", "level6"), lang)
    updateSelectInput(session, "intensity", choices = newChoices,
                      selected = input$intensity)
  })
  
  # --- Datei-Upload -----------------------------------------------------------
  observeEvent(input$userfiles, {
    updateCheckboxInput(session, "dataAvailable", value = FALSE)
    values$coroRawData <- importFiles(input$userfiles)
    
    # entferne bei neu geladenen Daten immer alle globalen Filter
    updateCheckboxInput(session, "filterById", value = FALSE)
    updateCheckboxInput(session, "filterByDate", value = FALSE)
    updateSelectInput(session, "filterByIdSelect",
                      choices = unique(values$coroRawData[,match("Label",
                                       colnames(values$coroRawData))]))
    updateSelectInput(session, "filterByDateSelect",
                      choices = unique(values$coroRawData[,match("Date",
                                       colnames(values$coroRawData))]))
    if (!is.null(values$coroRawData)) {
      updateCheckboxInput(session, "dataAvailable", value = TRUE)
      if (nrow(values$coroRawData[values$coroRawData$GPS=="+",])>0) {
        updateCheckboxInput(session, "mapAvailable", value = TRUE)
      } else {
        updateCheckboxInput(session, "mapAvailable", value = FALSE)
      }
      myText <- paste(input$nachname, input$vorname, sep = ", ")
      myText <- gsub("^, ", "", myText)
      myText <- gsub(", $", "", myText)
      if (nchar(myText)>0) {
        myText <- paste0(translate("PersonenDaten3", input$language),
                         myText, "?")
      } else {
        myText <- translate("PersonenDaten", input$language)
      }
      showModal(modalDialog(title = translate("question",
                                              isolate(input$language)),
                            myText,
                            footer = tagList(
                              modalButton(translate("yes",
                                                    isolate(input$language))),
                              actionButton("patientNew",
                                           translate("no",
                                                     isolate(input$language)))
                            )
      ))
    } else {
      updateCheckboxInput(session, "dataAvailable", value = FALSE)
      updateCheckboxInput(session, "mapAvailable", value = FALSE)
    }
  })
  
  # --- Aenderung dataAvailable ------------------------------------------------
  observeEvent(input$dataAvailable, {
    if (is.null(values$coroRawData)) {
          for (i in c(1:2, 4)) {
            hideTab(mytabsetName = "tP", child = i, "tP0", status = 0)
          }
        } else {
          for (i in c(1:2, 4)) {
            hideTab(mytabsetName = "tP", child = i, "tP1", status = 1)
          }
        }
  })
  
  # --- Aenderung mapAvailable -------------------------------------------------
  observeEvent(input$mapAvailable, {
    if (input$mapAvailable) {
      hideTab(mytabsetName = "tP", child = 3, status = 1)
    } else {
      if (input$tP == "tP3") {
        if (input$dataAvailable) {
          newTab <- "tP1"
        } else {
          newTab <- "tP0"
        }
      } else {
        newTab <- NULL
      }
      hideTab(mytabsetName = "tP", child = 3, newTab, status = 0)
    }
  })
  
  # --- Aenderung Herzfrequenzgrenzen ------------------------------------------
  observeEvent(input$hfMax, {
    updateSliderInput(session, "hfBer", min = input$hfMax[1],
                      max = input$hfMax[2],
                      value = getRange(input$hfMax[1], input$hfMax[2],
                                       input$intensity))
  })
  
  # --- Aenderung Trainingslevel -----------------------------------------------
  observeEvent(input$intensity, {
    updateSliderInput(session, "hfBer",
                      value = getRange(input$hfMax[1], input$hfMax[2],
                                       input$intensity))
  })
  
  # --- Aenderung maximal moegliche Herzfrequenzgrenze -------------------------
  observeEvent(input$overrideMaxHF, {
    updateSliderInput(session, "hfMax", value = input$hfMax,
                      max = input$overrideMaxHF)
  })

  # --- Hinweis: neuer Patient -------------------------------------------------
  observeEvent(input$patientNew, {
    updateTextInput(session, "nachname", value = "")
    updateTextInput(session, "vorname", value = "")
    updateCheckboxInput(session, "inpAlterKA", value = FALSE)
    updateCheckboxInput(session, "geschKA", value = FALSE)
    updateCheckboxInput(session, "groesseKA", value = FALSE)
    updateCheckboxInput(session, "gewichtKA", value = FALSE)
    removeModal()
  })

  # --- Farbschema zurueckgesetzt ----------------------------------------------
  observeEvent(input$resetColors, {
    # Farben zurücksetzen
    updateColourInput(session, "cpUnder", value = basicCol[1])
    updateColourInput(session, "cpRight", value = basicCol[2])
    updateColourInput(session, "cpAbove", value = basicCol[3])
  })

  # --- Plotgroesse Summary anpassen -------------------------------------------
  observeEvent(input$summaryPlotExpander, {
    plotReRenderer(input$summaryPlotExpander*400)
  })
})

# *** ENDE ***
