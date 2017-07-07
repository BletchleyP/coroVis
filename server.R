library(shiny)    # Shiny selbst...
library(chron)    # Handling von Zeit und Datum...
library(XML)      # Einlesen von XML-Dateien ...
library(leaflet)  # Darstellung der Karten ... 
library(ggplot2)  # Darstellung der Plots ...

# ---------------------------
# Globale Variablendefinition
# ---------------------------

# Fuer die Devices
deviceList <- list("Polar M200 (CSV, GPX)" = "POLARM200", "Garmin/Allgemein (TCX)" = "GARMIN")

# Fuer die Berechnungen
einJahr <- 365
hfBereiche <- list("minimal" = 0.34,"leicht" = 0.54, "moderat" = 0.69, "schwer" = 0.89, "sehr schwer" = 0.97, "maximal" = 1.0)

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
  
  # --------------------------------
  # Max upload Filegroesse bis 15 MB
  # --------------------------------
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
      mask <- as.character(translate("maennlich"))
      fem <- as.character(translate("weiblich"))
      tagList(
        dateInput("inpAlter", value = currentBirthDate, label = translate("Geburtsdatum"), format = translate("dd.mm.yyyy"), language = translate("de"))
      )
    })
  }
  
  # Fuer das Alter
  renderAlter <- function(){
    output$alter <- renderUI({
      tagList(
        h5(translate("Alter")),
        verbatimTextOutput("alterausgabe")
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
  
  # Fuer den BMI
  renderBMI <- function() {
    output$bmiUI <- renderUI({
      tagList(
        h5("BMI"),
        verbatimTextOutput("bmi")
      )      
    })
  }
  
  # Fuer den Device-Select und den FileInput
  renderFI <- function() {
    output$fInput <- renderUI({
      tagList(
        selectInput("devSel", label = translate("Device waehlen"), choices = deviceList),
        fileInput("patDat", label = textOutput("patDat_t"), multiple = TRUE, buttonLabel = translate("Los"), placeholder = translate("Keine Datei gewählt"))
      )
    })
  }
  
  # Fuer die Trainingsdatenliste
  renderDataSelect <- function(choiceList = NULL) {
    output$datSelect <- renderUI({
      tagList(
        selectInput("inpData", label = translate("Trainingsdaten analysieren ..."), choices = choiceList)
      )
    })
  }
  
  renderTimeline <- function() {
    output$zeitraumSelect <- renderUI({
      tagList(
        dateRangeInput("inpDateRange", label = translate("Zeitraum analysieren"), 
                                       format = translate("dd.mm.yyyy"), 
                                       language = translate("de"), 
                                       separator = translate("bis"),
                                       start = "0000-00-00")
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
  
  # Fuer das Landesfaehnchen...
  renderFlag <- function() {
    output$flag <- renderUI({
      tags$img(src = paste0("data:image/png;base64,",translate("flag")), id = "languageflag")
    })
  }
  

  
  
  
  
  
  # Fuer Auswahl der Achsen an den Graphen
  renderSelAxis <- function() {
    output$selAxOut <- renderUI({
      tagList(
        inputPanel(
          selectInput("selAxisId", label = textOutput("selAxis"), choice = list(translate("Weg"), translate("Hoehe"), translate("Geschwindigkeit"))),
          selectInput("selAxisXId", label = textOutput("selAxisX"), choice = list(translate("Weg"), translate("Zeit"))),
          radioButtons("hrOn", label = translate("Herzfrequenz"), choiceNames = list(translate('An'), translate('Aus')), choiceValues = list(TRUE, FALSE), inline = TRUE, selected = "FALSE")
        )
      )
    })
  }
  
  renderSettings <- function() {
    output$settingsOut <- renderUI({
      tagList(
        h3(translate("Einstellungen"))
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
  
  # Abfrage ob die eingelesenen Daten immer noch zum selben Patienten gehoeren ...
  askForPerson <- function() {
    m = modalDialog(
      h2(translate("PersonenDaten")),
      title = translate("Bitte beachten"),
      footer = tagList(modalButton("Ja"), actionButton("samePerson", "NEIN"))
    )
    showModal(m)    
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
    
    # Setzen des Faehnchens...    
    renderFlag()
    
    # Input-Panel
    renderGebDat()
    renderGesch()
        
    # Sidebar
    output$patDat_t <- renderText({ paste(translate("Trainingsdaten suchen ...")) })
    renderFI()
    renderTimeline()
    renderAlter()
    output$bmiTitle <- renderText({ paste("BMI") })
    renderBMI()
    renderRiskClass()
    renderStrIntensity()
    output$risiko_t <- renderText({ paste(translate("Belastungsintensitaet")) })
    
    # mainPanel
    output$selAxisX <- renderText({ paste(translate("XAchse"))})
    output$selAxis <- renderText({ paste(translate("YAchse")) })
    
    renderSelAxis()
    renderSettings()
    
    # Wieder Einstellen von bestimmten ausgewaehlten Parametern nach dem Sprachwechsel...
    updateDateInput(session, "inpAlter", value = currentBirthDate)        # Alter wieder einstellen
    updateRadioButtons(session, "inpGesch", selected = currentSex)        # Geschlecht wieder einstellen
    renderDataSelect(unitList)                                # Auswahldaten (wieder) einstellen
    renderDataPlot()                                          # Datenplot neu beschriften
    renderMapPlot()                                           # Kartenplot ausfuehren
    renderSelAxis()                                           # Achsenauswahl neu beschriften

  }
  
  # -----------------
  # Die Daten plotten
  # -----------------
  
  # Datentabelle im TabPanel darstellen
  visualizeDataTable<- function(data) {
    alleDaten <- grep(" # Alle Daten", data)
    if (length(alleDaten) > 0) {
      choice <- gsub(" # Alle Daten", "", data)
      # ??? output$tabOut <- renderDataTable(exerciseData[[choice]], options = list(pageLength = 10))
      updateTabsetPanel(session, "tP", selected = "tP1")
    } else {
      # ??? output$tabOut <- renderDataTable(NULL, options = list(pageLength = 10))
    }
  }
  
  # Graphisch im TabPanel darstellen
  renderDataPlot <- function(xAx = "Weg", yAx = "Weg") {
    
    # X-Achse auswaehlen
    switch(xAx,
           Weg = { xAxis <- viewportDF$DistanceMeters },
           Zeit = { xAxis <- viewportDF$Time }
    )
    
    # Y-Achse auswaehlen
    switch(yAx,
           Weg = { yAxis <- viewportDF$DistanceMeters },
           Hoehe = { yAxis <- viewportDF$AltitudeMeters },
           
           # Geschwindigkeit berechnen...
           Geschwindigkeit = {
             dWeg <- array()
             for(i in 2:length(viewportDF$DistanceMeters)) {
              dT <- times(viewportDF$Time[i]) - times(viewportDF$Time[i-1])
              dT <- as.integer(substr(dT, 7, 8)) + as.integer(substr(dT, 4, 5)) * 60 + as.integer(substr(dT, 1, 2)) * 3600
              dWeg[i] <- ((viewportDF$DistanceMeters[i] - viewportDF$DistanceMeters[i-1]) / dT) * 3.6 
             }
             dWeg[1] = 0
             yAxis = dWeg
           }
    )
    
    # ... nur wenn x- und y-Achse Werte aufweisen, dann...
    if (!is.null(xAxis) && !is.null(yAxis)) {
    
      # ... die Daten selbst im Plot darstellen
      output$explorationPlot <- renderPlot({
      
        x <- xAxis
        yb <- viewportDF$HeartRateBpm < rVal$hfBu 
        yc <- viewportDF$HeartRateBpm > rVal$hfBo
      
        y1 <- yAxis
        plot(x, y1, main = translate("Optimale Belastung"), xlab = translate(xAx), ylab = translate(yAx))
        points(x, y1, col = ifelse(yb, "orange", ifelse(yc, "red", "green")))
      
      })
    
      # ... und den tolerierten Herzfrequenzbereich darstellen (wird nur angezeigt, wenn vom Benutzer gewuenscht)
      output$explorationPlotHR <- renderPlot({
      
        y2 <- viewportDF$HeartRateBpm
        x <- viewportDF$Time
      
        plot(x, y2, type = "p", col = "blue", main = NULL, xlab = translate("Zeit"), ylab = translate("HFq"), ylim = c(40, rVal$maxFr))
        rect(0, rVal$hfBu, x, rVal$hfBo, border = NULL, col = rgb(0,0,1,.005))
      
      })
    
    # Andernfalls eine kleine Meldung
    } else {
      output$selAxOut <- renderText({ paste("Keine Daten!") })
    }
  }
  
  # Karte im TabPanel darstellen
  renderMapPlot <- function() {
    
    output$tOut1 <- renderLeaflet({
      
      # Sind ueberhaupt GPS-Daten geladen?
      gpsAvailable <- grep("LatitudeDegrees", names(viewportDF))
      if (length(gpsAvailable) > 0) {
        
        # Dataframe zusammenstellen...
        testdata <- viewportDF[,c("Time", "LatitudeDegrees", "LongitudeDegrees")]
        colnames(testdata) <- c("lat", "lng", "hr")
        testdata$lat <- as.numeric(as.character(viewportDF$LatitudeDegrees))
        testdata$lng <- as.numeric(as.character(viewportDF$LongitudeDegrees))
        testdata$hr <- as.numeric(as.character(viewportDF$HeartRateBpm))
        testdata$colorcode <- ifelse(testdata$hr<rVal$hfBu, "#FFCC00", ifelse(testdata$hr>rVal$hfBo, "#FF0000", "#00FF00"))
        testdata$hr <- as.factor(testdata$hr)
    
        # Karte generieren...
        m <- leaflet() %>% addTiles() %>% addMarkers(lng = testdata$lng[1],  lat = testdata$lat[1], popup = "Start") %>%
              addProviderTiles(providers$CartoDB.Positron) %>% addCircles(data = testdata, lat = ~lat, lng = ~lng, popup= ~hr, radius=5, color= ~colorcode, stroke = TRUE, fillOpacity = 1)
        m
      
      # ... ansonsten leere Karte generieren  
      } else {
        m <- leaflet() %>% addTiles() 
        m
      }
    })
  }
  
  # ------------------------------------------------------
  # Funktionen fuer die Aufbereitung der Daten zur Ausgabe
  # ------------------------------------------------------
  
  # Die Funktion entscheidet anhand der eingelesenen Daten, von welcher Watch die Daten stammen
  # danach kann entschieden werden, welche Funktion benutzt werden soll, um die Daten entsprechend aufzubereiten
  decideWhichWatch <- function() {
    watch <- toupper(input$devSel)
    return(watch)
  }
  
  
  
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
      renderDataSelect(unitList)
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
      renderDataSelect(unitList)
    }
  }

  # --------------------------------------------------
  # Beim ersten Durchlauf Beschriftungen einstellen...
  # --------------------------------------------------
  
  # UI beschriften
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
    
    # Ist eine Berechnung des Alters erforderlich ...
    output$alterausgabe <- renderText({ 
      paste( round( difftime(Sys.time(), input$inpAlter)/einJahr , 1) )
    })
    
    # Ist der BMI-Rechner betaetigt ...
    output$bmi <- renderText({ paste(round(input$gewicht/((input$groesse/100)^2), digits = 1)) })
    
  })
  
  # ----------------------
  # Die Events beachten...
  # ----------------------
  
  # # Wenn Trainingsdaten geladen werden
  # observeEvent(input$patDat, {
  #   
  #   #Gehoeren die eingelesenen Daten immer noch zur selben Person?
  #   if (!is.null(exerciseData))
  #     askForPerson()
  #   
  #   # Dateiinfos der geladenen Dateien erhalten
  #   datInfo <- input$patDat
  #   
  #   # Von welcher Watch sind die Daten geladen
  #   switch(decideWhichWatch(),
  # 
  #     # Dateien der Trainingseinheiten nach Standard Polar M 200 (CSV, GPX, TCX) zusammenstellen
  #     POLARM200 = {
  #       renderDataSelect(NULL)
  #       lookForExerciseUnitPolarM200(datInfo)
  #       #renderTabSetPanel() - noch notwendig
  #       renderDataPlot()
  #       renderSelAxis()
  #       renderMapPlot()
  #     },
  #     
  #     # Dateien der Trainingseinheiten nach Standard GARMIN (TCX) zusammenstellen
  #     GARMIN = {
  #       renderDataSelect(NULL)
  #       lookForExerciseUnitGarmin(datInfo)
  #       #renderTabSetPanel() - noch notwendig
  #       renderDataPlot()
  #       renderSelAxis()
  #       renderMapPlot()
  #     }
  #     
  #   )
  # 
  #   visualizeDataTable(input$Data)
  # })
  
  # Wird eine Trainingseinheit ausgewaehlt, dann ....
  observeEvent(input$inpData, {
    visualizeDataTable(input$inpData)
  })
  
  # Wenn die Risikoklasse, die Belastungsintensität oder die Maximalherzfrequenz eingestellt wird

  observeEvent(input$inpRiskClass, {
    rVal$maxFr <- as.numeric(input$inpRiskClass)
    rVal$hfMaxOut <- 40 + (rVal$maxFr-40) * hfBereiche[[ifelse(is.null(input$risk), "minimal", rev_translate(input$risk))]]
    rVal$hfBu <- rVal$hfMaxOut - 5
    rVal$hfBo <- rVal$hfMaxOut + 5
    updateSliderInput(session, "hfMax", max = rVal$maxFr, value = rVal$hfMaxOut)
    updateSliderInput(session, "hfBer", max = rVal$hfMaxOut+10, value = c(rVal$hfBu, rVal$hfBo))
  })
  
  observeEvent(input$risk, {
    rVal$maxFr <- as.numeric(input$inpRiskClass)
    rVal$hfMaxOut <- 40 + (rVal$maxFr-40) * hfBereiche[[ifelse(is.null(input$risk), "minimal", rev_translate(input$risk))]]
    rVal$hfBu <- rVal$hfMaxOut - 5
    rVal$hfBo <- rVal$hfMaxOut + 5
    updateSliderInput(session, "hfMax", max = rVal$maxFr, value = rVal$hfMaxOut)
    updateSliderInput(session, "hfBer", max = rVal$hfMaxOut+10, value = c(rVal$hfBu, rVal$hfBo))
  })
  
  observeEvent(input$hfMax, {
    rVal$maxFr <- as.numeric(input$inpRiskClass)
    if (input$hfMax != 40)
      rVal$hfMaxOut <- input$hfMax
    else
      rVal$hfMaxOut <- 40 + (rVal$maxFr-40) * hfBereiche[[ifelse(is.null(input$risk), "minimal", rev_translate(input$risk))]]
    rVal$hfBu <- rVal$hfMaxOut - 5
    rVal$hfBo <- rVal$hfMaxOut + 5
    updateSliderInput(session, "hfMax", max = rVal$maxFr, value = rVal$hfMaxOut)
    updateSliderInput(session, "hfBer", max = rVal$hfMaxOut+10, value = c(rVal$hfBu, rVal$hfBo))
  })
  
  observeEvent(input$hfBer, {
    rVal$hfBu <- input$hfBer[1]
    rVal$hfBo <- input$hfBer[2]
  })
  
  observeEvent(input$samePerson, {
    updateTextInput(session, "nachname", value = "")
    updateTextInput(session, "vorname", value =  "")
    updateSliderInput(session, "groesse", value = 170)
    updateSliderInput(session, "gewicht", value=70)
    renderGebDat()
    renderGesch()
    removeModal()
  })
  
  # Wenn die Achsen für die Graphen ausgewaehlt werden ...
  observeEvent(input$selAxisXId, {
    renderDataPlot(rev_translate(input$selAxisXId), rev_translate(input$selAxisId))
  })
  
  observeEvent(input$selAxisId, {
    renderDataPlot(rev_translate(input$selAxisXId), rev_translate(input$selAxisId))
  })
  
  # Wenn die Herzfrequenz-Darstellung an-/ausgeschaltet wird
  observeEvent(input$hrOn, {
    if (input$hrOn == "TRUE")
      updateCheckboxInput(session, "hrPlotOn", value = TRUE)
    if (input$hrOn == "FALSE")
      updateCheckboxInput(session, "hrPlotOn", value = FALSE)
  })
  
  # Wenn der Login-Button betaetigt wird, dann wieder auf die Arbeits-UI
  observeEvent(input$login, {
    updateCheckboxInput(session, "lgIn", value = TRUE)
  })
  
  
  

  
  output$tabOut <- renderDataTable(values$reactiveDF, options = list(
    lengthMenu = c(10, 25, 100),
    pageLength = 10
  ))
  
# ########################################################################################################################
#
#    Definiere Funktionen mit Shiny-Interaktionen
# 
# ########################################################################################################################

# ------------------------------------------------------------------------------------------------------------------------
  
  # Die Funktion importiert Trainingsdaten aus einer TCX-Datei und liefert Dataframe
  importDataTCX <- function(tcxFile) {

    doc <- xmlParse(tcxFile)
    # ??? labs <- NULL
    
    # ermittle Gesamtanzahl <Trackpoint>-tags
    totalSize <- xmlSize(getNodeSet(doc, "//ns:Trackpoint", "ns"))
    if (totalSize>0) {
      if (file.exists("temp.csv")) {file.remove("temp.csv")}
      catTemp <- file("temp.csv", open = "a")
      myHeader <- c("Filename", "Id", "Time", "HeartRateBpm", "LatitudeDegrees", "LongitudeDegrees", "AltitudeMeters",
                    "DistanceMeters")
      cat(paste0(paste(myHeader, collapse = ","), "\n"), file=catTemp)
      
      # ermittle Anzahl <Activity>-tags
      nodes <- getNodeSet(doc, "//ns:Activities", "ns")
      size <- xmlSize(nodes[[1]])
      counter <- 0
      
      withProgress(message = translate("TCX-Import"), value = 0, {
        
        # foreach <Activity>-tag
        for (i in 1:size) {
          startNode <- nodes[[1]][[i]]
          label <- c(Id = xmlValue(startNode[["Id"]]))
          # ??? labs <- c(labs, label)
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
              v <- c("filename.txt", label, v)
              cat(paste0(paste(v[myHeader], collapse = ","), "\n"), file=catTemp)
            }
          }
          
          # aktualisiere Fortschrittsanzeige
          incProgress(i/size)
        }
      })
      
      # beende den Datenstrom und importiere die Daten aus catTemp
      close(catTemp)
      df_trackpoints <- read.csv("temp.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
      # ??? df_trackpoints$Filename <- as.factor(df_trackpoints$Filename)
      # ??? df_trackpoints$Id <- as.factor(df_trackpoints$Id)
      # ??? df_trackpoints$LatitudeDegrees <- as.factor(df_trackpoints$LatitudeDegrees)
      # ??? df_trackpoints$LongitudeDegrees <- as.factor(df_trackpoints$LongitudeDegrees)
      # ??? df_trackpoints$HeartRateBpm <- as.integer(df_trackpoints$HeartRateBpm)
      # ??? df_trackpoints$AltitudeMeters <- as.integer(df_trackpoints$AltitudeMeters)
      # ??? df_trackpoints$DistanceMeters <- as.numeric(df_trackpoints$DistanceMeters)
      # ??? df_trackpoints$Time <- times(substr(df_trackpoints$Time, 12, 19))
      # ??? df_trackpoints[is.na(df_trackpoints)] <- 0
      
    } else {
      df_trackpoints <- NULL
    }
    
    if (file.exists("temp.csv")) {file.remove("temp.csv")}
    # ??? ret_list <- list(labs, df_trackpoints)
    # ??? return(ret_list)
    return(df_trackpoints)
  }
  
# ------------------------------------------------------------------------------------------------------------------------
  
  # Funktion kontrolliert den Datenimport nach Upload
  importFiles <- function(n) {
    viewportNewDF <- NULL
    
    # durchlaufe alle zu importierenden Dateien
    for (i in 1:n) {
      currentFile <- input$patDat[i,]
      result <- checkFileformat(currentFile)
      
      if (substr(result, 1, 1) == "@") {
        # Fehlermeldung und weiter
        message(result)
        
      } else {
        # starte formatspezifische Konversion in Dataframe
        if (result == "TCX") {
          newDF <- importDataTCX(currentFile$datapath)
        } else if (result == "CSV") {
          newDF <- importDataCSV(currentFile$datapath)
        } else {
          newDF <- importDataGPX(currentFile$datapath)
        }
        
        if (is.null(newDF)) {
          # Fehlermeldung und weiter
          message(paste("Fehler beim Datenimport: is.null(dataframe) für", currentFile$name))
          
        } else {
          # merge neue Daten mit viewportNewDF
          if (is.null(viewportNewDF)) {
            viewportNewDF <- newDF
          } else {
            viewportNewDF <- mergeData(viewportNewDF, newDF)
          }
        }
      }
    }
    
    # publiziere neuen Gesamtdatensatz
    if (is.null(viewportNewDF)) {
      message("Sorry, Datenimport war nicht erfolgreich!")
    } else {
      values$reactiveDF <- viewportNewDF
      viewportDF <<- viewportNewDF
    }
  }
  
# --------------------------------------------------------------------------------------------------------------
  
# ##############################################################################################################
#
#    Erzeuge Reactive values & conductors
#
# ##############################################################################################################

# --------------------------------------------------------------------------------------------------------------

  values <- reactiveValues(reactiveDF = NULL)
  
# --------------------------------------------------------------------------------------------------------------
  
  
  
# ##############################################################################################################
#
#    Verbinde Reactive sources mit Reactive endpoints
#
# ##############################################################################################################
  
# --------------------------------------------------------------------------------------------------------------
  # Anpassung bei Sprachwechsel
  
  # Flag
  # TODO
  
  # headerPanel
  # TODO
  output$hilfe_t <- renderText({translate("Hilfe", input$currentLanguage)})
  output$impressum_t <- renderText({translate("Impressum", input$currentLanguage)})
  output$pers0_t <- renderText({translate("PersonenDaten2", input$currentLanguage)})
  output$pers1_t <- renderText({translate("PersonenDaten2", input$currentLanguage)})
  
  
  
  # titlePanel
  # TODO
  output$title_t <- renderText({ paste(translate("CoroVisTitle")) })
  
  # workingPanel > patientPanel
  # TODO
  output$name_t <- renderText({translate("Name", input$currentLanguage)})
  output$vorname_t <- renderText({translate("Vorname", input$currentLanguage)})
  output$groesse_t <- renderText({translate("Groesse", input$currentLanguage)})
  output$gewicht_t <- renderText({translate("Gewicht", input$currentLanguage)})
  
  # workingPanel > sidebarPanel
  # TODO
  output$hfMax_t <- renderText({translate("Maximale Herzfrequenz", input$currentLanguage)})
  output$frequenzbereich_t <- renderText({translate("Frequenzbereich", input$currentLanguage)})
  
  # workingPanel > mainPanel
  # TODO
  output$start <- renderText({translate("Start", input$currentLanguage)})
  output$startTitle <- renderText({translate("greeting", input$currentLanguage)})
  output$startSubtitle <- renderText({translate("greetingSubtitle", input$currentLanguage)})
  
  output$daten_t <- renderText({translate("Daten", input$currentLanguage)})

  output$zeit_t <- renderText({translate("Plot", input$currentLanguage)})
  output$karte_t <- renderText({translate("Karte", input$currentLanguage)})
  output$gesamt_t <- renderText({translate("Zusammenfassung", input$currentLanguage)})
  output$settings <- renderText({translate("Einstellungen", input$currentLanguage)})
  
  # helpPanel
  output$helpTitle <- renderText({translate("Hilfe", input$currentLanguage)})
  output$helpBackLabel <- renderText({translate("Zur?ck", input$currentLanguage)})
  
  # imprintPanel
  output$imprintTitle <- renderText({translate("Impressum", input$currentLanguage)})
  output$imprintHTML <- renderUI({getPage(translate("IncImpressum", input$currentLanguage))})
  output$imprintBackLabel <- renderText({translate("Zur?ck", input$currentLanguage)})
  
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
    updateSliderInput(session, inputId = "currentLanguage", value = (input$currentLanguage %% numDics) + 1)
  })
  
  # Einblenden der Hilfe
  shinyjs::onclick("hil", {
    updateSelectInput(session, inputId = "currentPanel", selected = "helpPanel")
  })
  
  # Einblenden des Impressums
  shinyjs::onclick("imp", {
    updateSelectInput(session, inputId = "currentPanel", selected = "imprintPanel")
  })
  
# --------------------------------------------------------------------------------------------------------------
  
  
  
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

  # Observer zu fileInput -> neue Importfunktion
  observeEvent(input$patDat, {
    importFiles(nrow(input$patDat))
  })
  
# --------------------------------------------------------------------------------------------------------------
  
})
