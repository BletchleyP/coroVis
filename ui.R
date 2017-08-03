# ############################################################################ #
#                                                                              #
#    CoroVis - ui.R                                                            #
#    Coding vom 30.07.2017                                                     #
#    Copyright by Gerhard Fuechsl, Enrico Georgi, Janina Rexin                 #
#                                                                              #
# ############################################################################ #

library(shiny)
library(leaflet)
library(colourpicker)

# --- Modifizierter FileInput --------------------------------------------------
coroVisFileInput <- function(inputId, label = NULL, labelIcon = NULL,
                             multiple = FALSE, accept = NULL, width = NULL,
                             progress = TRUE, ...) {
  inputTag <- tags$input(id = inputId, name = inputId, type = "file",
                         class = "coroVisFileIn")
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  div(..., style = if (!is.null(width)) paste0("width: ",
                                               validateCssUnit(width), ";",
                                               "display: inline;"), 
      inputTag,
      tags$label(`for` = inputId, div(icon(labelIcon), label,
                                      class = "btn btn-default action-button")),
      if(progress) {
        tags$div(id = paste0(inputId, "_progress"),
                 class = "progress shiny-file-input-progress",
                 tags$div(class = "progress-bar"))
      }
  )
}

# --- UI-Funktion --------------------------------------------------------------
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
# --- CSS ----------------------------------------------------------------------
  tags$head(
    tags$style(HTML("
                    #flag, #hilfe_t, #impressum_t, #decrease, #increase {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    .centered {
                      text-align: center;
                    }
                    .coroHeader {
                      padding-right: 10px;
                      float: right;
                    }
                    #patient {
                      padding: 10px 25px 0 25px;
                      border: 1px solid lightgray;
                      border-radius: 5px;
                      background-color: WhiteSmoke;
                    }
                    .checkbox label{
                      font-weight: 700;
                    }
                    .checkbox {
                      margin: 0;
                    }
                    .form-group {
                      margin-bottom: 5px;
                    }
                    #marg > .form-group {
                      margin-bottom: 15px;
                    }
                    .modal-title {
                      color: red;
                    }
                    .coroVisFileIn {
                      width: 0.1px;
                      height: 0.1px;
                      opacity: 0;
                      overflow: hidden;
                      position: absolute;
                      z-index: -1;
                    }
                    #fileCounter, #fileLabel {
                      display: inline;
                      margin: 10px;
                    }
                    "))
    ),
    
# --- JavaScript Funktionalitaet zum Toggeln der Tabs --------------------------
  tags$head(tags$script(HTML(
         "Shiny.addCustomMessageHandler('hideTab', function(message){
            var tabsetTarget = document.getElementById(message.tabsetName);
            if (message.hide == 0) {
                tabsetTarget.children[message.number].style.display = 'none';
            } else {
              tabsetTarget.children[message.number].style.display = 'inline';
            }
         });"
        ))),
  
# --- controlPanel mit ausgeblendeten Schaltern --------------------------------
  conditionalPanel("false",
     checkboxInput("iPanelOn", "InputPanel on/off", value = TRUE),
     selectInput(inputId = "currentPanel", label = "Aktuelles HauptPanel:",
                 choices = list("workingPanel", "helpPanel", "imprintPanel")),
     sliderInput(inputId = "language", label = "Aktuelle Sprache",
                 min = 1, max = numDics, step = 1, value = 1),
     checkboxGroupInput("tabsToShow", "Zeige folgende Tabs", 
                        c("Start" = 0, "Daten" = 1, "Plots" = 2, "Karten" = 3,
                          "Zusammenfassung" = 4, "Einstellungen" = 5),
                        selected = c(0, 1, 2, 3, 4, 5)),
     checkboxInput("dataAvailable", "Datensatz geladen?", value = FALSE),
     checkboxInput("mapAvailable", "GPS vorhanden?", value = FALSE)
  ),

# --- Kopfzeile mit gefloateten Panels -----------------------------------------
  div(class = "coroHeader", uiOutput("flag")),
  conditionalPanel("input.currentPanel == 'workingPanel'",
                   div(id = "hil", class = "coroHeader",
                       a(icon("question"), textOutput("hilfe_t"))),
                   div(id = "imp", class = "coroHeader",
                       a(icon("info"), textOutput("impressum_t"))),
                   conditionalPanel("input.iPanelOn == true",
                                    div(id = "decrease", class = "coroHeader",
                                        a(icon("toggle-on"),
                                          textOutput("pers0_t")))),
                   conditionalPanel("input.iPanelOn != true",
                                    div(id = "increase", class = "coroHeader",
                                        a(icon("toggle-off"),
                                          textOutput("pers1_t"))))
  ),
  
# --- titelPanel----------------------------------------------------------------
  titlePanel(title = textOutput("title_t"), windowTitle = "coroVis"),
  hr(),

# --- workingPanel mit drei Sub-Panels: patientPanel, sidebarPanel, mainPanel --
  conditionalPanel("input.currentPanel == 'workingPanel'",

    # --- patientPanel zur Eingabe der Patientendaten (IDAT) -------------------
    conditionalPanel("input.iPanelOn == true",
      div(id = "patient",
        fluidRow(
          column(2, textInput("nachname", textOutput("name_t"))),
          column(2, id = "marg", textInput("vorname", textOutput("vorname_t"))),
          column(2, checkboxInput("inpAlterKA", textOutput("gebdat_t"),
                                  value = FALSE),
                 conditionalPanel("input.inpAlterKA", uiOutput("gebdat"))),
          column(2, checkboxInput("geschKA", textOutput("gesch_t"),
                                  value = FALSE),
                 conditionalPanel("input.geschKA",
                                  radioButtons("inpGesch", NULL,
                                               choiceValues = list("m", "f"),
                                               choiceNames = list("m", "f")))),
          column(2, checkboxInput("groesseKA", textOutput("groesse_t"),
                                  value = FALSE),
                 conditionalPanel("input.groesseKA",
                      sliderInput("groesse", NULL, value = 170,
                                  min = 120, max = 210))),
          column(2, checkboxInput("gewichtKA", label = textOutput("gewicht_t"),
                                  value = FALSE),
                 conditionalPanel("input.gewichtKA",
                      sliderInput("gewicht", NULL, value = 70,
                                  min = 45, max = 180)))
        )
      ), hr()
    ),
                   
    # --- sidebarPanel zum Laden von Dateien und Vorgabe von Belastungswerten --
    sidebarLayout(
      sidebarPanel(
        tags$label(textOutput("fileTitle"), style = "display: block;"),
        coroVisFileInput(inputId = "userfiles", label = textOutput("fileLabel"),
                         labelIcon = "folder-open-o", width= "250px",
                         multiple = TRUE, progress = FALSE),
        textOutput("fileCounter"),
        hr(),
        conditionalPanel("input.dataAvailable",
          tags$label(textOutput("filterTitle")),
          fluidRow(
            column(8, align="left", checkboxInput("filterById", NULL, FALSE),
                   conditionalPanel("input.filterById==true",
                                    selectInput("filterByIdSelect", NULL, NULL))
                   ),
            column(4, align="left", checkboxInput("filterByDate", NULL, FALSE),
                   conditionalPanel("input.filterByDate==true",
                                    selectInput("filterByDateSelect",
                                                NULL, NULL))
                   )
          ),
          hr()
        ),
        tags$label(textOutput("riskfactorTitle")),
        fluidRow(
          column(4, align="center", h5(textOutput("ageTitle")),
                 verbatimTextOutput("alterausgabe", placeholder = TRUE)),
          column(4, align="center", h5(textOutput("bmiTitle")),
                 verbatimTextOutput("bmi")),
          column(4, align="center", h5(textOutput("sex_t")),
                 verbatimTextOutput("sex"))
        ),
        hr(),
        
        # Herzfrequenzgrenzen
        sliderInput("hfMax", label = textOutput("hfMax_t"),
                    min = 45, max = 200, value = c(60, 180)),
        # Belastungslevel
        selectInput("intensity", label = textOutput("risiko_t"),
                    choices = list("Level1"=1, "Level2"=2, "Level3"=3,
                                   "Level4"=4, "Level5"=5, "Level6"=6)),
        # Trainingsfrequenzbereich
        sliderInput("hfBer", label = textOutput("frequenzbereich_t"),
                    min = 45, max = 200, value = c(60, 80))
      ),
                     
    # --- mainPanel zur Patientendatenvisualisierung, als Tabsets organsiert ---
            
      mainPanel(
        tabsetPanel(id = "tP", type = "pills",
          tabPanel(textOutput(outputId = "start"), icon = icon("home"),
                   value = "tP0",
                   hr(),
                   div(class = "centered",
                     titlePanel(textOutput(outputId = "startTitle")),
                     img(src="ts.png", width = "50%"),
                     h3(textOutput(outputId = "startSubtitle"))
                   ),
                   hr()
          ),
          tabPanel(textOutput("daten_t"), icon = icon("table"), value = "tP1",
                   hr(),
                   dataTableOutput("coroTable")
          ),
          tabPanel(textOutput("zeit_t"), icon = icon("bar-chart"),
                   value = "tP2",
                   hr(),
                   plotOutput("explorationPlot"),
                   conditionalPanel("input.dataAvailable",
                      fluidRow(
                        column(4, align="left", offset = 1,
                               selectInput("axisXSelect",
                                           textOutput("axisXSelectLabel"),
                                           choices = c("Zeit", "Entfernung"))
                               ),
                        column(4, align="left",
                               selectInput("axisYSelect",
                                           textOutput("axisYSelectLabel"),
                                           choices = c("Herzfrequenz", "Höhe",
                                                       "Entfernung"))
                               ),
                        column(3, align="left",
                               checkboxGroupInput("plotInclude0",
                                                  textOutput("optPlot"),
                                                  choices = c("ymin <= 0" = 1,
                                                              "Linie" = 2)))
                      )
                   )
          ),
          tabPanel(textOutput("karte_t"), icon = icon("road"), value = "tP3",
                   hr(),
                   leafletOutput("tOut1", height = 600)
          ),
          tabPanel(textOutput("gesamt_t"), icon = icon("pie-chart"),
                   value = "tP4",
                   hr(),
                   h4(textOutput("summaryTitle")),
                   downloadButton("report", textOutput("dlReport")),
                   fluidRow(column(6, align="center",
                                   tableOutput("tOut2")),
                            column(6, align="center",
                                   numericInput("summaryPlotExpander",
                                                textOutput("chPlotSize"), 1,
                                                min = 0.25, max = 5,
                                                step = 0.25),
                                   plotOutput("summaryPlot",  width = "100%"))
                   )
          ),
          tabPanel(textOutput("settings"), icon = icon("sliders"),
                   value = "tP5",
                   hr(),
                   h4(textOutput("settingsColorTitle")),
                   fluidRow(
                     column(3, 
                            colourpicker::colourInput("cpUnder",
                              label = textOutput("settingsColor1Label"),
                              value = basicCol[1], showColour = "background"),
                            colourpicker::colourInput("cpRight",
                              label = textOutput("settingsColor2Label"),
                              value = basicCol[2], showColour = "background"),
                            colourpicker::colourInput("cpAbove",
                              label = textOutput("settingsColor3Label"),
                              value = basicCol[3], showColour = "background")),
                     column(3, actionButton("resetColors",
                            label = textOutput("settingsColorResetLabel"))),
                     column(3, selectInput("mapTilesSelect",
                                           textOutput("SelectMapLayer"),
                                           choices = providers),
                            sliderInput("timezoneSlider",
                                        textOutput("timeShiftCSV"),
                                        min = -12, max = 12, step = 1,
                                        value = getTZshift()),
                            numericInput("plotPointsize",
                                         textOutput("spotSize"), 1,
                                         min = 0, max = 3, step = 0.25),
                            numericInput("plotTextsize",
                                         textOutput("textSize"), 1,
                                         min = 1, max = 2, step = 0.25))
                   ),
                   hr(), h4(textOutput("settingsHFmaxTitle")),
                   sliderInput("overrideMaxHF", label = NULL,
                               min = 100, max = 240, step = 1, value = 200),
                   hr()
          )
        )
      )
    )
  ),
  
# --- helpPanel zur Anzeige des Impressums -------------------------------------
  
  conditionalPanel("input.currentPanel == 'helpPanel'",
                   h3(textOutput("helpTitle")),
                   hr(),
                   htmlOutput("helpHTML"),
                   hr(),
                   actionButton("helpBack", textOutput("helpBackLabel"))
  ),

# --- imprintPanel zur Anzeige des Impressums ----------------------------------
  
  conditionalPanel("input.currentPanel == 'imprintPanel'",
                   h3(textOutput("imprintTitle")),
                   hr(),
                   htmlOutput("imprintHTML"),
                   hr(),
                   actionButton("imprintBack", textOutput("imprintBackLabel"))
  )
))

# *** ENDE ***
