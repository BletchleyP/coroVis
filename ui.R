library(shiny)
library(leaflet)
library(colourpicker)

# modifizierter fileInput
coroVisFileInput <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE, 
                       accept = NULL, width = NULL, progress = TRUE, ...) {
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", class = "coroVisFileIn")
  if (multiple) {inputTag$attribs$multiple <- "multiple"}
  if (length(accept) > 0) {inputTag$attribs$accept <- paste(accept, collapse = ",")}
  div(..., style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";", "display: inline;"), 
      inputTag,
      tags$label(`for` = inputId, div(icon(labelIcon), label, class = "btn btn-default action-button")),
      if(progress) {
        tags$div(id = paste0(inputId, "_progress"), class = "progress shiny-file-input-progress",
                 tags$div(class = "progress-bar"))
      }
  )
}

shinyUI(fluidPage(

# --------------------------------------------------------------------------------------------------------------
  # Skripte des HTML-Head-Teils
  shinyjs::useShinyjs(),
  
  # CSS
  tags$head(
    tags$style(HTML("
                    #flag {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    #hilfe_t {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    #impressum_t {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    #decrease {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    #increase {
                      height: 17px !important;
                      cursor: pointer;
                    }
                    video {
                      display: block;
                      margin: 0 auto;
                    }
                    .centered {
                      text-align: center;
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
                    #xxx > .form-group {
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
    
    # JavaScript Funktionalitaet zum Toggeln der Tabs
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
  
# --------------------------------------------------------------------------------------------------------------
  # controlPanel mit ausgeblendeten Schaltern
  conditionalPanel("false",
                   checkboxInput("iPanelOn", "InputPanel on/off", value = TRUE),
                   selectInput(inputId = "currentPanel", label = "Aktuelles HauptPanel:", choices = 
                                 list("workingPanel", "helpPanel", "imprintPanel")),
                   sliderInput(inputId = "language", label = "Aktuelle Sprache",
                               min = 1, max = numDics, step = 1, value = 1),
                   checkboxGroupInput("tabsToShow", "Zeige folgende Tabs", 
                                      c("Start" = 0, "Daten" = 1, "Plots" = 2, "Karten" = 3,
                                        "Zusammenfassung" = 4, "Einstellungen" = 5),
                                      selected = c(0, 1, 2, 3, 4, 5)),
                   checkboxInput("dataAvailable", "Datensatz geladen?", value = FALSE)
                   
  ),

# --------------------------------------------------------------------------------------------------------------
  # Kopfzeile mit gefloateten Panels
  div(style = "padding-right: 10px; float:right", uiOutput("flag")),
  conditionalPanel("input.currentPanel != 'helpPanel'",
                   div(id = "hil", style = "padding-right: 10px; float:right",
                       a(icon("question"), textOutput("hilfe_t")))
                  ),
  conditionalPanel("input.currentPanel != 'imprintPanel'",
                    div(id = "imp", style = "padding-right: 10px; float:right",
                        a(icon("info"), textOutput("impressum_t")))
                   ),
  conditionalPanel("input.iPanelOn == true && input.currentPanel == 'workingPanel'",
                    div(id = "decrease", style = "padding-right: 10px; float:right",
                        a(icon("toggle-on"), textOutput("pers0_t")))
                  ),
  conditionalPanel("input.iPanelOn != true && input.currentPanel == 'workingPanel'",
                   div(id = "increase", style = "padding-right: 10px; float:right",
                       a(icon("toggle-off"), textOutput("pers1_t")))
                  ),
  
# --------------------------------------------------------------------------------------------------------------
  # titelPanel
  titlePanel(title = textOutput("title_t"), windowTitle = "coroVis"),
  hr(),

# --------------------------------------------------------------------------------------------------------------
  # workingPanel mit drei untergeordneten Panels: patientPanel, sidebarPanel und mainPanel
  conditionalPanel("input.currentPanel == 'workingPanel'",

          # ----------------------------------------------------------------------------------------------------
            # patientPanel zur Eingabe der Patientendaten (IDAT)
              conditionalPanel("input.iPanelOn == true",
                div(id = "patient",
                  fluidRow(
                    column(2, textInput("nachname", label = textOutput("name_t"))),
                    column(2, id = "xxx", textInput("vorname", label = textOutput("vorname_t"))),
                    column(2, checkboxInput("inpAlterKA", label = textOutput("gebdat_t"), value = FALSE),
                           conditionalPanel("input.inpAlterKA", uiOutput("gebdat"))),
                    column(2, checkboxInput("geschKA", label = textOutput("gesch_t"), value = FALSE),
                           conditionalPanel("input.geschKA",
                                radioButtons("inpGesch", label = NULL, choiceValues = list("m", "f"),
                                             choiceNames = list("m", "f")))),
                    column(2, checkboxInput("groesseKA", label = textOutput("groesse_t"), value = FALSE),
                           conditionalPanel("input.groesseKA",
                                sliderInput("groesse", label = NULL, value = 170, min = 120, max = 210))),
                    column(2, checkboxInput("gewichtKA", label = textOutput("gewicht_t"), value = FALSE),
                           conditionalPanel("input.gewichtKA",
                                sliderInput("gewicht", label = NULL, value = 70, min = 45, max = 180)))
                  )
                ), hr()
              ),
                   
          # ----------------------------------------------------------------------------------------------------
            # sidebarPanel zum Laden von Dateien und zur Vorgabe von Belastungswerten
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
                                            selectInput("filterByDateSelect", NULL, NULL))
                           )
                  ),
                  hr()
                ),
                
                
                
                
                fluidRow(
                  column(4, align="center", h5(textOutput("ageTitle")),
                         verbatimTextOutput("alterausgabe", placeholder = TRUE)),
                  column(4, align="center", h5(textOutput("bmiTitle")), verbatimTextOutput("bmi")),
                  column(4, align="center", br(), uiOutput("riskclass"))    # Risikoklasse auswaehlen
                ),
                fluidRow(
                  column(6, align="center", sliderInput("hfMax", label = textOutput("hfMax_t"), value = 70,
                                                        min = 40, max = hfMaxGeneral)),
                  column(6, align="center", uiOutput("intensity"))          # Belastungsintensitaet auswaehlen
                ),
                # Herzfrequenzbereich fuer die Belastung auswaehlen
                sliderInput("hfBer", label = textOutput("frequenzbereich_t"), value = c(60, 80),
                            min = 40, max = hfMaxGeneral)
              ),
                     
          # ----------------------------------------------------------------------------------------------------
            # mainPanel für die Patientendatenvisualisierung, als Tabsets organsiert
              mainPanel(
                tabsetPanel(id = "tP", type = "pills",
                  tabPanel(textOutput(outputId = "start"), icon = icon("home"), value = "tP0",
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
                  tabPanel(textOutput("zeit_t"), icon = icon("bar-chart"), value = "tP2",
                           hr(),
                           plotOutput("explorationPlot"),
                           conditionalPanel("input.dataAvailable",
                              fluidRow(
                                column(4, align="left", offset = 1,
                                       selectInput("axisXSelect", textOutput("axisXSelectLabel"),
                                                   choices = c("Zeit", "Entfernung"))
                                       #sliderInput("axisXZoom", "Zoom X", min = 0, max = 3000, value = c(0,3000))
                                       ),
                                column(4, align="left",
                                       selectInput("axisYSelect", textOutput("axisYSelectLabel"),
                                                   choices = c("Herzfrequenz", "Höhe", "Entfernung"))
                                       #("axisYZoom", "Zoom Y", min = 70, max = 130, value = c(70,130))
                                       ),
                                column(1, align="center",
                                       numericInput("plotPointsize", "Pointsize", 1,
                                                    min = 0, max = 3, step = 0.25),
                                       numericInput("plotTextsize", "Textsize", 1,
                                                    min = 1, max = 2, step = 0.25)),
                                column(2, align="left",
                                       checkboxGroupInput("plotInclude0", "Optionen",
                                                          choices = c("ymin <= 0" = 1, "Linie" = 2)))
                              )
                           )
                           
                  ),
                  tabPanel(textOutput("karte_t"), icon = icon("road"), value = "tP3",
                           hr(),
                           leafletOutput("tOut1", height = 600)
                  ),
                  tabPanel(textOutput("gesamt_t"), icon = icon("pie-chart"), value = "tP4",
                           hr(),
                           h4(textOutput("summaryTitle")),
                           downloadButton("report", "Download report"),
                           fluidRow(column(6, align="center",
                                           tableOutput("tOut2")),
                                    column(6, align="center",
                                           numericInput("summaryPlotExpander", "Change plot size", 1,
                                                        min = 0.25, max = 5, step = 0.25),
                                           plotOutput("summaryPlot",  width = "100%"))
                           )
                  ),
                  tabPanel(textOutput("settings"), icon = icon("sliders"), value = "tP5",
                           hr(),
                           h4(textOutput("settingsColorTitle")),
                           fluidRow(column(3,
                                           colourpicker::colourInput("cpUnder", label = textOutput("settingsColor1Label"),
                                                       value = basicCol[1], showColour = "background"),
                                           colourpicker::colourInput("cpRight", label = textOutput("settingsColor2Label"),
                                                       value = basicCol[2], showColour = "background"),
                                           colourpicker::colourInput("cpAbove", label = textOutput("settingsColor3Label"),
                                                       value = basicCol[3], showColour = "background")),
                                    column(3, actionButton("resetColors",
                                                           label = textOutput("settingsColorResetLabel"))),
                                    column(3, selectInput("mapTilesSelect", "Select map layer", choices = providers))
                           ),
                           hr(),
                           h4(textOutput("settingsHFmaxTitle")),
                           sliderInput("overrideMaxHF", label = NULL, value = hfMaxGeneral,
                                       min = 100, max = 240, step = 1),
                           hr()
                  )
                )
              )
            )
  ),
  
# --------------------------------------------------------------------------------------------------------------
  # helpPanel zur Anzeige des Impressums
  conditionalPanel("input.currentPanel == 'helpPanel'",
                   h3(textOutput("helpTitle")),
                   hr(),
                   htmlOutput("helpHTML"),
                   hr(),
                   actionButton("helpBack", textOutput("helpBackLabel"))
  ),

# --------------------------------------------------------------------------------------------------------------
  # imprintPanel zur Anzeige des Impressums
  conditionalPanel("input.currentPanel == 'imprintPanel'",
                   h3(textOutput("imprintTitle")),
                   hr(),
                   htmlOutput("imprintHTML"),
                   hr(),
                   actionButton("imprintBack", textOutput("imprintBackLabel"))
  )
  
))
