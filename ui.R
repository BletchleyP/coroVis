library(shiny)
library(leaflet)

# Definition der UI
shinyUI(fluidPage(

# --------------------------------------------------------------------------------------------------------------
  # Skripte des HTML-Head-Teils
  shinyjs::useShinyjs(),
  
  # Darstellung der Sprachen-Fähnchen, von Hilfe und Impressum
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
                    .modal-title {
                      color: red;
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
                   checkboxInput("hrPlotOn", "HR-Plot on/off", value = FALSE),
                   
                   selectInput(inputId = "currentPanel", label = "Aktuelles HauptPanel:", choices = 
                                 list("workingPanel", "helpPanel", "imprintPanel")),
                   sliderInput(inputId = "currentLanguage", label = "Aktuelle Sprache",
                               min = 1, max = numDics, step = 1, value = 1),
                   checkboxGroupInput("tabsToShow", "Zeige folgende Tabs", 
                                      c("Start" = 0, "Daten" = 1, "Plots" = 2, "Karten" = 3, "Zusammenfassung" = 4, "Einstellungen" = 5),
                                      selected = c(0, 5))
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
  conditionalPanel("input.iPanelOn == true && input.currentPanel != 'imprintPanel' && input.currentPanel != 'helpPanel'",
                    div(id = "decrease", style = "padding-right: 10px; float:right",
                        a(icon("toggle-on"), textOutput("pers0_t")))
                  ),
  conditionalPanel("input.iPanelOn != true && input.currentPanel != 'imprintPanel' && input.currentPanel != 'helpPanel'",
                   div(id = "increase", style = "padding-right: 10px; float:right",
                       a(icon("toggle-off"), textOutput("pers1_t")))
                  ),
  
# --------------------------------------------------------------------------------------------------------------
  # TitelPanel
  titlePanel(title = textOutput("title_t"), windowTitle = "coroVis"),
  hr(),

# --------------------------------------------------------------------------------------------------------------
  # WorkingPanel mit drei untergeordneten Panels: PatientPanel, SidebarPanel und MainPanel
  conditionalPanel("input.currentPanel == 'workingPanel'",

          # ----------------------------------------------------------------------------------------------------
            # WorkingPanel > patientPanel zur Eingabe der administrativen Patientendaten (IDAT)
              conditionalPanel("input.iPanelOn == true",
                div(id = "patient",
                  fluidRow(
                    column(2, textInput("nachname", label = textOutput("name_t"))),
                    column(2, textInput("vorname", label = textOutput("vorname_t"))),
                    column(2, uiOutput("gebdat")),
                    column(2, uiOutput("gesch")),
                    column(2, sliderInput("groesse", label = textOutput("groesse_t"),
                                          value = 170, min = 120, max = 210)),
                    column(2, sliderInput("gewicht", label = textOutput("gewicht_t"),
                                          value = 70, min = 45, max = 180))
                  )
                ), hr()
              ),
                   
          # ----------------------------------------------------------------------------------------------------
            # WorkingPanel > sidebarPanel zum Laden von Dateien und zur Vorgabe von Belastungswerten
            sidebarLayout(
              sidebarPanel(
                uiOutput("fInput"),             # Auswahl der Trainingsdatei                        
                uiOutput("datSelect"),          # Eingabe der Dateiauswahl für die Trainingsdaten
                uiOutput("zeitraumSelect"),     # Eingabe des Zeitraums, dessen Daten ausgewertet werden sollen
                hr(),
                fluidRow(
                  column(4, align="center", uiOutput("alter")),             # Ausgabe des Alters
                  column(4, align="center", uiOutput("bmiUI")),             # Ausgabe des BMI
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
            # WorkingPanel > mainPanel für die Patientendatenvisualisierung, als Tabsets organsiert
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
                           dataTableOutput("tabOut")
                  ),
                  tabPanel(textOutput("zeit_t"), icon = icon("bar-chart"), value = "tP2",
                           hr(),
                           plotOutput("explorationPlot"),
                           uiOutput("selAxOut"),
                           conditionalPanel("input.hrPlotOn", plotOutput("explorationPlotHR"))
                  ),
                  tabPanel(textOutput("karte_t"), icon = icon("road"), value = "tP3",
                           hr(),
                           leafletOutput("tOut1", height = 600)
                  ),
                  tabPanel(textOutput("gesamt_t"), icon = icon("pie-chart"), value = "tP4",
                           hr(),
                           tableOutput("tOut2")
                  ),
                  tabPanel(textOutput("settings"), icon = icon("sliders"), value = "tP5",
                           hr(),
                           uiOutput("settingsOut")
                  )
                )
              )
            )
  ),
  
# --------------------------------------------------------------------------------------------------------------
  # helpPanel zur Anzeige des Impressums
  conditionalPanel("input.currentPanel == 'helpPanel'",
                   h1(textOutput(outputId = "helpTitle")),
                   br(),
                   tags$video(src='bref.mp4', type='video/mp4', width='40%', controls='controls'),
                   actionButton(inputId = "helpBack", label = textOutput(outputId = "helpBackLabel"))
  ),

# --------------------------------------------------------------------------------------------------------------
  # imprintPanel zur Anzeige des Impressums
  conditionalPanel("input.currentPanel == 'imprintPanel'",
                   h1(textOutput(outputId = "imprintTitle")),
                   br(),
                   htmlOutput(outputId = "imprintHTML"),
                   actionButton(inputId = "imprintBack", label = textOutput(outputId = "imprintBackLabel"))
  )
  
))
