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
                    video{
                      display: block;
                      margin: 0 auto;
                    }
                    "))
    ),
   tags$script(HTML('type="text/javascript" src="jquery.dataTables.js"')),
   tags$script(HTML("type=\"text/javascript\" 
                      $(document).ready(function() {
                        $('#tabOut').dataTable( {
                        \"language\": {
                          \"url\": \"//datatables.net/plug-ins/i18n/German\"
                        }
                      }); 
                    });")
                ),
  
# --------------------------------------------------------------------------------------------------------------
  # controlPanel mit ausgeblendeten Schaltern
  conditionalPanel("false",
                   checkboxInput("lgIn", "Login", value = TRUE),
                   checkboxInput("helpCB", "Hilfe", value = FALSE),
                   checkboxInput("imprCB", "Impressum", value = FALSE),
                   checkboxInput("iPanelOn", "InputPanel on/off", value = TRUE),
                   checkboxInput("hrPlotOn", "HR-Plot on/off", value = FALSE),
                   
                   selectInput(inputId = "currentPanel", label = "Aktuelles HauptPanel:", choices = 
                                 list("workingPanel", "helpPanel", "imprintPanel")),
                   sliderInput(inputId = "currentLanguage", label = "Aktuelle Sprache", min = 1, max = numDics, step = 1, value = 1)
                   ),

# --------------------------------------------------------------------------------------------------------------
  # Kopfzeile mit gefloateten Panels
  div(style = "padding-right: 10px; float:right", uiOutput("flag")),
  conditionalPanel("input.helpCB != true",
                   div(id = "hil", style = "padding-right: 10px; float:right", a(icon("question"), uiOutput("hilfe_t")))
                  ),
  conditionalPanel("input.imprCB != true",
                    div(id = "imp", style = "padding-right: 10px; float:right", a(icon("info"), uiOutput("impressum_t")))
                   ),
  conditionalPanel("input.iPanelOn == true && input.imprCB != true && input.helpCB != true",
                    div(id = "decrease", style = "padding-right: 10px; float:right", a(icon("toggle-on"), uiOutput("pers0_t")))
                  ),
  conditionalPanel("input.iPanelOn != true && input.imprCB != true && input.helpCB != true",
                   div(id = "increase", style = "padding-right: 10px; float:right", a(icon("toggle-off"), uiOutput("pers1_t")))
                  ),
  
# --------------------------------------------------------------------------------------------------------------
  # TitelPanel
  titlePanel(title = textOutput("title_t"), windowTitle = "CoroVis - powered JEG ShinyGroupAG and R"),
  hr(),

# --------------------------------------------------------------------------------------------------------------
  # WorkingPanel mit drei untergeordneten Panels: PatientPanel, SidebarPanel und MainPanel
  conditionalPanel("input.lgIn == true && input.helpCB == false && input.imprCB == false",

          # ----------------------------------------------------------------------------------------------------
            # WorkingPanel > PatientPanel zur Eingabe der administrativen Patientendaten (IDAT)
                   conditionalPanel("input.iPanelOn == true",
                   inputPanel(
                     # Eingabe des Namens
                     textInput("nachname", label = textOutput("name_t")),
                     
                     # Eingabe des Vornamens
                     textInput("vorname", label = textOutput("vorname_t")),
                     
                     # Eingabe des Geburtsdatums und des Alters
                     uiOutput("gebdat"),
                     
                     # Ausgabe des Alters
                     uiOutput("alter"),
                     
                     # Eingabe des Geschlechts
                     uiOutput("gesch"),
                     
                     # Eingabe der Groesse
                     sliderInput("groesse", label = textOutput("groesse_t"), value = 170, min = 120, max = 210),
                     
                     # Eingabe des Koerpergewichts
                     sliderInput("gewicht", label = textOutput("gewicht_t"), value = 70, min = 45, max = 180),
                     
                     # Ausgabe des BMI
                     uiOutput("bmiUI")

                   ),
                   hr()
                   ),
                   
          # ----------------------------------------------------------------------------------------------------
          # WorkingPanel > SidebarPanel zum Laden von Dateien und zur Vorgabe von Belastungswerten
                   sidebarLayout(
                     
                     #sidebarPanel für die Patientendaten
                     sidebarPanel(
                       
                       # Auswahl der Trainingsdatei
                       uiOutput("fInput"),
                       
                       # Eingabe der Dateiauswahl für die Trainingsdaten
                       uiOutput("datSelect"),
                       
                       # Eingabe des Zeitraums, dessen Daten ausgewertet werden sollen
                       uiOutput("zeitraumSelect"),
                       hr(),
                       
                       # Risikoklasse auswaehlen
                       uiOutput("riskclass"),
                       
                       # Individuelle Belastungsintensitaet auswaehlen
                       uiOutput("intensity"),
                       
                       # Maximale Herzfrequenz auswaehlen
                       sliderInput("hfMax", label = textOutput("hfMax_t"), value = 70, min = 40, max = hfMaxGeneral),
                       
                       # Herzfrequenzbereich fuer die Belastung auswaehlen
                       sliderInput("hfBer", label = textOutput("frequenzbereich_t"), value = c(60, 80), min = 40, max = hfMaxGeneral)

                     ),
                     
          # ----------------------------------------------------------------------------------------------------
          # WorkingPanel > MainPanel für die Patientendatenvisualisierung, als Tabsets organsiert
                     mainPanel(
                       
                       tabsetPanel(id = "tP",
                          tabPanel(textOutput("start"), id = "plo00", uiOutput("startOut"), icon = icon("home")),
                          
                          tabPanel(textOutput("daten_t"), id = "plo0", value = "tP1", dataTableOutput("tabOut"),icon = icon("table")),
                          
                          tabPanel(textOutput("zeit_t"), 
                                   id = "plo1", 
                                   plotOutput("explorationPlot"),
                                   uiOutput("selAxOut"),
                                   conditionalPanel("input.hrPlotOn",
                                                    plotOutput("explorationPlotHR")), 
                                   icon = icon("bar-chart")),         
                          
                          tabPanel(textOutput("karte_t"), 
                                   id = "plo5",
                                   leafletOutput("tOut1", height = 600),
                                   icon = icon("road")),
                          
                          tabPanel(textOutput("gesamt_t"), id = "plo6", tableOutput("tOut2"), icon = icon("pie-chart")),
                          
                          tabPanel(textOutput("settings"), id = "plo7", uiOutput("settingsOut"), icon = icon("sliders")),
                          
                          type = "pills",
                          
                          selected = "plo0"
                       )
                     )
                   )
  ),
  
# --------------------------------------------------------------------------------------------------------------
  # HelpPanel zur Anzeige von Hilfetexten
  conditionalPanel("input.helpCB == true",
                   uiOutput("helpPanel")
  ),
  
# --------------------------------------------------------------------------------------------------------------
  # ImprintPanel zur Anzeige des Impressums
  conditionalPanel("input.imprCB == true",
                   uiOutput("aboutPanel")
  ),

  conditionalPanel("input.currentPanel == 'imprintPanel'",
                   h1(textOutput(outputId = "imprintTitle")),
                   br(),
                   htmlOutput(outputId = "imprintHTML"),
                   actionButton(inputId = "imprintBack", label = textOutput(outputId = "imprintBackLabel"))
  )

  
))
