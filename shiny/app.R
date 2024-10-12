# Exemple d'application web Shiny

library(shiny)
library(shinythemes)
library(palmerpenguins)
library(DT)
library(skimr)
library(ggplot2)
library(readr)
library(leaflet)

#Chargement des tableaux
eruptions <- read_csv("../data/eruptions.csv")
events <- read_csv("../data/events.csv")
volcano <- read_csv("../data/volcano.csv")
#Création du jeu de donnée
volcan1<-merge(eruptions,events, by="eruption_number") #Jeu intermédiaire pour 1er merge
volcan<-merge(volcan1,volcano, by.x="volcano_number.x", by.y="volcano_number")
volcan<-volcan[, -c(8,9,12,13,16,17,18,21,23,24,25,31,32,37:40,42:45)]
colnames(volcan)[1] <- "volcano_number"
colnames(volcan)[3] <- "volcano_name"
colnames(volcan)[10] <- "latitude"
colnames(volcan)[11] <- "longitude"
volcan$last_eruption_year<-as.numeric(volcan$last_eruption_year)
volcan$minor_rock_1<-ifelse(volcan$minor_rock_1== unique(volcan$minor_rock_1)[3],NA,volcan$minor_rock_1)
remove(volcan1)#retrait du jeu intermédiaire
#Création du jeu de donnée pour la carte interactive
tab_point <- data.frame(nom=volcan$volcano_name,long=volcan$longitude,lati=volcan$latitude, pays=volcan$country, derniere_erup=volcan$last_eruption_year, altitude=volcan$elevation)
tab_point <- unique(tab_point)
#Création de l'icône que l'on souhaite afficher
icone <- icons(iconUrl = "https://raw.githubusercontent.com/R-CoderDotCom/chinchet/main/inst/red.png",
               iconWidth = 50, iconHeight = 50)

volcan_resume<-volcan[,c(3,15,20,16,25,17,18,19,5,7,9,4,6,8,13,14)]


#Choix des variables catégoriques pour la prédiction
noms_regions <- unique(volcan$region)
event_type <- unique(volcan$event_type)
volcan_type <- unique(volcan$primary_volcano_type)


# Define UI (User Interface) for application
ui <- fluidPage(
    
    theme = shinytheme("flatly"),
    
    titlePanel("Exploration du jeu de données des volcans"),
    
    navbarPage(
        title = "Choix de visualisation :",
        tabPanel(
            title = icon("home"),
            img(src = 'lter_penguins.png', width = "100%"),
            p("Illustration de @allison_horst ", tags$a(
                href = "https://github.com/allisonhorst/palmerpenguins", 
                "https://github.com/allisonhorst/palmerpenguins"
            ))
        ),
        
        #Données brutes sur les volcans:
        tabPanel(
        title = "Données brutes volcan",
        DT::dataTableOutput("volcan_resume")
        ),
        
        #Onglet pour les prédictions:
        tabPanel(
          title = "Prédictions des intensités volcaniques", 
          img(src = "VEIfigure_en.svg.png", width = "30%"),
          p("Index de force des éruptions volcaniques"), 
          p("Source :", tags$a(href = "https://www.kaggle.com/datasets/jessemostipak/volcano-eruptions", 
                               "https://www.kaggle.com/datasets/jessemostipak/volcano-eruptions")),
          verbatimTextOutput("sortie_predict"),
          
          sidebarLayout(     
            sidebarPanel = sidebarPanel(
              selectInput(
                inputId = "var1",                        
                label = "Choix de la région du volcan :",    
                choices = noms_regions
              ),
              selectInput(
                inputId = "var4",                 
                label = "Choix du type d'événement :",           
                choices = event_type
              ),
              selectInput(
                inputId = "var5",                 
                label = "Choix du type de volcan :",           
                choices = volcan_type
              ),
              sliderInput(inputId = "var3",
                          label = "Choix de l'année où l'événement a débuté :",
                          min=min(volcan$start_year, na.rm=TRUE),
                          max=max(volcan$start_year, na.rm=TRUE), 
                          value = -5000
              ),
              sliderInput(
                inputId = "var2",
                label = "Choix de l'élévation du volcan :",
                min=min(volcan$elevation,na.rm=TRUE),
                max=max(volcan$elevation,na.rm=TRUE),
                value=2200
              )
            ),
            mainPanel = mainPanel(
              textOutput("XXXXXX"),
              tableOutput("YYYYYY")
            )
          )
        ),
    
        #Volet carte animée:
        tabPanel(
          title = "Carte animée",
          img(src = "ToutesAnnées.gif", width = "100%")
        ),
        
        #Statistiques descriptives
        tabPanel(
            title = "Statistiques descriptives",
            p("Structure de l'objet R contenant les données :"),
            verbatimTextOutput("sortie_str"),
            p("Statistiques descriptives pour une variable sélectionnée :"),
            sidebarLayout(     
                sidebarPanel = sidebarPanel(
                    varSelectInput(
                        inputId = "variable",
                        label = "Choix de la variable :",
                        data = volcan
                    )
                ),
                mainPanel = mainPanel(
                    textOutput("type_var"),
                    tableOutput("sortie_stat_desc")
                )
            )
        ),
        
        #Carte interactive
        tabPanel(
          title = "Carte interactive",
         
            leafletOutput("carte_interac",width = "100%", height = 800)
          
        )
        )
     )
       



# Define server logic
server <- function(input, output) {
    
    
    #Nos données complètes:
    output$volcan <- DT::renderDataTable({
      volcan
    })
    
    #Nos données partielles:
    output$volcan_resume  <- DT::renderDataTable({
      volcan_resume
    })

    output$sortie_str <- renderPrint({
        str(volcan)
    })
    
#    output$sortie_predict <- renderText({
#      paste("Prédiction de l'intensité du volcan", input$variable, ":")
#    })
    
    modpred <- reactive({
      mod <- lm(vei~region+start_year+event_type+primary_volcano_type+elevation, data=volcan)
      predict(mod, newdata = data.frame(region=input$var1,
                                        elevation=input$var2,
                                        start_year=input$var3,
                                        event_type=input$var4,
                                        primary_volcano_type=input$var5))
    })
    output$sortie_predict <- renderText({
      paste("Prédiction de l'intensité d'un volcan avec les caractéristiques choisies", 
            ":", round(modpred(), digits = 5))
    })
    
    
    output$type_var <- renderText({
        if (as.character(input$variable) %in% c("volcano_name", "region")) {
            paste("Fréquences des modalités observées de la variable catégorique", input$variable, ":")
        } else if (as.character(input$variable) %in% c("elevation", "vei")) {
            paste("Mesures de position et de tendance centrale pour les observations de la variable numérique", input$variable, ":")
        }
    })
    
    output$sortie_stat_desc <- renderTable({
        if (as.character(input$variable) %in% c("volcano_name", "region")) {
            table=as.data.frame(table(volcan[[input$variable]], useNA = "ifany"))
#            names(table)=c(input$variable,"Fréquence")
        } else if (as.character(input$variable) %in% c("elevation", "vei")) {
            t(as.matrix(summary(volcan[[input$variable]])))
        }
    })
    
    
    
    output$carte_interac <- renderLeaflet({
      leaflet(options=leafletOptions(minZoom = 2, maxZoom = 18))%>% addTiles() %>% 
        addMarkers(data = data.frame(lng = tab_point$long, lat = tab_point$lati),
                   popup = paste0("Nom du volcan: ",tab_point$nom, "<br>", "Pays: ",tab_point$pays, "<br>",
                                  "Altitude: ",tab_point$altitude,"<br>","Année de la dernière activité volcanique: ",tab_point$derniere_erup),
                   icon = icone)
    })
      
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
