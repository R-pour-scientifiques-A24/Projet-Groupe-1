# Exemple d'application web Shiny

library(shiny)
library(shinythemes)
library(palmerpenguins)
library(DT)
library(skimr)
library(ggplot2)
library(readr)
library(leaflet)
#library(plotly)


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

volcan_resume<-volcan[,c(3,15,20,16,25,17,18,19,5,7,9,4,6,8,13,14)]


#Création du jeu de donnée pour la carte interactive
tab_point <- data.frame(nom=volcan$volcano_name,long=volcan$longitude,lati=volcan$latitude, pays=volcan$country, derniere_erup=volcan$last_eruption_year, altitude=volcan$elevation)
tab_point <- unique(tab_point)
#Création de l'icône que l'on souhaite afficher
icone <- icons(iconUrl = "https://raw.githubusercontent.com/R-CoderDotCom/chinchet/main/inst/red.png",
               iconWidth = 50, iconHeight = 50)



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
            img(src = 'magma_lave.avif', width = "100%"),
            p("Source :", tags$a(
                href = "https://www.futura-sciences.com/planete/questions-reponses/volcan-volcan-difference-lave-magma-6988/", 
                "https://www.futura-sciences.com/planete/questions-reponses/volcan-volcan-difference-lave-magma-6988/"
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
              ),
              
            ),
            mainPanel = mainPanel(
              img(src = "VEIfigure_en.svg.png", width = "45%"),
              p("Index de force des éruptions volcaniques"), 
              p("Source :", tags$a(href = "https://www.kaggle.com/datasets/jessemostipak/volcano-eruptions", 
                                   "https://www.kaggle.com/datasets/jessemostipak/volcano-eruptions"))
            ),
          ),
          verbatimTextOutput("sortie_predict"),
        ),
    
        #Volet carte animée:
        tabPanel(
          title = "Carte animée",
          img(src = "ToutesAnnées.gif", width = "100%")
        ),
  
  
        #Test vidéo:
        tabPanel(title = "Test video",
            mainPanel(
              p("Éruptions volcaniques par années"),
              tags$video(
                src = "hirondellebic_M_vidéo_480px320px.mp4",
                type = "video/mp4",
                controls=TRUE, autoplay=TRUE),
            )
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
                        data = volcan_resume
                    )
                ),
                mainPanel = mainPanel(
                    textOutput("type_var"),
                    plotOutput("graphDesc"),
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
        str(volcan_resume)
    })

    
    modpred <- reactive({
      mod <- lm(vei~region+start_year+event_type+primary_volcano_type+elevation, data=volcan)
      predict(mod, newdata = data.frame(region=input$var1,
                                        elevation=input$var2,
                                        start_year=input$var3,
                                        event_type=input$var4,
                                        primary_volcano_type=input$var5))
    })
    
    output$sortie_predict <- renderText(
      paste("Prédiction de l'intensité d'un volcan avec les caractéristiques choisies", 
            ":", round(modpred()))
    )
  
    
    output$type_var <- renderText({
        if (as.character(input$variable) %in% c("volcano_name", "region")) {
            paste("Fréquences des modalités observées de la variable catégorique", input$variable, ":")
        } else if (as.character(input$variable) %in% c("elevation", "vei")) {
            paste("Mesures de position et de tendance centrale pour les observations de la variable numérique", input$variable, ":")
        }
    })
    
    
    output$type_var <- renderText({
      if (as.character(input$variable) %in% c("volcano_name", "primary_volcano_type", "country", "region",  "subregion","area_of_activity","eruption_category", "evidence_method_dating", "event_type")) {
                                              paste("Fréquences des modalités observées de la variable catégorique", input$variable, ":")
      } else {
        paste("Mesures de position et de tendance centrale pour les observations de la variable numérique", input$variable, ":")
      }
    })
        
    output$sortie_stat_desc <- renderTable({
        if (as.character(input$variable) %in% c("volcano_name", "primary_volcano_type", "country", "region",  "subregion","area_of_activity","eruption_category", "evidence_method_dating", "event_type")) {
          table=as.data.frame(table(volcan_resume[[input$variable]], useNA = "ifany"))
          table=table[order(table$Freq, decreasing = TRUE),]
            # names(table)=c(input$variable,"Fréquence")
        } else {
          t(as.matrix(summary(volcan_resume[[input$variable]])))
        }
    })
    
    output$graphDesc<- renderPlot({
      #Faire un plotly pour avoir des données quand on passe la souris? Ou juste 
      #faire une meilleure sélection de graphiques ou ggplot plus beau? Ajuster titres et axes.
      
      tabvolfreq<-table(volcan_resume[[input$variable]], useNA = "ifany")
      tabvolfreqord<-tabvolfreq[order(tabvolfreq,decreasing=TRUE)]
      tabvolfreqord10<-tabvolfreqord[1:10]

      
      if (as.character(input$variable) %in% c("volcano_name", "primary_volcano_type", "country", "region",  "subregion","area_of_activity","eruption_category", "evidence_method_dating", "event_type")) {
        barplot(tabvolfreqord10, las=2)
        
        #p<-ggplot(data=volcan_resume, aes(x=volcan_resume[[input$variable]])) +
        #  geom_bar()
        #p
        
      } else {
        hist(na.omit(volcan_resume[[input$variable]]))
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
