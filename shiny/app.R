# Exemple d'application web Shiny


library(shiny)
library(shinythemes)
library(palmerpenguins)
library(DT)
library(skimr)
library(ggplot2)
library(readr)

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


noms_var_num <- c(
    "bill length" = "bill_length_mm",
    "bill depth" = "bill_depth_mm",
    "flipper length" = "flipper_length_mm",
    "body mass" = "body_mass_g"
)

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
        DT::dataTableOutput("volcan")
        ),
        
        #Onglet pour les prédictions:
        tabPanel(
          title = "Prédictions des intencité volcaniques"
        ),
    
        #Volet carte animée:
        tabPanel(
          title = "Carte animée",
          img(src = "ToutesAnnées.gif", width = "100%")
        ),
        
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
        tabPanel(
            title = "Graphiques",
            sidebarLayout(
                sidebarPanel = sidebarPanel(
                    selectInput(
                        inputId = "x",
                        label = "Variable sur l'axe des abcisses (x) :",
                        choices = noms_var_num
                    ),
                    selectInput(
                        inputId = "y",
                        label = "Variable sur l'axe des ordonnées (y) :",
                        choices = noms_var_num,
                        selected = "bill_depth_mm"
                    ),
                    radioButtons(
                        inputId = "color",
                        label = "Variable déterminant la couleur des points :",
                        choices = c("species", "island", "sex", "year")
                    ),
                    img(src = 'bill_depth.png', width = "100%"),
                    p("Illustration de @allison_horst ", tags$a(
                        href = "https://github.com/allisonhorst/palmerpenguins", 
                        "https://github.com/allisonhorst/palmerpenguins"
                    ))
                ),
                mainPanel = mainPanel(
                    plotOutput("scatterplot", height = 600)   
                )
            )
        )
    )
)


# Define server logic
server <- function(input, output) {
    
    output$table_donnees <- DT::renderDataTable({
        penguins     # penguins est un jeu de donnees du package palmerpenguins
    })
    
    #Nos données:
    output$volcan <- DT::renderDataTable({
      volcan
    })

    output$sortie_str <- renderPrint({
        str(volcan)
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
#            names(table)=c(volcan$variable,"Fréquence")
        } else if (as.character(input$variable) %in% c("elevation", "vei")) {
            t(as.matrix(summary(volcan[[input$variable]])))
        }
    })
    
    output$scatterplot <- renderPlot({
        ggplot(data = penguins) +
            geom_point(
                mapping = aes(
                    x = .data[[input$x]],
                    y = .data[[input$y]],
                    color = factor(.data[[input$color]])
                ),
                size = 2
            ) +
            theme_minimal(base_size = 15) +
            scale_color_manual(values = c("darkorange","purple","cyan4")) +
            labs(
                title = "Relation entre deux variables numériques en fonction d'une variable catégorique :",
                x = names(noms_var_num)[noms_var_num == input$x],
                y = names(noms_var_num)[noms_var_num == input$y],
                color = input$color
            )
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
