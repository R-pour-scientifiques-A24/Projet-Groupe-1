# Exemple d'application web Shiny



library(shiny)
library(shinythemes)
library(palmerpenguins)
library(DT)
library(skimr)
library(ggplot2)


noms_var_num <- c(
    "bill length" = "bill_length_mm",
    "bill depth" = "bill_depth_mm",
    "flipper length" = "flipper_length_mm",
    "body mass" = "body_mass_g"
)

# Define UI (User Interface) for application
ui <- fluidPage(
    
    theme = shinytheme("flatly"),
    
    titlePanel("Exploration du jeu de données des pingouins de Palmer"),
    
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
        tabPanel(
            title = "Données brutes",
            DT::dataTableOutput("table_donnees")
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
                        data = penguins
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
    
    output$sortie_str <- renderPrint({
        str(penguins)
    })
    
    output$type_var <- renderText({
        if (as.character(input$variable) %in% c("species", "island", "sex", "year")) {
            paste("Fréquences des modalités observées de la variable catégorique", input$variable, ":")
        } else {
            paste("Mesures de position et de tendance centrale pour les observations de la variable numérique", input$variable, ":")
        }
    })
    
    output$sortie_stat_desc <- renderTable({
        if (as.character(input$variable) %in% c("species", "island", "sex", "year")) {
            t(as.matrix(table(penguins[[input$variable]], useNA = "ifany")))
        } else {
            t(as.matrix(summary(penguins[[input$variable]])))
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
