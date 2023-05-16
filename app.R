library(shiny)
library(timetk)
library(plotly)
library(gsheet)

inscritos <- gsheet2tbl("https://docs.google.com/spreadsheets/d/14YWebKqKK5pBFeEIgaguPSJCZJHiV0jr960TffKKPbU/edit?usp=sharing")
inscritos$Data <- as.Date(inscritos$`Carimbo de data/hora`, format = "%d/%m/%Y")

dadossemanais <- inscritos %>%
  mutate(Semana = lubridate::floor_date(Data, "week"))

contagem_semanal <- dadossemanais %>%
  group_by(Semana, Category) %>%
  summarise(Total_Inscritos = n())

total <- sum(contagem_semanal$Total_Inscritos)

ui <- fluidPage(
  titlePanel("Acompanhamento das Inscrições VII SER"),
  mainPanel(
    h3(paste("Até hoje", Sys.Date(), "temos", total, "inscritos")),
    h3("Gráfico 1: VII SER - Inscritos por semana"),
    plotlyOutput("graph1"),
    h3("Gráfico 2: VII SER - Inscritos por categoria"),
    plotlyOutput("graph2")
    
  )
)

server <- function(input, output) {
  output$graph1 <- renderPlotly({
    graph1 <- ggplot(contagem_semanal) +
      aes(x = Semana, fill = Category, weight = Total_Inscritos) +
      geom_bar() +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Semana",
        y = "Contagem de inscritos",
        fill = "Categoria"
      ) +
      theme(
        legend.position = "center",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) 
    
    ggplotly(graph1)
  })
  
  output$graph2 <- renderPlotly({
    graph2 <- ggplot(contagem_semanal) +
      aes(x = Category, y = Total_Inscritos, fill = Category) +
      geom_col() +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Categorias",
        y = "Contagem de inscritos",
        fill = "Categoria"
      ) +
      theme(
        legend.position = "center",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) 
    
    ggplotly(graph2)
  })
}

shinyApp(ui, server)
