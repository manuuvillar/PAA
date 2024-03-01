data<-read.csv('data_tratado.csv')
setwd('C:/Users/Utilizador/OneDrive - Universidade do Minho/2-ano/aprendizage_automatica')
attach(data)
library(dplyr)
library(ggplot2)
library(plotly)

soma_modelos_por_marca<- data %>% 
  group_by(brand) %>% 
  summarise(total_modelos = n_distinct(model))
soma_modelos_por_marca_media_mediana_preco


grafico <- ggplot(soma_modelos_por_marca, aes(x = total_modelos, y = brand)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Número de Modelos por Marca", x = "Nº de modelos", y = "Marca") 
ggplotly(grafico)

#------------------------------------------



grafico_interativo <- plot_ly(
  data,
  y = ~price,
  color = ~brand,
  type = "box",
  showlegend = TRUE
) %>% layout(
  title = "Boxplot de Preço por Marca",
  xaxis = list(title = "Marca"),
  yaxis = list(title = "Preço"),
  clickmode = "event"  # Habilita o modo de clique para interatividade
)

# Exibir o gráfico interativo
print(grafico_interativo)

#-------------------------------
library(shiny)
ui <- fluidPage(
  titlePanel("Boxplot de Preço por Marca"),
  sidebarLayout(
    sidebarPanel(
      selectInput("marca", "Escolha a marca:", choices = unique(data$brand))
    ),
    mainPanel(
      plotlyOutput("boxplot")
    )
  )
)

server <- function(input, output) {
  output$boxplot <- renderPlotly({
    dados_filtrados <- filter(data, brand == input$marca)
    
    plot_ly(
      dados_filtrados,
      y = ~price,
      type = "box",
      color = ~brand
    ) %>% layout(
      title = paste("Boxplot de Preço para", input$marca),
      xaxis = list(title = "Marca"),
      yaxis = list(title = "Preço")
    )
  })
}

shinyApp(ui = ui, server = server)
#-------------------------------------------

soma_modelos_por_marca_ano <- data %>%
  group_by(brand, model_year) %>%
  summarise(total_modelos = n_distinct(model))

a<-ggplot(soma_modelos_por_marca_ano, aes(x =model_year, y = brand, fill = total_modelos)) +
  geom_tile() +  # Adiciona as células do heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Define a escala de cores
  labs(x = "Marca", y = "Ano", fill = "Total de Modelos") +  # Adiciona rótulos aos eixos
  theme_minimal()

ggplotly(a)
