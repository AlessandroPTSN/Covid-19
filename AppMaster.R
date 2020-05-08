library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyr)
library(shinythemes)

#Dataset
df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
df1$Tempo = as.Date(df1$dateRep,"%d/%m/%Y")
df1$Escala = df1$cases
df2=df1

h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)

df1=merge(df1,h)
df2=merge(df2,i)

# UI
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("Covid-19"), 
                mainPanel("O banco de dados foi obtido do site https://opendata.ecdc.europa.eu"),
  column(
    6,fluidRow(column(6, selectizeInput("All", "Casos", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha um local:'))),
               column(6, selectizeInput("All2", "Mortes", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha um local:'))))
  ),
  column(
    12,fluidRow(column(12, plotlyOutput('plot'))
    )
  ) 
)


# Server code
server <- function(input, output) {
  
  outVar <- reactive({
    df1 %>%
      filter(countriesAndTerritories %in% input$All) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "Casos" ,"Total",x, sep = "_")) %>% 
      arrange(Tempo) %>%
      droplevels()
  })
  
  outVar2 <- reactive({
    df2 %>%
      filter(countriesAndTerritories %in% input$All2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "Mortes","Total",x, sep = "_")) %>% 
      arrange(Tempo) %>%
      droplevels()
  })
  
  output$plot <- renderPlotly({
    plot_ly(data=outVar(), x=~Tempo,  y = ~Escala,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories) %>%
      add_trace(data=outVar2(), x=~Tempo,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories)  %>%
      layout(legend = list(orientation = 'v'))         
  }) 
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

