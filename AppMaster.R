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
                titlePanel("Análise comparativa de casos e mortes por Covid-19"), 
                mainPanel(p("O banco de dados foi obtido pelo site: ",
                          a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                          p("Atualizado em:",df1$dateRep[1])
                          ),
  column(
    6,fluidRow(column(6, selectizeInput("All", "Dados sobre os casos", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
               column(6, selectizeInput("All2", "Dados sobre as mortes", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
  ),
  column(
    12,fluidRow(column(12, plotlyOutput('plot'))
    )
  ),
  mainPanel(
    p(strong("Criado por: "), a(href="https://github.com/AlessandroPTSN", "Alessandro Pereira Torres")))
)



# Server code
server <- function(input, output) {
  
  outVar <- reactive({
    df1 %>%
      filter(countriesAndTerritories %in% input$All) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos." ,"Total:",x, sep = " ")) %>% 
      arrange(Tempo) %>%
      droplevels()
  })
  
  outVar2 <- reactive({
    df2 %>%
      filter(countriesAndTerritories %in% input$All2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes.","Total:",x, sep = " ")) %>% 
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
