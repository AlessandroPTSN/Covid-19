library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(tidyr)
library(shinythemes)
library(viridis)
library(readxl)
library(httr)
library(stringr)


#Datasets

###############################################################
template <- tempfile(fileext = ".xlsx")
url <- httr::GET("https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral",
                 httr::add_headers("X-Parse-Application-Id" =
                                     "unAFkcaNDeXajurGB7LChj8SgQYS2ptm")) %>%
  httr::content() %>%
  '[['("results") %>%
  '[['(1) %>%
  '[['("arquivo") %>%
  '[['("url")



httr::GET(url,
          write_disk(template) )

my_data <- read_excel(template)
my_data$data2 = as.Date(my_data$data,"%Y-%m-%d")




cidade <- my_data %>%
  select(municipio,data2,populacaoTCU2019,casosAcumulado,obitosAcumulado,estado) %>%
  group_by(estado,municipio,data2) %>%
ungroup()

cidade = cidade %>% drop_na()



cidade$countriesAndTerritories = str_c(cidade$municipio," ","(",cidade$estado,")")
cidade$dateRep = cidade$data2
cidade$cases = as.integer(cidade$casosAcumulado)
cidade$deaths = as.integer(cidade$obitosAcumulado)
cidade$Data = cidade$data2
cidade$Quantidade = as.integer(cidade$casosAcumulado)

cidade = cidade %>% drop_na()

j=aggregate(cidade$cases, by=list(countriesAndTerritories=cidade$countriesAndTerritories), FUN=max)
k=aggregate(cidade$deaths, by=list(countriesAndTerritories=cidade$countriesAndTerritories), FUN=max)


cidade2=cidade

cidade=merge(cidade,j)
cidade2=merge(cidade2,k)

###############################################################



df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
df1$Data = as.Date(df1$dateRep,"%d/%m/%Y")
df1$Quantidade = df1$cases
df2=df1

h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
hh=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(hh)[names(hh) == "x"] <- "o"
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
ii=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(ii)[names(ii) == "x"] <- "o"

df1=merge(df1,h)
df2=merge(df2,i)



mini1=data.frame(countriesAndTerritories=h$countriesAndTerritories,incidencia=round((h$x*100000)/hh$o,1))
mini2=data.frame(countriesAndTerritories=h$countriesAndTerritories,mortalidade=round((i$x*100000)/ii$o,1))

df1=merge(df1,mini1)
df2=merge(df2,mini2)


# UI
ui <- navbarPage("Covid-19",theme = shinytheme("slate"),
                 
                 tabPanel("Mundo", 
                titlePanel("Análise comparativa de casos e mortes por Covid-19 no mundo"), 
                mainPanel(p("O banco de dados foi obtido pelo site: ",
                          a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                          p("Atualizado em:",df1$dateRep[1])
                          ),
  column(
    6,fluidRow(column(6, selectizeInput("All", "Dados sobre os casos diários", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
               column(6, selectizeInput("All2", "Dados sobre as mortes diárias", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
  ),
  column(
    12,fluidRow(column(12, plotlyOutput('plot'))
    )
  ),
  mainPanel(
    p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN", "Alessandro Pereira Torres")))
),



tabPanel("Brasil",
         titlePanel("Análise comparativa de casos e mortes por Covid-19 no Brasil"), 
         mainPanel(p("O banco de dados foi obtido pelo site: ",
                     a(href="https://github.com/liibre/coronabr/blob/master/R/get_corona_minsaude.R", "GitHub")),
                   p("Atualizado em:",cidade$data2[length(cidade$data2)])
         ),
         column(
           6,fluidRow(column(6, selectizeInput("All11", "Dados sobre os casos acumulados", multiple = T,choices = unique(cidade$countriesAndTerritories), 
                                               options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
                      column(6, selectizeInput("All22", "Dados sobre as mortes acumuladas", multiple = T,choices = unique(cidade2$countriesAndTerritories), 
                                               options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
         ),
         column(
           12,fluidRow(column(12, plotlyOutput('plot2'))
           )
         ),
         
         
         mainPanel(
           p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN", "Alessandro Pereira Torres")))

         ),




tabPanel("Sobre",
         
         


             titlePanel("Sobre o site"),
             p("COVID-19 é uma doença infeciosa causada pelo coronavírus, seus principais sintomas são febre, tosse seca e cansaço.",br(), 
             "O site tem como objetivo mostrar as pessoas o avanço do Covid-19 ao decorrer dos dias, através de gráficos interativos suportados pelo",
               a(href="https://shiny.rstudio.com", "Shiny,"),
               "um dos vários pacotes do ",
               a(href="https://www.r-project.org", "R"),br(),br(),p("Cálculo da incidência : (casos * 100000)/(População)"),
             p("Cálculo da mortalidade : (mortes * 100000)/(População)")),

         

            titlePanel("Criador"),
            strong("Alessandro Pereira Torres De Sa Neto"),
            br(),
            mainPanel(p("Graduando em estatística na ",
                     a(href="https://ufrn.br", "UFRN"),
                     " com experiência em consultoria estatística no",
                     a(href="http://lea.estatistica.ccet.ufrn.br", "LEA"),
                     "e criação de jogos digitais pelo ",a(href="https://imd.ufrn.br/portal/", "IMD,"),
                     "possui conhecimento nas áreas de Estatística, Probabilidade, Matemática, Machine Learning, Big Data, Informática e Programação em:"),
                   p("-R",br(),"-Python",br(),"-Markdown e LaTeX",br(),"-Excel",br(),"-C#"),
                   br(),
                   strong("GitHub: ",a(href="https://github.com/AlessandroPTSN", "https://github.com/AlessandroPTSN")),
                   br(),
                   strong("Email: alessandroptsn@yahoo.com.br" )
         )
         )

)




# Server code
server <- function(input, output) {
  
  outVar <- reactive({
    df1 %>%
      filter(countriesAndTerritories %in% input$All) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos." ,"Total:",x,"Incidência:",incidencia, sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVar2 <- reactive({
    df2 %>%
      filter(countriesAndTerritories %in% input$All2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes.","Total:",x,"Mortalidade:",mortalidade, sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$plot <- renderPlotly({
    plot_ly(data=outVar(), x=~Data,  y = ~Quantidade,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVar2(), x=~Data,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'v'))
  }) 
  
  
  
  
  outVara <- reactive({
    cidade %>%
      filter(countriesAndTerritories %in% input$All11) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos acumulados. Total:",x, sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVara2 <- reactive({
    cidade2 %>%
      filter(countriesAndTerritories %in% input$All22) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes acumuladas. Total:",x, sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(data=outVara(), x=~Data,  y = ~Quantidade,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVara2(), x=~Data,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'v'))         
  })
  
}
# Return a Shiny app object
shinyApp(ui = ui, server = server)
