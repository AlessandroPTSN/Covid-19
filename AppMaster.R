library(plotly)
library(dplyr)
library(shiny)
library(tidyr)
library(shinythemes)
library(readxl)
library(httr)
library(stringr)
library(DT)
library(xlsx)
library(readxl)
library(openxlsx)
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
my_data = openxlsx::read.xlsx(template,1)
my_data$data2 = convertToDate(c(my_data$data))



cidade <- my_data %>%
  select(municipio,data2,populacaoTCU2019,casosAcumulado,obitosAcumulado,estado) %>%
  group_by(estado,municipio,data2) %>%
ungroup()
my_data=0
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

cidade=left_join(cidade,j,by="countriesAndTerritories")
cidade2=left_join(cidade2,k,by="countriesAndTerritories")
j=0;k=0
tabela2=cidade
tabela2$y=cidade2$x
tabela2$Local = tabela2$municipio

tabela2 <- tabela2 %>%
  select(Local,x,y) %>%
  group_by(Local) %>%
  summarise('Total Casos' = max(x),'Total Mortes' = max(y))


###############################################################



df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
df1$Data = as.Date(df1$dateRep,"%d/%m/%Y")
df1=df1 %>%  filter(countriesAndTerritories!="Cases_on_an_international_conveyance_Japan")
df1$countriesAndTerritories = str_replace_all(df1$countriesAndTerritories,"_"," ")
df1$Quantidade = df1$cases
df2=df1

h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
hh=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(hh)[names(hh) == "x"] <- "o"
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
ii=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(ii)[names(ii) == "x"] <- "o"



df1=left_join(df1,h,by="countriesAndTerritories")
df2=left_join(df2,i,by="countriesAndTerritories")




mini1=data.frame(countriesAndTerritories=h$countriesAndTerritories,incidencia=round((h$x*100000)/hh$o,1))
mini2=data.frame(countriesAndTerritories=h$countriesAndTerritories,mortalidade=round((i$x*100000)/ii$o,1))
h=0;i=0
hh=0;ii=0

df1=left_join(df1,mini1,by="countriesAndTerritories")
df2=left_join(df2,mini2,by="countriesAndTerritories")
mini1=0
mini2=0


df1$Quantidade = round((df1$Quantidade*100000)/df1$popData2018,1)
df2$deaths = round((df2$deaths*100000)/df2$popData2018,1)



df11=df1
df22=df1



tabela = df1[,-c(1,2,3,4,8,9,14,15)]
tabela$Mortalidade = df2$deaths
tabela$Locais = tabela$countriesAndTerritories


tabela1 <- tabela %>%
  select(Locais,cases,deaths,Quantidade,Mortalidade) %>%
  group_by(Locais) %>%
  summarise('Total Incidência' = sum(Quantidade),'Total Casos' = sum(cases),'Total Mortalidade' = sum(Mortalidade),'Total Mortes' = sum(deaths))
tabela=0

df11$Quantidade = df11$cases

df111=df1
df222=df1


df111=  df111 %>%
  select(countriesAndTerritories,cases,deaths,Data) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cases = rev(cumsum(rev(cases))),deaths = rev(cumsum(rev(deaths))),Data=Data)
  
df222=  df222 %>%
    select(countriesAndTerritories,cases,deaths,Data) %>%
    group_by(countriesAndTerritories) %>%
    mutate(cases = rev(cumsum(rev(cases))),deaths = rev(cumsum(rev(deaths))),Data=Data)

df111$Quantidade=df111$cases

df111=ungroup(df111)
df222=ungroup(df222)
# UI
ui <- navbarPage("Covid-19",theme = shinytheme("darkly"),
                 
                 navbarMenu("Mundo",
                            tabPanel("Diária", 
                                     titlePanel("Análise comparativa de casos e mortes diária por Covid-19 no mundo"), 
                                     mainPanel(p("O banco de dados foi obtido pelo site: ",
                                                 a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                                               p("Atualizado em:",df1$dateRep[1])
                                     ),
                                     column(
                                       6,fluidRow(column(6, selectizeInput("Alll", "Dados sobre os casos diários", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                                                           options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
                                                  column(6, selectizeInput("Alll2", "Dados sobre as mortes diárias", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                                                           options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
                                     ),
                                     column(
                                       12,fluidRow(column(12, plotlyOutput('Total'))
                                       )
                                     ),
                                     mainPanel(
                                       p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
                            ),
                 
                 tabPanel("Incidência", 
                titlePanel("Análise comparativa de incidências e mortalidades por Covid-19 no mundo"), 
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
    12,fluidRow(column(12, plotlyOutput('Incidencia'))
    )
  ),
  mainPanel(
    p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
),

tabPanel("Acumulativo", 
         titlePanel("Análise comparativa de casos e mortes acumuladas por Covid-19 no mundo"), 
         mainPanel(p("O banco de dados foi obtido pelo site: ",
                     a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                   p("Atualizado em:",df1$dateRep[1])
         ),
         column(
           6,fluidRow(column(6, selectizeInput("Allll", "Dados sobre os casos diários", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                               options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
                      column(6, selectizeInput("Allll2", "Dados sobre as mortes diárias", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                               options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
         ),
         column(
           12,fluidRow(column(12, plotlyOutput('Acumulado'))
           )
         ),
         mainPanel(
           p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
),

tabPanel("Dados",
         DT::dataTableOutput("table"),

       )

),

navbarMenu("Brasil",
tabPanel("Acumulativo",
         titlePanel("Análise comparativa de casos e mortes acumuladas por Covid-19 no Brasil"), 
         mainPanel(p("O banco de dados foi obtido pelo site: ",
                     a(href="https://github.com/liibre/coronabr/blob/master/R/get_corona_minsaude.R", "GitHub")),
                   p("Atualizado em:",max(cidade$data2))
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
           p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
         ),
tabPanel("Dados",
         DT::dataTableOutput("table2")
)
),





           tabPanel("Sobre",
                    
                    titlePanel("Site"),
                    p("COVID-19 é uma doença infeciosa causada pelo coronavírus, seus principais sintomas são febre, tosse seca e cansaço.",br(), 
                      "O site tem como objetivo mostrar as pessoas o avanço do Covid-19 ao decorrer dos dias, através de tabelas e gráficos interativos suportados pelo",
                      a(href="https://shiny.rstudio.com", "Shiny,"),
                      "um dos vários pacotes do ",
                      a(href="https://www.r-project.org", "R"),br(),br(),p("Cálculo da incidência : (casos * 100000)/(População)"),
                      p("Cálculo da mortalidade : (mortes * 100000)/(População)")),

                    
                    titlePanel("Criador"),
                    strong("Alessandro Pereira Torres De Sá Neto"),
                    br(),
                    mainPanel(p("Graduando em estatística na ",
                                a(href="https://ufrn.br", "UFRN"),
                                " com experiência em consultoria estatística no",
                                a(href="http://lea.estatistica.ccet.ufrn.br", "LEA"),
                                "e criação de jogos digitais pelo ",a(href="https://imd.ufrn.br/portal/", "IMD,"),
                                "possui conhecimento nas áreas de Estatística, Probabilidade, Matemática, Machine Learning, Big Data, Informática e Programação em:"),
                              p("-R",br(),"-Python",br(),"-Markdown e LaTeX",br(),"-Excel",br(),"-C++ e C#"),
                              br(),
                              strong("GitHub: ",a(href="https://github.com/AlessandroPTSN/Covid-19", "https://github.com/AlessandroPTSN/Covid-19")),
                              br(),
                              strong("Email: alessandroptsn@yahoo.com.br" ))
           )
)






# Server code
server <- function(input, output) {
  
  
  outVarr <- reactive({
    df11 %>%
      filter(countriesAndTerritories %in% input$Alll) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVarr2 <- reactive({
    df22 %>%
      filter(countriesAndTerritories %in% input$Alll2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })

  output$Total <- renderPlotly({
    plot_ly(data=outVarr(), x=~Data,  y = ~Quantidade,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVarr2(), x=~Data,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'h',y = 100, x = 0))
  })
  
  outVar <- reactive({
    df1 %>%
      filter(countriesAndTerritories %in% input$All) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVar2 <- reactive({
    df2 %>%
      filter(countriesAndTerritories %in% input$All2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$Incidencia <- renderPlotly({
    plot_ly(data=outVar(), x=~Data,  y = ~Quantidade,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVar2(), x=~Data,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'h',y = 100, x = 0))
  })
  
  
  outVarrr <- reactive({
    df111 %>%
      filter(countriesAndTerritories %in% input$Allll) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVarrr2 <- reactive({
    df222 %>%
      filter(countriesAndTerritories %in% input$Allll2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$Acumulado <- renderPlotly({
    plot_ly(data=outVarrr(), x=~Data,  y = ~Quantidade,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVarrr2(), x=~Data,  y = ~deaths,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'h',y = 100, x = 0))
  })
  
  
  
  outVara <- reactive({
    cidade %>%
      filter(countriesAndTerritories %in% input$All11) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos acumulados", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVara2 <- reactive({
    cidade2 %>%
      filter(countriesAndTerritories %in% input$All22) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes acumuladas", sep = " ")) %>% 
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
      layout(legend = list(orientation = 'h',y = 100, x = 0))         
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(tabela1,style = 'bootstrap',options = list(
      initComplete = JS(
        "function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': '#000',
        'color': '#fff'
        }); 
        }")
    ))
  })
  
  output$table2 <- DT::renderDataTable({
    DT::datatable(tabela2,style = 'bootstrap',options = list(
      initComplete = JS(
        "function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': '#000',
        'color': '#fff'
        }); 
        }")

    ))
  })
  

  
}

shinyApp(ui = ui, server = server)
