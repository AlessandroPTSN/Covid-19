# UI
ui <- navbarPage("Covid-19",theme = shinytheme("darkly"),
                 
                 navbarMenu("Mundo",
                            tabPanel("Diária", 
                                     titlePanel("Análise comparativa de casos e mortes diária por Covid-19 no mundo"), 
                                     mainPanel(p("O banco de dados foi obtido pelo site: ",
                                                 a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                                               p("Atualizado em:",df1$Data[1])
                                     ),
                                     column(
                                       6,fluidRow(column(6, selectizeInput("Alll", "Dados sobre os casos diários", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                                                           options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
                                                  column(6, selectizeInput("Alll2", "Dados sobre as mortes diárias", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                                                           options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
                                     ),

                                     checkboxInput(inputId = "smoother", label = strong("Média móvel"), value = FALSE),
                                     
                                     conditionalPanel(condition = "input.smoother == true",
                                                      
                                                      column(
                                                        12,fluidRow(column(12, plotlyOutput("Total2"))
                                                        )
                                                      ),
                                     ),
                                                      conditionalPanel(condition = "input.smoother == false",
                                                                       
                                                                       column(
                                                                         12,fluidRow(column(12, plotlyOutput("Total"))
                                                                         )
                                                                       ),
                                     ),
                                     mainPanel(
                                       p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
                            ),
                 
                 tabPanel("Incidência", 
                titlePanel("Análise comparativa de incidências e mortalidades por Covid-19 no mundo"), 
                mainPanel(p("O banco de dados foi obtido pelo site: ",
                          a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                          p("Atualizado em:",df1$Data[1])
                          ),
  column(
    6,fluidRow(column(6, selectizeInput("All", "Dados sobre os casos diários", multiple = T,choices = unique(df1$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))),
               column(6, selectizeInput("All2", "Dados sobre as mortes diárias", multiple = T,choices = unique(df2$countriesAndTerritories), 
                                        options = list(maxItems = 5, placeholder = 'Escolha os locais:'))))
  ),
  
  
  checkboxInput(inputId = "smoother2", label = strong("Média móvel"), value = FALSE),
  
  conditionalPanel(condition = "input.smoother2 == true",
                   
                   column(
                    12,fluidRow(column(12, plotlyOutput("Incidencia2"))
                     )
                   ),
  ),
  conditionalPanel(condition = "input.smoother2 == false",
                   
                   column(
                     12,fluidRow(column(12, plotlyOutput("Incidencia"))
                     )
                   ),
  ),
  
  
  mainPanel(
    p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
),

tabPanel("Acumulativo", 
         titlePanel("Análise comparativa de casos e mortes acumuladas por Covid-19 no mundo"), 
         mainPanel(p("O banco de dados foi obtido pelo site: ",
                     a(href="https://opendata.ecdc.europa.eu", "European Centre for Disease Prevention and Control")),
                   p("Atualizado em:",df1$Data[1])
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
                     a(href="https://brasil.io/dataset/covid19/caso_full/", "Brasil.io")),
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
