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
                            p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
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
                            p(strong("Observação: "),"Passe o cursor em cima do gráfico para visualizar melhor os dados",br(),strong("Criado por: "), a(href="https://github.com/AlessandroPTSN/Covid-19", "Alessandro Pereira Torres")))
                          
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
                                    strong("GitHub: ",a(href="https://github.com/AlessandroPTSN/Covid-19", "https://github.com/AlessandroPTSN/Covid-19")),
                                    br(),
                                    strong("Email: alessandroptsn@yahoo.com.br" )
                          )
                 )
                 
)