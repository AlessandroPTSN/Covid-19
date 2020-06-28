# Server code
server <- function(input, output) {

  
#####################################
  outVarrB <- reactive({
    df11 %>%
      filter(countriesAndTerritories %in% input$Alll) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVarr2B <- reactive({
    df22 %>%
      filter(countriesAndTerritories %in% input$Alll2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$Total2 <- renderPlotly({
    plot_ly(data=outVarrB(), x=~Data,  y = ~QuantidadeMedia,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVarr2B(), x=~Data,  y = ~deaths2,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'h',y = 100, x = 0))
  })
#####################################  
  
  
  

  
  
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

  
  
  
  #####################################
  
  outVark <- reactive({
    df1 %>%
      filter(countriesAndTerritories %in% input$All) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "casos", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  outVar2k <- reactive({
    df2 %>%
      filter(countriesAndTerritories %in% input$All2) %>%
      mutate(countriesAndTerritories = paste(countriesAndTerritories, "mortes", sep = " ")) %>% 
      arrange(Data) %>%
      droplevels()
  })
  
  output$Incidencia2 <- renderPlotly({
    plot_ly(data=outVark(), x=~Data,  y = ~QuantidadeMedia,
            type = 'scatter', mode = 'lines', legendgroup = "1",
            color = ~countriesAndTerritories,colors = "viridis") %>%
      add_trace(data=outVar2k(), x=~Data,  y = ~deaths2,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~countriesAndTerritories,colors = "viridis")  %>%
      layout(legend = list(orientation = 'h',y = 100, x = 0))
  })
  ##########################################
  
  
  
  

  
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
# Return a Shiny app object
#shinyApp(ui = ui, server = server)
