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
#shinyApp(ui = ui, server = server)