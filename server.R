shinyServer(
  function(input, output) {
    
    data_input <- reactive({id_stat %>% select("id", "GYV_SK", "LAIM") %>% filter(LAIM == input$selectas)})
    data_input_ordered <- reactive({as.data.frame(data_input()) %>% filter(GYV_SK > min(input$slider2)) %>% filter(GYV_SK < max(input$slider2))})
    
    marker_input <- reactive({cnames4 %>% filter(GYV_SK > min(input$slider2)) %>% filter(GYV_SK < max(input$slider2))})
    stat_input <- reactive({id_stat %>% select("id", input$selectas1, input$selectas2, "RESULT")})
    
    output$summary_table1 <- renderDataTable(data_input_ordered())
    output$summary_table2 <- renderDataTable(stat_input())
    output$summary_table3 <- renderDataTable(id_stat1)
    output$summary_table4 <- renderDataTable(id_stat1)
    
    output$mymap1 <- renderLeaflet({
      leaflet(spdf1) %>%
        addTiles() %>%
        addMarkers(lng=marker_input()$long, lat=marker_input()$lat, popup=marker_input()$id) %>%
        addLegend(pal = dd_pal, values = ~LAIM, opacity = 0.7, title = "Laimėjusių kandidatų trumpiniai", position = "bottomright") %>%
        addPolygons(
          fillColor = ~dd_pal(LAIM),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))})
    
    output$mymap2 <- renderLeaflet({
      leaflet(spdf2) %>%
        addTiles() %>%
        addLegend(pal = e_pal, values = ~RESULT, opacity = 0.7, title = "Kandidatu trumpiniai", position = "bottomright") %>%
        addPolygons(
          fillColor = ~e_pal(RESULT),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))})
    
    output$mymap3 <- renderLeaflet({
      leaflet(spdf2) %>%
        addTiles() %>%
        addLegend(pal = e_pal, values = ~RESULT, opacity = 0.7, title = "Kandidatu trumpiniai", position = "bottomright") %>%
        addPolygons(
          fillColor = ~e_pal(RESULT),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))})
    
    output$mymap4 <- renderLeaflet({
      leaflet(spdf3) %>%
        addTiles() %>%
        addLegend(pal = p_pal, values = ~GYV_SK, opacity = 0.7, title = "Kandidatu trumpiniai", position = "bottomright") %>%
        addPolygons(
          fillColor = ~p_pal(GYV_SK),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))})
    })