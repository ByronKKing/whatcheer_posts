library("leaflet")
library("RColorBrewer")
library("scales")
library("lattice")
library("dplyr")

set.seed(100)

function(input, output, session) {
  
  ## Interactive Map
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -39.37919, lat = 19.18161, zoom = 3)
  })
  
  # A reactive expression that returns the set of zips that are in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(df[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(df,
  #          lat >= latRng[1] & lat <= latRng[2] &
  #            lon >= lngRng[1] & lon <= lngRng[2])
  # })
  
  ### Plots
  # 
  # # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, df$upper_estimate, breaks = 20)$breaks
  # 
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   hist(zipsInBounds()$upper_estimate,
  #        breaks = centileBreaks,
  #        main = "Optimal Location Score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(df$upper_estimate),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  # 
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   print(xyplot(lower_estimate ~ upper_estimate, data = zipsInBounds(), xlim = range(df$upper_estimate), ylim = range(df$lower_estimate)))
  # })
  # 
  ## Observe Functions
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    df = df
    
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- df[[colorBy]]
    pal <- colorFactor("viridis", colorData)
    
    radius <- df[[sizeBy]] / max(df[[sizeBy]],na.rm = TRUE) * 500000
    
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius=radius, layerId=~area,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(area, lat, lng) {
    selectedZip <- df[df$area == area,]
    content <- as.character(tagList(
      tags$h4("Selected Population Estimate:", as.integer(selectedZip$upper_estimate)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$area, selectedZip$region, selectedZip$country
      ))), tags$br()
      # ,
      # sprintf("Median Household Income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      # sprintf("Competition Proximity Score: %s%%", as.integer(selectedZip$competition_proximity_score)), tags$br(),
      # sprintf("Adult Population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = area)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  # 
  # observe({
  #   areas <- if (is.null(input$name)) character(0) else {
  #     filter(df, area %in% input$name) %>%
  #       `$`('area') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$name[input$name %in% areas])
  #   updateSelectInput(session, "areas", choices = areas,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   countries <- if (is.null(input$country)) character(0) else {
  #     df %>%
  #       filter(country %in% input$country,
  #              is.null(input$country) | country %in% input$country) %>%
  #       `$`('country') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$country[input$country %in% countries])
  #   updateSelectInput(session, "countries", choices = countries,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lon
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  # 
  # output$ziptable <- DT::renderDataTable({
  #   df <- df %>%
  #     filter(
  #       # Optimal_Location_Score >= input$minScore,
  #       # Optimal_Location_Score <= input$maxScore,
  #       is.null(input$area) | area %in% input$area,
  #       is.null(input$country) | country %in% input$country
  #       # ,
  #       # is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '" data-zip="', country, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df)
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })

}