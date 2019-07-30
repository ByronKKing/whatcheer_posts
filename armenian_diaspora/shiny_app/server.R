library("leaflet")
library("RColorBrewer")
library("scales")
library("lattice")
library("dplyr")

set.seed(100)

function(input, output, session) {
  
  # Create Map ###########################################
  
  ##map object
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -39.37919, lat = 19.18161, zoom = 3)
  })
  
  ## Create Plots
  
  ##reactive expression that returns the set of zips that are in bounds
  zipsInBounds = reactive({
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds = input$map_bounds
    latRng = range(bounds$north, bounds$south)
    lngRng = range(bounds$east, bounds$west)

    subset(df,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })

  output$histCentile = renderPlot({
    ##if no zipcodes in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    ##calculate breaks for histograms    
    centileBreaks = hist(plot = FALSE, zipsInBounds()$upper_estimate, breaks = 20)$breaks

    hist(zipsInBounds()$upper_estimate,
         breaks = centileBreaks,
         main = "Upper Population Estimates (visible locations)",
         xlab = "Percentile",
         xlim = range(centileBreaks),
         col = '#00DD00',
         border = 'white')
  })

  output$scatterPopulation = renderPlot({
    ##if no zipcodes in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(upper_estimate ~ lower_estimate, data = zipsInBounds(), xlim = range(df$lower_estimate), ylim = range(df$upper_estimate)))
  })

  
  ## Observe Functions
  
  ##maintains circles and legend based on user variables for color and size
  observe({
    
    df = df
    
    colorBy = input$color
    sizeBy = input$size
    
    colorData = df[[colorBy]]
    pal = colorFactor("viridis", colorData)
    
    radius = ifelse(df[[sizeBy]] / max(df[[sizeBy]],na.rm = TRUE) * 500000 > 5e+05, 350000,
                    ifelse(df[[sizeBy]] / max(df[[sizeBy]],na.rm = TRUE) * 500000 < 35000,50000,
                           df[[sizeBy]] / max(df[[sizeBy]],na.rm = TRUE) * 500000))
    
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius=radius, layerId=~area,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  ##show popup at given location
  showZipcodePopup = function(area, lat, lng) {
    selectedZip = df[df$area == area,]
    content = as.character(tagList(
      tags$h4(tags$strong(HTML(paste(selectedZip$area, selectedZip$country,sep = ", ")))),
      ifelse(is.na(selectedZip$upper_estimate),"",sprintf("Upper Estimate: %s", prettyNum(selectedZip$upper_estimate, big.mark=",", scientific = FALSE))), tags$br(),
      ifelse(is.na(selectedZip$lower_estimate),"",sprintf("Lower Estimate: %s", prettyNum(selectedZip$lower_estimate, big.mark=",", scientific = FALSE))), tags$br(),
      ifelse(is.na(selectedZip$official_data),"",sprintf("Official Data: %s", prettyNum(selectedZip$official_data, big.mark=",", scientific = FALSE))), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = area)
  }
  
  ##show popup with info when user clicks circle
  observe({
    leafletProxy("map") %>% clearPopups()
    event = input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  # Data Explorer ###########################################

  ##update data filters with selected choices
  observe({
    areas = if (is.null(input$country)) character(0) else {
      filter(df, country %in% input$country) %>%
        `$`('area') %>%
        unique() %>%
        sort()
    }
    stillSelected = isolate(input$area[input$area %in% areas])
    updateSelectInput(session, "area", choices = areas,
                      selected = stillSelected)
  })
  
  observe({
    city_region = if (is.null(input$country)) character(0) else {
      df %>%
        filter(country %in% input$country,
               is.null(input$area) | area %in% input$area) %>%
        `$`('city_or_region') %>%
        unique() %>%
        sort()
    }
    stillSelected = isolate(input$city_or_region[input$city_or_region %in% city_region])
    updateSelectInput(session, "city_or_region", choices = city_region,
                      selected = stillSelected)
  })

  ##go to clicked area
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map = leafletProxy("map")
      map %>% clearPopups()
      dist = 0.5
      area = input$goto$area
      lat = input$goto$lat
      lng = input$goto$lng
      showZipcodePopup(area, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  ##go to continent
  observe({
    if (input$continent_choice == ""){
      return()
    }else{
      df = df[df$continent == input$continent_choice,]
      new_lng = mean(df$lon)
      new_lat = mean(df$lat)    
    }
    
    isolate({
      map = leafletProxy("map")
      lat = new_lat
      lng = new_lng
      map %>% 
        setView(lng = new_lng, lat = new_lat, zoom = 4)
    })
  })

  ##table object
  output$fulltable = DT::renderDataTable({
    df = df %>%
      filter(
        is.null(input$area) | area %in% input$area,
        is.null(input$country) | country %in% input$country,
        is.null(input$city_or_region) | city_or_region %in% input$city_or_region
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '" data-area="', area, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action = DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

}