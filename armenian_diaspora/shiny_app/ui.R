library("leaflet")

# Choices for drop-downs
color_vars = c(
  "Country" = "country",
  "City or Region" = "city_or_region"
)

size_vars = c(
  "Official Data" = "official_data",
  "Lower Estimate" = "lower_estimate",
  "Upper Estimate" = "upper_estimate",
  "Alternative Estimate 1" = "estimate_1",
  "Alternative Estimate 2" = "estimate_2"
)

choices_area = unique(df$area)
choices_city_region = unique(df$city_or_region)
choices_country = unique(df$country)

navbarPage(h5(strong("Armenians in Diaspora")), id="nav",
           
           tabPanel(h5("Diaspora Map"),
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Location Explorer"),
                                      
                                      selectInput("color", h5("Color"), color_vars, selected = "country"),
                                      selectInput("size", "Size", size_vars, selected = "official_data"),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterPopulation", height = 250)
                        ),

                        tags$div(id="cite",
                                 'Data compiled from Wikipedia: ', tags$em('Largest Armenian Diaspora Communities.'), 'See here: https://en.wikipedia.org/wiki/Largest_Armenian_diaspora_communities'
                        )
                    )
           ),
           
           tabPanel(h5("Explore the Data"),
                    fluidRow(
                      column(3,
                             selectInput("country", "Country", choices_country, multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.country",
                                              selectInput("area", "Community", choices_area, multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.country",
                                              selectInput("city_or_region", "City or Region", choices_city_region, multiple=TRUE)
                             )
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("fulltable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)