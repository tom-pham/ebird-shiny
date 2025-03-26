library(shiny)
library(tidyverse)
library(data.table)
library(rebird)
library(leaflet)
library(leaflet.minicharts)
library(lubridate)
library(shinythemes)
library(rvest)
library(tigris)
library(httr)
library(sf)
library(DT)

# Load some data
taxonomy <- fread(file.path("data/taxonomy.csv"))
state_centers <- fread(file.path("data/state_centers.csv"))
states_shp <- read_sf(file.path("data/states_shp.shp"))
states_shp <- st_transform(states_shp, crs = 4326)

regionCodeUS <- fread(file.path("data/regionCodeUS.csv"))
EBIRD_KEY <- readRDS(file.path("data/EBIRD_KEY.rds"))

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$style(HTML("
    .selected { background-color: #FFD700 !important; } /* Highlight color */
  ")),
  navbarPage("eBird Species Occurence",
             tabPanel("Recent Observations",
                      titlePanel("Species observed in the past 30 days"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "stateInput",
                                      label = "Select a state:",
                                      choices = c("Alabama","Alaska","Arizona",'Arkansas', 
                                                  "California","Colorado","Connecticut","Delaware",
                                                  "Florida","Georgia","Hawaii","Idaho",'Illinois',
                                                  "Indiana","Iowa","Kansas","Kentucky",'Louisiana',
                                                  "Maine","Maryland","Massachusetts","Michigan",
                                                  "Minnesota","Mississippi","Missouri","Montana",
                                                  "Nebraska","Nevada","New Hampshire","New Jersey",
                                                  "New Mexico","New York","North Carolina",
                                                  "North Dakota","Ohio","Oklahoma","Oregon",
                                                  "Pennsylvania","Rhode Island","South Carolina",
                                                  "South Dakota","Tennessee","Texas","Utah","Vermont",
                                                  "Virginia","Washington","West Virginia",
                                                  "Wisconsin","Wyoming")),
                          selectInput(inputId = "speciesInput",
                                      label = "Select a species:",
                                      choices = ""
                                      
                          ),
                          radioButtons("radio_maptype", label = h4("Choose"),
                                       choices = list("Static" = 1, "Animated" = 2), 
                                       selected = 1),
                          htmlOutput("datesObserved"),
                        ),
                        mainPanel(
                          leafletOutput("mymap",
                                        width = "75%",
                                        height = 600)
                        )
                      )
             ),
             tabPanel("Notables Nearby",
                      tags$script('
                          $(document).ready(function () {
                            navigator.geolocation.getCurrentPosition(onSuccess, onError);
                                  
                            function onError (err) {
                              Shiny.onInputChange("geolocation", false);
                            }
                                  
                            function onSuccess (position) {
                              setTimeout(function () {
                                var coords = position.coords;
                                console.log(coords.latitude + ", " + coords.longitude);
                                Shiny.onInputChange("geolocation", true);
                                Shiny.onInputChange("lat", coords.latitude);
                                Shiny.onInputChange("long", coords.longitude);
                              }, 1100)
                            }
                          });
                      '),
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Allow the browser to provide your location to see notable sightings near you!"),
                          uiOutput("notable_bird_description"),
                        ),
                        mainPanel(
                          leafletOutput("near_me_map",
                                        width = "75%",
                                        height = 600),
                          fluidRow(verbatimTextOutput("near_me_map_marker_click")),
                          DTOutput("nearbyTable",
                                   width = "75%")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {

  # Updates species input selection based on state selected
  observe({
    updateSelectInput(session, "speciesInput",
                      choices = spList()
    )
  })
  
  spList <- reactive({
    # Get ebird region code

    regioncode <- regionCodeUS %>% 
      filter(name == input$stateInput) %>% 
      pull(code)
    
    # Get observations in past 30 days in California
    ebirdData <- ebirdregion(loc = regioncode, key = EBIRD_KEY,
                             back = 30)
    
    # Unique bird common names
    birdList <- unique(ebirdData$comName)
    
    # Return sorted list of species for given state, add "" blank value to 
    # the top so nothing is selected by default
    c("", sort(birdList))
  })
  
  observations <- reactive({
    
    req(input$speciesInput)
    
    # Get species code
    spcode <-  taxonomy$speciesCode[taxonomy$comName == input$speciesInput]
    
    # Get ebird region code
    regioncode <- regionCodeUS %>% 
      filter(name == input$stateInput) %>% 
      pull(code)
    
    # Get observations in past 30 days in California
    ebird_data <- ebirdregion(loc = regioncode, species = spcode, key = EBIRD_KEY,
                              back = 30)

    ebird_data
  })  
  
  # Retrieve notable observations nearby 
  notable_near <- reactive({

    req(input$lat)
    
    # Get notable observations given user lat/lon
    obs <- ebirdnotable(lat = input$lat, lng = input$long, key = EBIRD_KEY) %>% 
      mutate(obsDt = ymd_hm(obsDt))
    
    obs$uid <- 1:nrow(obs)
    obs
  })  
  
  time_step_obs <- reactive({
    # time series animation of max observations of a species per location per day
    locs <- observations() %>% 
      group_by(locName) %>% 
      summarise(
        lat = mean(lat),
        lng = mean(lng)
      )
    
    obs <- observations() %>% 
      mutate(
        obsDt = ymd_hm(obsDt),
        date = as.Date(obsDt)
      ) %>% 
      group_by(locName, date) %>% 
      summarise(
        max_obs = max(howMany)
      ) %>% 
      left_join(
        locs
      ) %>% 
      filter(
        !is.na(max_obs),
        !is.na(date)
      )
    
    # Create a grid of all locations by all dates from the earliest date to the 
    # latest date
    timestep <- expand.grid(obs$locName, seq(min(obs$date), max(obs$date), by = 1),
                            stringsAsFactors = F)
    
    colnames(timestep) <- c("locName", "date")
    
    # Summarise detections counts by GEN and date and join into the grid
    timestep <- timestep %>% 
      left_join(
        obs %>% 
          select(locName, date, max_obs)
      ) %>% 
      mutate(
        max_obs = ifelse(is.na(max_obs), 0, max_obs)
      ) %>% 
      left_join(
        locs, by = "locName"
      )
  })
  
  output$mymap <- renderLeaflet({
    
    # No species selected yet, center on state
    if (input$speciesInput == '') {
      
      leaflet()  %>% 
        addPolygons(
          data = filter(states_shp, NAME == input$stateInput),
          color = 'red',
          weight = 1,
          fillColor = 'black',
          fillOpacity = 0) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
        addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>% 
        setView(
          lng = state_centers[State == input$stateInput, Lon],
          lat = state_centers[State == input$stateInput, Lat],
          zoom = state_centers[State == input$stateInput, zoom]) %>%
        addLayersControl(
          baseGroups = c("Dark", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addMiniMap(tiles = providers$CartoDB.DarkMatter)
      
    } else if (is_empty(observations())) {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
        addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>%
        setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>% 
        addMiniMap(tiles = providers$CartoDB.DarkMatter) %>% 
        addLayersControl(
          baseGroups = c("Dark", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else {

      if (input$radio_maptype == "1") {
        leaflet(data = observations()) %>% 
          addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
          addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>%
          addMiniMap(tiles = providers$CartoDB.DarkMatter) %>% 
          addCircleMarkers(~lng, ~lat, 
                           popup = ~as.character(locName), 
                           label = ~as.character(locName),
                           radius = 1) %>% 
          addLayersControl(
            baseGroups = c("Dark", "Terrain"),
            options = layersControlOptions(collapsed = FALSE)
          )
      } else { # "Animated
        timestep <- time_step_obs()
        leaflet(data = timestep, width = "100%", height = "800px") %>%
          addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
          addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>%
          addMinicharts(
            lat = timestep$lat,
            lng =  timestep$lng,
            chartdata = timestep$max_obs,
            time = timestep$date,
            fillColor = "blue",
            width = 60, height = 60,
            # transitionTime = 750,
            popup = popupArgs(
              showValues = FALSE,
              supValues = timestep %>% select(locName, max_obs),
              supLabels = c("Location", "N = ")
            ),
            showLabels = TRUE,
            opacity = .7
          ) %>% 
          addLayersControl(
            baseGroups = c("Dark", "Terrain"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% 
          addMiniMap(tiles = providers$CartoDB.DarkMatter)
      }
    }
  })
  
  # Print out the earliest date of observations
  output$datesObserved <- renderUI({
    req(input$speciesInput)
    spcode <- taxonomy$speciesCode[taxonomy$comName == input$speciesInput]
    url <- paste0("https://ebird.org/species/", spcode)

    imgsrc <- read_html(url) %>%
      html_nodes('meta[property="og:image"]') %>%
      html_attr('content')
    
    # Retrieve the description for the given bird species. If none exists
    # return message saying no description available
    descript <- tryCatch(read_html(url) %>% 
                    html_nodes('p.u-stack-sm') %>% 
                    html_text() %>% 
                    .[[1]], error=function(e) print("No description available"))
    
    if (is_empty(observations())) {

      tags$div(class="header", checked=NA,
               tags$img(src = imgsrc, width = "100%"),
               tags$h3("Description"),
               tags$p(descript),
               tags$br(),
               tags$h4("Date range"),
               tags$p(paste0("No observations of ", input$speciesInput, " have been observed in ",
                             input$stateInput, " in the past 30 days."))
      )
    }else {
      min_date <- observations() %>% 
        select(obsDt) %>% 
        mutate(obsDt = as.Date(ymd_hm(obsDt))) %>% 
        filter(!is.na(obsDt)) %>% 
        pull()
      
      max_date <- observations() %>% 
        select(obsDt) %>% 
        mutate(obsDt = as.Date(ymd_hm(obsDt))) %>% 
        filter(!is.na(obsDt)) %>% 
        pull()
      
      tags$div(class="header", checked=NA,
               tags$a(
                 href = paste0("https://ebird.org/species/", taxonomy[comName == input$speciesInput, speciesCode]),
                 style = "font-size: 20px;",  
                 input$speciesInput
               ),         
               tags$img(src = imgsrc, width = "100%"),
               tags$h3("Description"),
               tags$p(descript),
               tags$br(),
               tags$h4("Date range"),
               tags$p(paste0("Earliest observation: ", as.character(min(min_date)))),
               tags$p(paste0("Latest observation: ", as.character(max(max_date))))
      )
    }
  })

  output$near_me_map <- renderLeaflet({
    
    # If no records were retrieved display empty map centered on the contiguous US
    if (is_empty(notable_near())) {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
        addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>% 
        setView(lng = input$long, lat = input$lat, zoom = 10) %>% 
        addLayersControl(
          baseGroups = c("Dark", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addMiniMap(tiles = providers$CartoDB.DarkMatter)
      
    }else {
      leaflet(data = notable_near()) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
        addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>% 
        setView(lng = input$long, lat = input$lat, zoom = 10) %>% 
        addCircleMarkers(~lng, ~lat, 
                         layerId = ~uid,
                         popup = ~as.character(locName), 
                         label = ~as.character(locName),
                         radius = 1) %>% 
        addLayersControl(
          baseGroups = c("Dark", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addMiniMap(tiles = providers$CartoDB.DarkMatter)
    }
    
  })
  
  filtered_notable <- reactiveVal(NULL)
  
  observeEvent(input$near_me_map_marker_click, {
    p <- input$near_me_map_marker_click
    
    notable <- notable_near()
    loc_click = notable$locName[notable$uid == p$id]
    
    # Update the filtered_notable based on the location clicked
    filtered_notable(
      notable %>% 
        mutate(obsDt = as.character(as.Date(obsDt))) %>% 
        group_by(comName, locName, sciName, obsDt, .groups = "drop") %>% 
        summarise(maxCount = max(howMany)) %>% 
        filter(locName == loc_click) %>% 
        select(
          'Common name' = comName,
          'Scientific name' = sciName,
          Location = locName,
          'Observe Date' = obsDt,
          'Max observations' = maxCount
        ) %>% 
        arrange('Observe Date')
    )
  })
  
  output$nearbyTable <- renderDT({
    req(filtered_notable())  # Ensure it's not NULL
    
    filtered_notable <- filtered_notable()
    
    datatable(
      filtered_notable, 
      selection = 'single',
      options = list(
        dom = 't', # Removes unnecessary elements like search bar, if not needed
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'color': 'white'});", # Change all text to red
          "}"
        )
      )
    )
  })
  
  observeEvent(input$nearbyTable_rows_selected, {
  req(filtered_notable())  # Ensure data is not NULL
  
  # Reset the filtered_notable when switching locations
  if (length(input$nearbyTable_rows_selected) > 0) {
    # Get the currently selected row
    selected_row <- filtered_notable()[input$nearbyTable_rows_selected, ]
    species_name <- selected_row$`Common name`
    
    # Retrieve the species code for the selected bird
    spcode <- taxonomy$speciesCode[taxonomy$comName == species_name]
    
    # Get bird image and description
    url <- paste0("https://ebird.org/species/", spcode)
    
    imgsrc <- read_html(url) %>%
      html_nodes('meta[property="og:image"]') %>%
      html_attr('content')
    
    # Retrieve description for the selected bird species
    descript <- tryCatch(read_html(url) %>%
                           html_nodes('p.u-stack-sm') %>%
                           html_text() %>%
                           .[[1]], error = function(e) "No description available")
    
    # Render the bird description and image
    output$notable_bird_description <- renderUI({
      tags$div(class = "header", checked = NA,
               tags$a(
                 href = paste0("https://ebird.org/species/", spcode),
                 style = "font-size: 20px;",
                 species_name
               ),
               tags$img(src = imgsrc, width = "100%"),
               tags$h3("Description"),
               tags$p(descript)
      )
    })
  } else {
    # Reset the display when no rows are selected
    output$notable_bird_description <- renderUI({
      tags$p("Please select a location.")
    })
  }
})
  
}

shinyApp(ui = ui, server = server)
