library(rebird)
library(leaflet)
library(leaflet.minicharts)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(lubridate)
library(plotly)
library(rvest)


EBIRD_KEY <- readRDS('EBIRD_KEY.rds')

# Get species code
species_code('hydroprogne caspia')

cali <- ebirdregion(loc = 'US-CA', key = EBIRD_KEY, back = 30)

# Get observations in past 30 days in California
cate <- ebirdregion(loc = 'US-CA', species = 'caster1', key = EBIRD_KEY, back = 30)
notable <- rebird::ebirdnotable(lat = 36.96, lng = -122.01, dist = 15, 
                                key = EBIRD_KEY)

# Map observations
leaflet(data = cate) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircleMarkers(~lng, ~lat, 
                   popup = ~as.character(locName), 
                   label = ~as.character(locName),
                   radius = 1) %>% 
  addMiniMap(tiles = providers$CartoDB.DarkMatter)

# convert ebd to spatial object
cate_sf <- cate %>% 
  select(comName, lat, lng) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 26910)

# get state boundaries using rnaturalearth package
states <- ne_states(iso_a2 = c("US", "CA"), returnclass = "sf")

ca <- filter(states, postal == "CA") %>% 
  st_geometry()

# map
par(mar = c(0, 0, 0, 0), bg = "skyblue")

# set plot extent
plot(ca, col = NA)

# add state boundaries
plot(states %>% st_geometry(), col = "grey40", border = "white", add = TRUE)
plot(ca, col = "grey20", border = "white", add = TRUE)

# ebird data
plot(cate_sf, col = "#4daf4a99", pch = 19, cex = 0.75, add = TRUE)

# Get list of taxonomy common names, scientific names, species code
taxonomy <- ebirdtaxonomy(key = EBIRD_KEY)

taxonomy <- taxonomy %>% 
  select(sciName, comName, speciesCode)

write_csv(taxonomy, "taxonomy.csv")

state_centers <- read_csv("state_centers.csv")


# Bubble plot animation ---------------------------------------------------
# time series animation of max observations of a species per location per day
locs <- cate %>% 
  group_by(locName) %>% 
  summarise(
    lat = mean(lat),
    lng = mean(lng)
  )

obs <- cate %>% 
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


# Create a grid of all receiver general locations by all dates from the earliest detection date to the 
# latest detection date
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

leaflet(data = timestep, width = "100%", height = "800px") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  # setView(mean(timestep$lat), mean(timestep$lng), 7) %>%
  addMinicharts(
    lat = timestep$lat,
    lng =  timestep$lng,
    chartdata = timestep$max_obs,
    time = timestep$date,
    fillColor = "blue",
    width = 60, height = 60,
    # transitionTime = 2000,
    popup = popupArgs(
      showValues = FALSE,
      supValues = timestep %>% select(locName, max_obs),
      supLabels = c("Location", "N = ")
    ),
    showLabels = TRUE,
    opacity = .7
  )


# Notable observations  ----------------------------------------------------------
locs <- notable %>% 
  group_by(locName) %>% 
  summarise(
    lat = mean(lat),
    lng = mean(lng)
  )

# For each location, count the number of unique species
notable_sum <- notable %>% 
  select(locName, comName) %>% 
  distinct() %>% 
  group_by(locName) %>% 
  count() %>% 
  left_join(
    locs
  )


leaflet(data = notable_sum, width = "100%", height = "800px") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  setView(lng = mean(notable_sum$lng), lat = mean(notable_sum$lat), zoom = 11) %>%
  addCircleMarkers(
    radius = ~ sqrt(n),
    lng = ~lng,
    lat = ~lat,
    popup = ~as.character(n),
    label = ~locName
  )

# Plot notable locations by # unique species
notable_sum <- notable_sum %>% 
  arrange(desc(n)) %>% 
  mutate(
    locName = factor(locName, levels = notable_sum$locName)
  ) 

levels(notable_sum$locName) <- notable_sum$locName
  
  
plot_ly(
  x = ~locName,
  y = ~n,
  type = "bar"
)



# Webscraping with Rvest --------------------------------------------------
birdcode <- "acafly"
url <- paste0("https://ebird.org/species/", birdcode)

start <- Sys.time()
imgsrc <- read_html(url) %>%
  html_node(xpath = '//*/img') %>%
  html_attr('src')
end <- Sys.time()


# Retrieve bird description

# Get list of metatag tags 
metatags <- read_html(url) %>% 
  html_nodes('meta') %>% 
  html_attr('name')

# Get which row has the description
rownum <- which(metatags == "description")

content <- read_html(url) %>% 
  html_nodes('meta') %>% 
  html_attr('content') 

content[rownum]



#---
url <- "https://ebird.org/species/y00324"
imgsrc <- read_html(url) %>%
  html_node(xpath = '//*/img') %>%
  html_attr('srcset')

download.file(imgsrc, "test.jpeg")


# Get all US State Region Codes -------------------------------------------
states <- c("Alabama","Alaska","Arizona",'Arkansas', 
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
            "Wisconsin","Wyoming")

regioncode <- lapply(states, pick_UScode, county = NULL, ebirdkey = "plu0uejrs3vn") 
regioncode <- regioncode %>% 
  bind_rows()



# Visualize bird observation frequency by date ----------------------------

# Get species code
species_code('hydroprogne caspia')

cali <- ebirdregion(loc = 'US-CA', key = EBIRD_KEY, back = 30)

dates <- cate %>% 
  mutate(obsDt = as.Date(ymd_hm(obsDt))) %>% 
  group_by(obsDt) %>% 
  count()

plot <- ggplot(data = dates, mapping = aes(x = obsDt, y = n)) +
  geom_col() +
  scale_x_date(
    date_labels = '%m/%d', date_breaks = '1 day',
    expand = c(0,0)
  ) +
  scale_y_continuous(expand = c(0,0)) +
  ylab('Observation Frequency') +
  xlab('Date') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  )
