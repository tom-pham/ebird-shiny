library(dplyr)
library(data.table)
library(rebird)

# Load latest taxonomy and sace
tax <- ebirdtaxonomy()

tax <- tax %>% 
  select(sciName, comName, speciesCode)

fwrite(tax, file.path('data', 'taxonomy.csv'))

states_shp <- states()
sf::write_sf(
  states_shp,
  file.path('data', 'states_shp.shp')
)
