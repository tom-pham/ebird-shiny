# Load latest taxonomy and sace
tax <- rebird::ebirdtaxonomy()

tax <- tax %>% 
  select(sciName, comName, speciesCode)

readr::write_csv(tax, 'taxonomy.csv')