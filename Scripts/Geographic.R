library(classInt)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(tidyverse)

# Read in the data

ticks <- read.csv('~/Github/tickmaps/TickMaps.csv')

# Pull out the iso codes column - this is what gets mapped

ticks %>% 
  as_tibble() %>% 
  select(ISO_code) %>% 
  mutate(ISO_code = strsplit(ISO_code, ',')) %>%
  unnest() %>%
  count(ISO_code) -> demo

demo$n <- factor(demo$n, levels=1:107)

# Attach that all to the map
sPDF <- joinCountryData2Map(demo,
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO_code")

x <- ggthemr('flat', set_theme = FALSE)

cols <- brewer.pal(11, "Spectral")
cols <- rev(colorRampPalette(cols)(107))

spplot(sPDF, 'n', col.regions = cols)

# Check for continentals

ticks %>% as_tibble() %>% 
  filter(ISO_code == '') %>% 
  select(ISO_country) %>% 
  mutate(ISO_country = str_split(ISO_country, ', ')) %>%
  unnest(ISO_country) %>% 
  count(ISO_country)
