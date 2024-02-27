library(tidyverse)
library(magrittr)
library(sf)
library(patchwork)
library(viridis)
library(MetBrewer)


mers <- read_sf("~/Downloads/mers-spill.shp")
merspts <- read_csv("~/Downloads/MERSCoV_dataset_SciData_9.20.19.csv")

merspts %<>% filter(organism_type == "human",
                    transmission_route == "zoonotic")

mers %>%
  ggplot((aes(fill = spillover))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mersspill

merspts %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_point(pch = 21, color = 'white', fill = 'darkblue', size = 2) + 
  theme_void()  + 
  xlim(21, 63.5) + 
  ylim(-2, 40)  -> mersptmap

mersspill + 
  annotation_custom(ggplotGrob(mersptmap))

##

nipah <- read_sf("~/Downloads/batpoly.shp.shp")
nipts <- read_csv("~/Downloads/experiment1_all_henipaviruses_2019-07-15.csv")

nipts %<>% filter(organism_type == "human", 
                  pathogen == "nipah virus")

nipah %>%
  ggplot((aes(fill = BATS/max(BATS)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> nispill

nipts %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_point(pch = 21, color = 'white', fill = 'darkblue', size = 2) + 
  theme_void() + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> niptmap

nispill + 
  annotation_custom(ggplotGrob(niptmap))
