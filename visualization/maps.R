library(tidyverse)
library(magrittr)
library(sf)
library(patchwork)
library(viridis)
library(MetBrewer)

mapn <- read_sf("~/Downloads/Nipah_shapefile.shp")
mapm <- read_sf("~/Downloads/MERS_shapefile.shp")

dosesn <- read_csv("~/Downloads/df_ws1_adm2.csv")
dosesm <- read_csv("~/Downloads/df_ws1_adm1.csv")

## 

dosesn %<>%
  select(adm2, mean_all, max_all) 

mapn %>%
  st_as_sf() %>% 
  mutate(NAME_2 = tolower(NAME_2)) %>%
  left_join(dosesn %>% rename(NAME_2 = adm2)) -> dosemap.n

dosemap.n %>%
  ggplot((aes(fill = mean_all))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'rocket', direction = -1) + 
  labs(fill = 'Doses') -> g1

dosemap.n %>%
  ggplot((aes(fill = max_all))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'rocket', direction = -1) + 
  labs(fill = 'Doses') -> g2

## 

dosesm %<>%
  select(adm1, mean_all, max_all) 

mapm %>%
  st_as_sf() %>% 
  mutate(adm1 = tolower(adm1)) %>%
  left_join(dosesm) -> dosemap.m

dosemap.m %>%
  ggplot((aes(fill = mean_all))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'rocket', direction = -1) + 
  labs(fill = 'Doses') -> g3

dosemap.m %>%
  ggplot((aes(fill = max_all))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'rocket', direction = -1) + 
  labs(fill = 'Doses') -> g4

## 

(g1 + g2) / (g3 + g4) + plot_annotation(tag_levels = 'A')

################
################
################
################

mers <- read_sf("~/Downloads/mers-spill.shp")

mers %>%
  ggplot((aes(fill = spillover))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mersspill


nipah <- read_sf("~/Downloads/batpoly.shp.shp")

nipah %>%
  ggplot((aes(fill = BATS/max(BATS)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> nispill

##

merspop <- read_csv("~/Downloads/MERS_adm1_pop.csv")

mers %<>% left_join(merspop)

mers %>%
  ggplot((aes(fill = pop))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'mako', trans = "log", breaks = c(50000, 500000, 5000000, 50000000)) + 
  labs(fill = 'Population') + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mersmap

## 

merscon <- read_csv("~/Downloads/connectivity_mers.csv")

mers %>%
  st_centroid() %>%
  st_coordinates() %>% 
  bind_cols(mers$NAME_1) %>% 
  rename(lon = X, lat = Y, 'ADM1' = '...3') -> merspts

merscon %>%
  left_join(merspts, by = c('source_admin' = 'ADM1'), relationship = 'many-to-many') %>%
  rename(x = lon, y = lat) %>%
  left_join(merspts, by = c('dest_admin' = 'ADM1'), relationship = 'many-to-many') %>%
  rename(xend = lon, yend = lat) %>%
  select(x, y, xend, yend, mobility) -> merscon
  
ggplot(merscon) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = mobility),
             curvature = 0.33, alpha = 0.5, color = 'black') +
  scale_size_continuous(guide = FALSE, range = c(0.05, 2)) +  # scale for edge widths
  theme_void() +
  theme(legend.position = c(0.5, -0.1),
        legend.direction = "horizontal") + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mersnet

mersmap + 
  annotation_custom(ggplotGrob(mersnet)) -> mers2

##

nipop <- read_csv("~/Downloads/Nipah_adm2_pop.csv")

nipah %<>% select(-POPULATION) %>% left_join(nipop)

nipah %>%
  ggplot((aes(fill = POPULATION))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_viridis(option = 'mako', trans = "log", breaks = c(50000, 500000, 5000000, 50000000)) + 
  labs(fill = 'Population') + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> nimap

## 

nicon <- read_csv("~/Downloads/connectivity_nipah.csv")

nipah %>%
  st_centroid() %>%
  st_coordinates() %>% 
  bind_cols(nipah$NAME_2) %>% 
  rename(lon = X, lat = Y, 'ADM2' = '...3') %>%
  mutate(ADM2 = tolower(ADM2)) -> nipts

nicon %>%
  left_join(nipts, by = c('source_admin' = 'ADM2'), relationship = 'many-to-many') %>%
  rename(x = lon, y = lat) %>%
  left_join(nipts, by = c('dest_admin' = 'ADM2'), relationship = 'many-to-many') %>%
  rename(xend = lon, yend = lat) %>%
  select(x, y, xend, yend, mobility) -> nicon

ggplot(nicon) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = mobility),
             curvature = 0.33, alpha = 0.5, color = 'black') +
  scale_size_continuous(guide = FALSE, range = c(0.05, 2)) +  # scale for edge widths
  theme_void() +
  theme(legend.position = c(0.5, -0.1),
        legend.direction = "horizontal") + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ninet

nimap + 
  annotation_custom(ggplotGrob(ninet)) -> ni2

##

(nispill + ni2) / (mersspill + mers2) + plot_annotation(tag_levels = 'A')

