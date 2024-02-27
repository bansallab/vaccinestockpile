
library(tidyverse)
library(magrittr)
library(sf)
library(patchwork)
library(viridis)
library(MetBrewer)
library(scales)

mapn <- read_sf("~/Downloads/sixlayer.shp")

mapn %>%
  ggplot((aes(fill = BATS/max(BATS)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni1; ni1

mapn %>%
  ggplot((aes(fill = BATPTS/max(BATPTS)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni2; ni2

mapn %>%
  ggplot((aes(fill = DEFORES/max(DEFORES)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni3; ni3

mapn %>%
  ggplot((aes(fill = TREECOV/max(TREECOV)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni4; ni4

mapn %>%
  ggplot((aes(fill = TOTALFR/max(TOTALFR)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni5; ni5

mapn %>%
  ggplot((aes(fill = TOTALOI/max(TOTALOI)))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(68, 98) + 
  ylim(6.5, 36) -> ni6; ni6

ni1 + ni2 + ni3 + ni4 + ni5 + ni6 + 
  plot_layout(ncol = 2) + plot_annotation(tag_levels = "A")

##

mers <- read_sf("~/Downloads/mers-spill.shp")

mers %>%
  ggplot((aes(fill = aglog))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mers1; mers1

mers %>%
  ggplot((aes(fill = camellog))) + 
  geom_sf(lwd = 0) + 
  theme_void() + 
  scale_fill_gradientn(colors=rev(met.brewer("Hokusai1")))+ 
  labs(fill = 'Risk')  + 
  xlim(21, 63.5) + 
  ylim(-2, 40) -> mers2; mers2


mers1 + mers2 + 
  plot_layout(ncol = 2) + plot_annotation(tag_levels = "A")
