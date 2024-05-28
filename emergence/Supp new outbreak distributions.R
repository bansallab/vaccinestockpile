
library(tidyverse)
library(MetBrewer)

setwd("~/Downloads/Files")

ni.full <- read_csv("./Nipah/Full_vaccination/df_ws1_adm0.csv")
ni.full %>% 
  select(adm0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "Nipah", Model = "Strategy 3") -> ni.full

ni.hcw <- read_csv("./Nipah/HCW_vaccination_only/df_ws1_adm0.csv")
ni.hcw %>% 
  select(NAME_0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "Nipah", Model = "Strategy 1") -> ni.hcw

ni.no <- read_csv("./Nipah/No_catchup_vaccination/df_ws1_adm0.csv")
ni.no %>% 
  select(adm0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "Nipah", Model = "Strategy 2") -> ni.no

ni <- bind_rows(ni.full, ni.hcw, ni.no)
ni %>%
  ggplot(aes(x = Doses, group = Model, fill = Model)) + geom_histogram() + scale_y_log10() + theme_bw()


me.full <- read_csv("./MERS/Full_vaccination/df_ws1_adm0.csv")
me.full %>% 
  select(NAME_0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "MERS", Model = "Strategy 3") -> me.full

me.hcw <- read_csv("./MERS/HCW_vaccination_only/df_ws1_adm0.csv")
me.hcw %>% 
  select(NAME_0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "MERS", Model = "Strategy 1") -> me.hcw

me.no <- read_csv("./MERS/No_catchup_vaccination/df_ws1_adm0.csv")
me.no %>% 
  select(NAME_0, starts_with("Doses")) %>%
  pivot_longer(cols = starts_with("Doses"), names_to = 'Iteration', values_to = 'Doses') %>%
  group_by(Iteration) %>% summarize(Doses = sum(Doses)) %>%
  mutate(Disease = "MERS", Model = "Strategy 2") -> me.no

me <- bind_rows(me.full, me.hcw, me.no)
me %>%
  ggplot(aes(x = Doses, group = Model, fill = Model)) + geom_histogram() + scale_y_log10() + theme_bw()

doses <- bind_rows(me, ni)
doses %>%
  ggplot(aes(x = Doses, group = Model, fill = Model)) + geom_histogram() + scale_y_log10() + theme_bw() + 
  facet_wrap(~Disease, scales = "free_x") + 
  scale_fill_manual(values = c())
