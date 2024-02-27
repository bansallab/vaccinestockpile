
setwd("~/Downloads")

library(tidyverse)
library(magrittr)
library(glmmTMB)


nipah <- read_rds("nipah_GLMM.rds")
nipah %<>% confint() %>%
  as_tibble(rownames = 'variable') %>% 
  filter(!(variable %in% c('(Intercept)',
                           'Std.Dev.(Intercept)|param_scenario',
                           'Std.Dev.(Intercept)|emerg_location')))

nipahc <- read_rds("nipah_GLMM_corrected.rds")
nipahc %<>% confint() %>%
  as_tibble(rownames = 'variable') %>% 
  filter(!(variable %in% c('(Intercept)',
                           'Std.Dev.(Intercept)|param_scenario',
                           'Std.Dev.(Intercept)|emerg_location')))

MERS <- read_rds("MERS_GLMM.rds")
MERS %<>% confint() %>%
  as_tibble(rownames = 'variable') %>% 
  filter(!(variable %in% c('(Intercept)',
                           'Std.Dev.(Intercept)|param_scenario',
                           'Std.Dev.(Intercept)|emerg_location')))

MERSc <- read_rds("MERS_GLMM_corrected.rds")
MERSc %<>% confint() %>%
  as_tibble(rownames = 'variable') %>% 
  filter(!(variable %in% c('(Intercept)',
                           'Std.Dev.(Intercept)|param_scenario',
                           'Std.Dev.(Intercept)|emerg_location')))

nipah %<>% mutate(Pathogen = "Nipah virus", Stockpile = 'Total')
nipahc %<>% mutate(Pathogen = "Nipah virus", Stockpile = 'Community')
MERS %<>% mutate(Pathogen = "MERS-CoV", Stockpile = 'Total')
MERSc %<>% mutate(Pathogen = "MERS-CoV", Stockpile = 'Community')

data <- bind_rows(nipah, nipahc, MERS, MERSc)

codes <- c('alpha' = 'Death rate',
           'asymptomatic_rate' = 'Asymptomatic proportion',
           'days_random_vac' = 'Delay for contact tracing',
           'gamma' = 'Recovery rate',
           'omega' = 'Rate of progression to infectious',
           'pH_mean' = 'Mean time to hospital',
           'phi' = 'Vaccine efficacy',
           'ring_vac' = 'Ring vaccination rate',
           'threshold' = 'Case threshold for vaccination',
           'unsafe' = 'Proportion of risky contacts for HCWs',
           'vacc_delay' = 'Delay for vaccine availability',
           'vacc_eff_delay' = 'Delay between vaccination and protection',
           'vacc_rate_HCW' = 'HCWs vaccination rate'
           )

data %>%
  mutate(variable = recode(variable, !!!codes)) %>%
  mutate(variable = factor(variable, levels = rev(c('Death rate',
                                                   'Rate of progression to infectious',
                                                   'Asymptomatic proportion',
                                                   'Recovery rate',
                                                   'Case threshold for vaccination',
                                                   'Delay for vaccine availability',
                                                   'Delay between vaccination and protection',
                                                   'Vaccine efficacy',
                                                   'Ring vaccination rate',
                                                   'Delay for contact tracing',
                                                   'Mean time to hospital',
                                                   'Proportion of risky contacts for HCWs',
                                                   'HCWs vaccination rate')))) %>%
  mutate(Stockpile = factor(Stockpile, levels = c('Total',
                                                  'Community'))) %>%
  ggplot(aes(y = variable, x = Estimate,
             color = Stockpile)) + 
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_pointrange(aes(xmin = `2.5 %`, xmax = `97.5 %`), position = position_dodge(-0.4)) + 
  facet_grid(~Pathogen, scales = "free") +
  #scale_x_discrete(limits = 'rev') +
  theme_bw() + ylab('') + xlab('\nEstimated effect') + 
  theme(legend.position = c(0.92, 0.88),
        legend.box.background = element_rect(color="grey70", size=1),
        strip.text.x = element_text(size = 11.5)) + 
  scale_color_manual(values = c('#374E55', '#79AF97'))
