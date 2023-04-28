# Facetted figure for all models
# Ana Miller-ter Kuile
# September 23, 2022

# this script generates a facetted figure of all model
# beta estimates labeled nicely and in order of their
# "type"


# Load packages -----------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "patchwork")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

nest_s <- read.csv(here("data_outputs",
                        "03_model_results",
                        "nest_survival_parameter_mediansCIs.csv"))

egg_n <- read.csv(here("data_outputs",
                        "03_model_results",
                        "egg_number_parameter_mediansCIs.csv"))

egg_s <- read.csv(here("data_outputs",
                        "03_model_results",
                        "egg_survival_parameter_mediansCIs.csv"))

nestling_s <- read.csv(here("data_outputs",
                        "03_model_results",
                        "nestling_survival_parameter_mediansCIs.csv"))


# Combine data ------------------------------------------------------------

medians <- nest_s %>%
  bind_rows(egg_n, egg_s, nestling_s) %>%
  group_by(category) %>%
  arrange(category) %>%
  mutate(parameter = factor(parameter, levels=unique(parameter))) %>%
  mutate(p_levels = case_when((p >=0 & p <0.01) ~ "p < 0.01",
                              (p >= 0.01 & p<0.05) ~ "p < 0.05",
                               p == 0.05 ~ "p = 0.05",
                              TRUE ~ "ns"))

# Graph it! ---------------------------------------------------------------

ggplot(medians, 
       aes(x = parameter, y = beta_50, color = p_levels)) +
  geom_point(size =2) +
  geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  facet_grid(category ~ model, scales = "free_y", space = "free_y")
