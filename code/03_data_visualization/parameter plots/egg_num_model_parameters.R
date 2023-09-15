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

source(here::here("code", 
                  "00_functions",
                  "plot_functions.R"))

source(here::here("code",
                  "00_functions",
                  "tidy_functions.R"))


# Load model -------------------------------------------------------------

egg_num <- readRDS(here("monsoon",
                       "8_24_23",
                       "egg_num",
                       "outputs",
                       "egg_number_model_summary_8_24.RDS"))

# Prep model for plotting ------------------------------------------------

#WHat we need:
#For each model - 
#1. rename the parameters to be human-friendly 
## and consistent across models
#2. A column of the p-value of that parameter in taht 
## model
#3. A column for "model ID" to indicate which model it is from
#4. a Model for parameter number to help sort the plot
## for presentation
#5. A column for covariate "category" to maybe facet plots

# Get parameter estimates per model ---------------------------------------

egg_number <- as.data.frame(egg_num$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "b")) %>%
  filter(!str_detect(parameter, "b0|z"))

parameters <- egg_number$parameter

# Get p-values per model --------------------------------------------------

egg_n_ps <- as.data.frame(egg_num$statistics) %>%
  dplyr::select(Mean) %>%
  rownames_to_column(var = "zvalue") %>%
  filter(str_detect(zvalue, "z")) %>%
  mutate(p = case_when(Mean >= 0.5 ~ (1-Mean), #I Think these are 1-tailed p-values but check with Kiona
                       Mean < 0.5 ~ (1 - (1-Mean)))) %>%
  mutate(parameter = parameters) 

# Combine p-values with parameter estimates -------------------------------

#combine DFs and then rename the parameters, making
# sure they stay in the right order - tricky
egg_number_df <- egg_number %>%
  left_join(egg_n_ps, by = "parameter") %>%
  filter(parameter != 'b1[1]') %>%
  mutate(category = case_when(parameter %in% c('b1[2]',
                                               'b1[3]',
                                               'b1[4]') ~ "Nest management (2.25 ha)",
                              parameter == "b[2]" ~ "Nest habitat",
                              parameter %in% c('b[3]', 'b[4]', 
                                               'b[5]') ~ "Local habitat (0.4 ha)",
                              parameter %in% c("b[6]", "b[7]") ~ "Climate (27 ha)",
                              parameter %in% c("b[8]", "b[9]", "b[10]",
                                               'b[11]') ~ "Landscape habitat (314 ha)",
                              parameter %in% c("b[12]", "b[13]") ~ "Landscape management (314 ha)",
                              parameter %in% c("b[14]", "b[15]",
                                               "b[16]", "b[17]",
                                               "b[18]", "b[19]") ~ "Variable interactions",
                              TRUE~ NA_character_)) %>%
  mutate(parameter = case_when(parameter == "b1[2]" ~ "Treatment type: Burn",
                               parameter == "b1[3]" ~ "Treatment type: Harvest",
                               parameter == "b1[4]" ~ "Treatment type: Harvest&Burn",
                               parameter == "b[2]" ~ "Nest initiation day",
                               parameter == "b[3]" ~ "Large tree density",
                               parameter == "b[4]" ~ "Small tree density",
                               parameter == "b[5]" ~ "Percent ponderosa forest",
                               parameter == "b[6]" ~ "Maximum temperature",
                               parameter == "b[7]" ~ "Precipitation",
                               parameter == "b[8]" ~ "Forest patch size coeff. variation",
                               parameter == "b[9]" ~ "Contagion index",
                               parameter == "b[10]" ~ "Largest patch index",
                               parameter == "b[11]" ~ "Number of open patches",
                               parameter == "b[12]" ~ "Percent of landscape harvested",
                               parameter == "b[13]" ~ "Percent of landscape burned",
                               parameter == "b[14]" ~ "Large tree density*perc. ponderosa",
                               parameter == "b[15]" ~ "Small tree density*perc. ponderosa",
                               parameter == "b[17]"~ "Large tree density*maximum temperature",
                               parameter == "b[16]" ~ "Small tree density*maximum temperature",
                               parameter == "b[18]" ~ "Percent of landscape harvested*percent burned",
                               parameter == "b[19]" ~ "Temperature * precipitation",
                               TRUE ~ parameter)) %>%
  rename("beta_lower" = "2.5%",
         "beta_50" = "50%",
         "beta_upper" = "97.5%") %>%
  dplyr::select(parameter, beta_lower, beta_50, Mean,
                beta_upper, p, category)



write.csv(egg_number_df, 
          here("data_outputs",
               "03_model_results",
               "egg_number_parameter_mediansCIs.csv"))

#"PIPO", "Abies", "POTR5",
#"JUOC", "PSME"
