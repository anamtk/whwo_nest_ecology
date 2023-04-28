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

init_mod <- readRDS(here("monsoon",
                        "10_19_22",
                        "initiation",
                        "outputs",
                        "initiation_temp_JAGS_model_summary_10_19.RDS"))

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

init_data <- as.data.frame(init_mod$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "b")) %>%
  filter(!str_detect(parameter, "b0|z")) 

parameters <- init_data$parameter

# Get p-values per model --------------------------------------------------

init_ps <- as.data.frame(init_mod$statistics) %>%
  dplyr::select(Mean) %>%
  rownames_to_column(var = "zvalue") %>%
  filter(str_detect(zvalue, "z")) %>%
  mutate(p = case_when(Mean >= 0.5 ~ (1-Mean), #I Think these are 1-tailed p-values but check with Kiona
                       Mean < 0.5 ~ (1 - (1-Mean)))) %>%
  mutate(parameter = parameters) 

# Combine p-values with parameter estimates -------------------------------

#combine DFs and then rename the parameters, making
# sure they stay in the right order - tricky
init_df <- init_data %>%
  left_join(init_ps, by = "parameter") %>%
  filter(parameter != 'b1[1]') %>%
  mutate(category = case_when(parameter %in% c("b[8]",
                                               "b[9]") ~ "Landscape management (314 ha)",
                              parameter %in% c('b[1]', 'b[2]', 
                                               'b[3]') ~ "Local habitat (0.4 ha)",
                              parameter %in% c("b[4]") ~ "Climate (27 ha)",
                              parameter %in% c("b[5]", "b[6]",
                                               "b[7]") ~ "Landscape habitat (314 ha)",
                              parameter %in% c("b[10]", "b[11]",
                                               "b[12]", "b[13]",
                                               "b[14]") ~ "Variable interactions",
                              TRUE~ NA_character_)) %>%
  mutate(parameter = case_when(parameter == "b[1]" ~ "Large tree density",
                               parameter == "b[2]" ~ "Small tree density",
                               parameter == "b[3]" ~ "Percent ponderosa forest",
                               parameter == "b[4]" ~ "Maximum temperature",
                               parameter == "b[5]" ~ "Forest patch size coeff. variation",
                               parameter == "b[6]" ~ "Proximity of forest patches",
                               parameter == "b[7]" ~ "Contagion index",
                               parameter == "b[8]" ~ "Percent of landscape harvested",
                               parameter == "b[9]" ~ "Percent of landscape burned",
                               parameter == "b[10]" ~ "Large tree density*perc. ponderosa",
                               parameter == "b[11]" ~ "Small tree density*perc. ponderosa",
                               parameter == "b[12]"~ "Small tree density*maximum temperature",
                               parameter == "b[13]" ~ "Large tree density*maximum temperature",
                               parameter == "b[14]" ~ "Percent of landscape harvested*percent burned",
                               TRUE ~ parameter)) %>%
  rename("beta_lower" = "2.5%",
          "beta_50" = "50%",
          "beta_upper" = "97.5%") %>%
  dplyr::select(parameter, beta_lower, beta_50,
                beta_upper,  Mean, p, category)



write.csv(init_df, 
          here("data_outputs",
               "03_model_results",
               "init_parameter_mediansCIs.csv"))

#"PIPO", "Abies", "POTR5",
#"JUOC", "PSME"