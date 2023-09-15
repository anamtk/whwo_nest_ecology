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

nestling_s <- readRDS(here("monsoon",
                           "8_24_23",
                           "nestling",
                           "outputs",
                           "nestling_survival_model_summary_8_24.RDS"))

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

nestling_survival <- as.data.frame(nestling_s$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "b")) %>%
  filter(!str_detect(parameter, "b0|z")) 

parameters <- nestling_survival$parameter

# Get p-values per model --------------------------------------------------

nestling_survival_ps <- init_ps <- as.data.frame(nestling_s$statistics) %>%
  dplyr::select(Mean) %>%
  rownames_to_column(var = "zvalue") %>%
  filter(str_detect(zvalue, "z")) %>%
  mutate(p = case_when(Mean >= 0.5 ~ (1-Mean), #I Think these are 1-tailed p-values but check with Kiona
                       Mean < 0.5 ~ (1 - (1-Mean)))) %>%
  mutate(parameter = parameters) 

# Combine p-values with parameter estimates -------------------------------

#combine DFs and then rename the parameters, making
# sure they stay in the right order - tricky
nestling_survival_df <- nestling_survival %>%
  left_join(nestling_survival_ps, by = "parameter") %>%
  filter(!parameter %in% c("b1TrtID[1]", 'b2SpeciesID[1]')) %>%
  mutate(category = case_when(parameter %in% c("b1TrtID[2]",
                                               'b1TrtID[3]',
                                               'b1TrtID[4]') ~ "Nest management (2.25 ha)",
                              parameter %in% c('b2SpeciesID[2]',
                                               'b2SpeciesID[3]',
                                               'b2SpeciesID[4]',
                                               'b2SpeciesID[5]',
                                               'b[3]',
                                               'b[4]',
                                               'b[5]') ~ "Nest habitat",
                              parameter %in% c('b[6]', 'b[7]', 
                                               'b[8]') ~ "Local habitat (0.4 ha)",
                              parameter %in% c("b[9]", 
                                               "b[10]", "b[11]", "b[12]") ~ "Climate (27 ha)",
                              parameter %in% c("b[13]", "b[14]",
                                               "b[15]", "b[16]",
                                               "b[17]") ~ "Landscape habitat (314 ha)",
                              parameter %in% c("b[18]", 
                                               "b[19]") ~ "Landscape management (314 ha)",
                              parameter %in% c("b[20]", "b[21]",
                                               "b[22]", "b[23]",
                                               "b[24]", "b[25]") ~ "Variable interactions",
                              TRUE~ NA_character_)) %>%
  mutate(parameter = case_when(parameter == "b1TrtID[2]" ~ "Treatment type: Harvest",
                               parameter == "b1TrtID[3]" ~ "Treatment type: Burn",
                               parameter == "b1TrtID[4]" ~ "Treatment type: Harvest&Burn",
                               parameter == "b2SpeciesID[2]" ~ "Nest tree species: Aspen",
                               parameter == "b2SpeciesID[3]" ~ "Nest tree species: Juniper",
                               parameter == "b2SpeciesID[4]" ~ "Nest tree species: Douglas Fir",
                               parameter == "b2SpeciesID[5]" ~ "Nest tree species: Fir",
                               parameter == "b[3]" ~ "Nest height",
                               parameter == "b[4]" ~ "Nest orientation",
                               parameter == "b[5]" ~ "Nest initiation day",
                               parameter == "b[6]" ~ "Large tree density",
                               parameter == "b[7]" ~ "Small tree density",
                               parameter == "b[8]" ~ "Percent ponderosa forest",
                               parameter == "b[9]" ~ "Maximum temperature",
                               parameter == "b[10]"~ "Maximum temperatue ^2",
                               parameter == "b[11]" ~ "Precipitation",
                               parameter == "b[12]" ~ "Precipitation ^2",
                               parameter == "b[13]" ~ "Percent forested landscape",
                               parameter == "b[14]" ~ "Total number of forest patches",
                               parameter == "b[15]" ~ "Forest patch size CV",
                               parameter == "b[16]" ~ "Contagion",
                               parameter == "b[17]" ~ "Largest Patch Index",
                               parameter == "b[18]" ~ "Perc. landscape burned",
                               parameter == "b[19]"~ "Perc. landscape harvested",
                               parameter == "b[20]" ~ "Small trees * percent ponderosa",
                               parameter == "b[21]" ~ "Large trees * percent ponderosa",
                               parameter == "b[22]" ~ "Large trees * temperature",
                               parameter == "b[23]" ~ "Perc harvest * perc. burned",
                               parameter == "b[24]" ~ "Temperature * precipiation",
                               TRUE ~ parameter)) %>%
  rename("beta_lower" = "2.5%",
         "beta_50" = "50%",
         "beta_upper" = "97.5%") %>%
  dplyr::select(parameter, beta_lower, beta_50, Mean,
                beta_upper, p, category)


#"PIPO", "Abies", "POTR5",
#"JUOC", "PSME"

write.csv(nestling_survival_df, 
          here("data_outputs",
               "03_model_results",
               "nestling_survival_parameter_mediansCIs.csv"))

