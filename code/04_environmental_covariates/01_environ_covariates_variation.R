# Exploration of variation within nest survival environmental covariates
# Ana Miller-ter Kuile
# December 13, 2021

#  this script creates explores variation in all the variables
# that may be important for nest survival model. The goal here is 
# to identify those with no or little variation and remove them
# from what is right now a sort of complicated model

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "GGally", "patchwork",
                  "lubridate")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

#load custom plotting
source(here("code",
            "00_functions",
            "plot_functions.R"))

# Load data ---------------------------------------------------------------

#nests8 dataset output from the cleaning code "04_nest_survival_dataprep.R"
nests <- read.csv(here("data_outputs", "01_cleaning", 
                       "03_nestling_survival",
                       "Egg_Nestling_survival_data.csv"))


# Select columns of interest ----------------------------------------------

colnames(nests)

nest_var <- nests %>%
  distinct(Nest_ID, Project_ID, Trt_cat,
           Nest_Ht, Tree_sp, cosOrientation, Init_day,
           pPIPO, Trees_2550, Trees_50,
           Tmax_eg, Tmax_ne, PPT_eg, PPT_ne, a1000_areacv2, a1000_contag,
           a1000_pland2, a1000_np, a1000_proxmn2,
           a1000_Ha, a1000_Bu, n_tx, Time_groups)


# Treatment covariates ----------------------------------------------------

tx_cat <- nest_var %>%
  group_by(Project_ID, Trt_cat) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = Trt_cat, y = total)) +
  geom_bar(stat = "identity") +
  facet_grid(Project_ID~.)

tx_time <- nest_var %>%
  group_by(Project_ID, Time_groups) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = Time_groups, y = total)) +
  geom_bar(stat = "identity") +
  facet_grid(Project_ID~.)

ntx <- covariate_variation_fn(df = nest_var,
                       vars = "n_tx")

tx_all <- tx_cat + tx_time + ntx

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "variation_tx.jpeg"),
       plot = tx_all,
       width= 7,
       height =5,
       units = 'in')

# Nest Level --------------------------------------------------------------

#continuous Nest Level
# Nest_ht, Orientation, Initiation_date
#categorical nest level
# Tree_sp

nest_cont <- covariate_variation_fn(df = nest_var, 
                       vars = c("Nest_Ht", "cosOrientation",
                                "Init_day"))

nest_cat <- nest_var %>%
  group_by(Project_ID, Tree_sp) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = Tree_sp, y = total)) +
  geom_bar(stat = "identity") +
  facet_grid(Project_ID~.)

nest_all <- nest_cont + nest_cat +
  plot_layout(widths = c(3,1))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "variation_nest.jpeg"),
       plot = nest_all,
       width= 7,
       height =5,
       units = 'in')


# Site Level --------------------------------------------------------------

# Local scale:
# Trees_50
# Trees_2550
#PPIPO
local <- covariate_variation_fn(df = nest_var,
                       vars = c("Trees_2550",
                                "Trees_50",
                                "pPIPO"))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "variation_local.jpeg"),
       plot = local,
       width= 7,
       height =5,
       units = 'in')



# Climate -----------------------------------------------------------------

climate <- covariate_variation_fn(df = nest_var,
                       vars = c("Tmax_eg",
                                "Tmax_ne",
                                "PPT_eg",
                                "PPT_ne"))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "variation_climate.jpeg"),
       plot = climate,
       width= 7,
       height =5,
       units = 'in')



# Landscape ---------------------------------------------------------------

landscape <- covariate_variation_fn(df = nest_var,
                       vars = c("a1000_areacv2",
                                "a1000_contag",
                                "a1000_pland2",
                                "a1000_np",
                                "a1000_proxmn2",
                                "a1000_Ha",
                                "a1000_Bu"))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "variation_land.jpeg"),
       plot = landscape,
       width= 8,
       height =6,
       units = 'in')

  
