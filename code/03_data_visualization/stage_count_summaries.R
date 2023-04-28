# Histograms of eggs, nestlings, fledglings
# Ana Miller-ter Kuile
# September 12, 2022

# pic for paper of histogram of eggs, nestlings, fledglings




# Load packages -----------------------------------------------------------


# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "patchwork", 'ggridges')


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

# Load data ---------------------------------------------------------------

data <- read.csv(here("data_outputs",
                       "01_cleaning",
                       "03_nestling_survival",
                       "Egg_Nestling_survival_data.csv"))


init_data <- read.csv(here("data_outputs",
                           "01_cleaning",
                           "03b_nest_initiation",
                           "Nest_initiation_data.csv"))
# Prep data ---------------------------------------------------------------

colnames(data)

#make DF longer so that egg, young, fledge are all
#in one count column with "groups" by "age"
data1 <- data %>%
  filter(No_eggs > 0) %>%
  dplyr::select(Nest_ID, NoFL_uncert, No_eggs,
                No_young) %>%
  pivot_longer(NoFL_uncert:No_young,
               names_to = "age",
               values_to = "count")  

init2 <- init_data %>%
  dplyr::select(Init_day) 
# Plot --------------------------------------------------------------------


# Option 1: bar graph histograms ------------------------------------------

# New facet label names for stage variable
stage.labs <- c("Eggs", "Nestlings", "Fledglings")
names(stage.labs) <- c("No_eggs", "No_young", "NoFL_uncert")


(stages_plot <- ggplot(data1, aes(x = count, fill = age)) +
  geom_bar(position = position_dodge(preserve = "single"), 
           color = "black") +
  scale_fill_manual(name = "Stage", 
                    values = c('#969696',
                               '#cccccc',
                               '#f7f7f7'),
                    labels = c("No_eggs" = "Eggs",
                               "No_young" = "Nestlings",
                               "NoFL_uncert" = "Fledglings")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3,
                                4, 5, 6, 7)) +
  labs(x = "Count in stage", y = "Number of nests") +
  facet_grid(age~.,
             labeller = labeller(age = stage.labs)) +
  theme(legend.position = "none"))


ggsave(plot = stages_plot,
       filename = here("pictures", "Rfigures", "stages_plot.pdf"),
       width = 3, 
       height = 5,
       units = "in")

# Initiation facet --------------------------------------------------------

(init_plot <- ggplot(init2, aes(x = Init_day)) +
  geom_histogram(fill = "#525252", color = "black") +
  labs(x = "Nest initiation day", y = "Number of nests"))

ggsave(plot = init_plot,
       filename = here("pictures", "Rfigures", "init_plot.pdf"),
       width = 4.75, 
       height = 2,
       units = "in")
