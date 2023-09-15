#Table/figure of covariate inclusion/exclusion justifications
#Ana Miller-ter Kuile
#August 30, 2023

#this makes a geom_tile figure of the covariate inclusion/exclusion
#per model table

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load table data ---------------------------------------------------------

data <- read.csv(here("supplements",
                      "cov_inclusion_table.csv"))


# Make tile figure --------------------------------------------------------

data1 <- data %>%
  mutate(Covariate = case_when(Covariate == 'Local-scale management category' ~ 
                                 "Stand-scale management category",
                               TRUE ~ Covariate)) %>%
  mutate(Covariate = factor(Covariate,
                            levels = c('Nest height',
                                       'Nest orientation',
                                       'Nest tree species',
                                       'Initiation date',
                                       'Small tree density',
                                       'Large tree density',
                                       'Percent ponderosa',
                                       'Stand-scale management category',
                                       'Mean forest patch size',
                                       'Area weighted mean forest patch size',
                                       'Forest patch size coefficient of variation',
                                       'Contagion Index',
                                       'Percent forested area',
                                       'Percent open area',
                                       'Largest patch index',
                                       'Largest open patch index',
                                       'Largest forested patch index',
                                       'Number of patches',
                                       'Number of open patches',
                                       'Number of forest patches',
                                       'Forest patch mean proximity',
                                       'Percent landscape harvested',
                                       'Percent landscape burned',
                                       'Number of times treated',
                                       'Maximum temperature',
                                       'Maximum temperature2',
                                       'Precipitation',
                                       'Precipitation2',
                                       'Antecedent maximum temperature',
                                       'Antecedent precipitation',
                                       'Small tree density x Percent ponderosa',
                                       'Large tree density x Percent ponderosa',
                                       'Small tree density x Maximum temperature',
                                       'Large tree density x Maximum temperature',
                                       'Perc. landscape harvested x Perc. landscape burned',
                                       'Maximum temperature x Precipitation'))) %>%
  mutate(inclusion_cat = factor(inclusion_cat, levels = c("Included",
                                                          "Excluded - Scale", 
                                                          "Excluded - Response data",
                                                          "Excluded - Correlated",
                                                          "Excluded - Extreme"))) %>%
  mutate(Model = factor(Model, levels = c("Nest initation date",
                                          "Egg production",
                                          "Egg survival",
                                          "Nestling survival")))
# 
# '#b35806'
# '#e08214'
# '#fdb863'
# '#fee0b6'
# 
# '#8073ac'
# '#542788'


fig <- ggplot(data1) +
  geom_tile(aes(x = Model, y = ordered(Covariate, 
                                       levels=rev(levels(Covariate))), 
                fill = inclusion_cat), color = "black", linewidth = 0.5
            ) +
  theme_bw() +
  coord_fixed(ratio = 0.7) +
  scale_fill_manual(values = c('#8073ac',
                               '#b35806',
                               '#e08214', 
                               '#fdb863',
                               '#fee0b6'),
                    name = "Included/Excluded") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Covariate")

ggsave(filename = here("Pictures",
                       "Rfigures",
                       "supplement",
                       "covariates",
                       "covariate_inclusion.pdf"),
       plot = fig,
       height = 11,
       width = 8,
       units = "in"
       )


