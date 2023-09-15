#Egg production plots
# Ana Miller-ter Kuile
# September 26, 2022

# this script brings in the model output as well as a 
# summaried 95% CI DF for the egg production model
# and makes a all-parameters plot with p-values as well as a 
# predicted plto for each significant parameter


# Load packages -----------------------------------------------------------


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

# Load data summary -------------------------------------------------------

egg_n <- read.csv(here("data_outputs",
                       "03_model_results",
                       "egg_number_parameter_mediansCIs.csv"))



# Data summary plot -------------------------------------------------------

egg_n2 <- egg_n %>%
  mutate(category = factor(category, levels = c("Nest management (2.25 ha)",
                                                "Landscape management (314 ha)",
                                                "Nest habitat",
                                                "Local habitat (0.4 ha)",
                                                "Landscape habitat (314 ha)",
                                                "Climate (27 ha)",
                                                "Variable interactions"))) %>%
  group_by(category) %>%
  arrange(category) %>%
  mutate(parameter = factor(parameter, levels=unique(parameter))) %>%
  mutate(p_levels = case_when((p >=0 & p <0.01) ~ "p < 0.01",
                              (p >= 0.01 & p<0.05) ~ "p < 0.05",
                              p == 0.05 ~ "p = 0.05",
                              TRUE ~ "ns"))%>%
  mutate(p_cat = case_when(p <= 0.05 ~ "s",
                           TRUE ~ "ns"))


eggn_exp <- egg_n2 %>%
  dplyr::select(parameter, beta_50, beta_lower, beta_upper, p, category)

write.csv(eggn_exp, 
          file = here('data_outputs', '04_paper_tables',
                      'eggn_summary.csv'),
          row.names = F)

# (overall_egg_n <- ggplot(egg_n2, 
#        aes(x = parameter, y = beta_50, color = p_cat)) +
#   geom_point(size =2) +
#   geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_color_manual(values = c("black")) +
#     labs(y = "Covariate estimate \n (posterior median and 95% BCI)",
#          x = "Covariate")+
#   coord_flip() +
#   theme(strip.text.y.left = element_text(angle = 0)) +
#   facet_grid(category ~ ., 
#              scales = "free_y", 
#              space = "free_y") +
#   theme(strip.text.y = element_text(angle = 0),
#         legend.position = 'none',
#         strip.background = element_rect(fill = "white")))
# 
# 
# ggsave(plot = overall_egg_n,
#        filename = 'egg_n_all_plot.pdf',
#        path = here("pictures", "Rfigures", "egg_num"),
#        width = 8, height = 4,
#        units = "in")


# Summaries ---------------------------------------------------------------

#b0 intercept
as.data.frame(egg_num$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`) %>%
  mutate(across(is.numeric, ~ exp(.)))

#varinace terms
as.data.frame(egg_num$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "sig")) %>%
  dplyr::select(parameter, `2.5%`, `50%`, `97.5%`)

