# Tree data visualizations
# Ana Miller-ter Kuile
# January 12, 2022

# this script explores the tree data that has just been re-generated


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl", "lubridate",
                  "DHARMa", 'patchwork')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())


# Load data ---------------------------------------------------------------

trees <- read.csv(here('data_outputs', 
                       "01_cleaning", 
                       "03_nest_survival",
                       "Nest_survival_data.csv"))


# Prep data ---------------------------------------------------------------

trees <- trees %>%
  distinct(Nest_ID, Trt_cat, Year_located,
           Project_ID, UTM_datum_zone, 
           UTM_N, UTM_E, Trees_2550,
           Trees_50, pPIPO) %>%
  mutate(Trt_cat2 = case_when(Trt_cat %in% c("B", "H", "HB") ~ "Treated",
                             Trt_cat == "U" ~ "Untreated",
                             TRUE ~ NA_character_)) %>%
  mutate(Trt_cat= factor(Trt_cat, 
                         levels = c("U", "H", "B", "HB")))
  

boxplot_fun <- function(df, x, y, facet){
  plot <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.2) +
    facet_wrap(~ .data[[facet]])
  
  return(plot)
}

boxplot_fun(df = trees,
            x = "Trt_cat",
            y = "Trees_2550",
            facet = "Project_ID")

boxplot_fun(df = trees,
            x = "Trt_cat",
            y = "Trees_50",
            facet = "Project_ID")

boxplot_fun(df = trees,
            x = "Trt_cat",
            y = "pPIPO",
            facet = "Project_ID")


boxplot_fun(df = trees,
            x = "Trt_cat2",
            y = "Trees_2550",
            facet = "Project_ID")

boxplot_fun(df = trees,
            x = "Trt_cat2",
            y = "Trees_50",
            facet = "Project_ID")

boxplot_fun(df = trees,
            x = "Trt_cat2",
            y = "pPIPO",
            facet = "Project_ID")


# Stat --------------------------------------------------------------------

m1 <- lm(Trees_2550 ~ Trt_cat,
          data = trees)

simulateResiduals(m1, plot = T)
summary(m1) #harvest less and harvest burn less

m2 <- lm(Trees_50 ~ Trt_cat,
              data = trees)


simulateResiduals(m2, plot = T)
summary(m2) #harvest marginal

m3 <- lm(pPIPO ~ Trt_cat,
              data = trees)

simulateResiduals(m3, plot = T)
summary(m3) #all more than untreated

#on average, how different is tree density in treated/untreated
trees %>%
  ungroup() %>%
  group_by(Trt_cat) %>%
  summarise(total = n(),
            mean_2550 = mean(Trees_2550, na.rm = T),
            sd_2550 = sd(Trees_2550, na.rm = T),
            se_2550 = sd_2550/sqrt(total),
            mean_50 = mean(Trees_50, na.rm = T),
            sd_50 = sd(Trees_50, na.rm = T),
            se_50 = sd_50/sqrt(total),
            mean_pP = mean(pPIPO, na.rm = T),
            sd_pP = sd(pPIPO, na.rm = T),
            se_pP = sd_pP/sqrt(total))


# boxplots without facets -------------------------------------------------
trees <- trees %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("B", "H", "HB", "U")))


(a <- ggplot(trees, aes(x = Trt_cat, y = Trees_2550)) +
  geom_boxplot() +
  labs(x = "Treatment category",
       y = bquote("Small (25-50cm DBH) trees" ~ha^-1)) +
  scale_x_discrete(labels = c("U" = "Untreated",
                              "H" = "Harvest",
                              "B" = "Burn",
                              "HB" = "Harvest&Burn")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text",
           x = 2, 
           y = 200,
           label = "*",
           size = 10) +
  annotate(geom = "text",
           x = 3, 
           y = 200,
           label = "*",
           size = 10))
  

(b <- ggplot(trees, aes(x = Trt_cat, y = Trees_50)) +
  geom_boxplot() +
  labs(x = "Treatment category",
       y = bquote("Large (>50cm DBH) trees" ~ha^-1)) +
  scale_x_discrete(labels = c("U" = "Untreated",
                              "H" = "Harvest",
                              "B" = "Burn",
                              "HB" = "Harvest&Burn")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text",
           x = 2, 
           y = 75,
           label = "..",
           size = 10) +
  ylim(0, 75))

(c <- ggplot(trees, aes(x = Trt_cat, y = pPIPO)) +
  geom_boxplot() +
  labs(x = "Treatment category",
       y = "Percent ponderosa") +
  scale_x_discrete(labels = c("U" = "Untreated",
                              "H" = "Harvested",
                              "B" = "Burned",
                              "HB" = "Harvest&Burn")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom = "text",
           x = 2, 
           y = 1.1,
           label = "*",
           size = 10) +
  annotate(geom = "text",
           x = 1, 
           y = 1.1,
           label = "*",
           size = 10) +
  annotate(geom = "text",
           x = 3, 
           y = 1.1,
           label = "*",
           size = 10) +
  scale_y_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1.15)))

plots <- a + b + c +
  plot_layout(nrow = 3)

ggsave(plots, 
       filename = here("pictures", "Rfigures", "tx_trees.pdf"),
       width = 4,
       height = 6.25, 
       units = "in")
