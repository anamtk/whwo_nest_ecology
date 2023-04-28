# Exploration of correlation between environmental covariates
# Ana Miller-ter Kuile
# November 9, 2021

#  this script creates a correlation exploration between 
# the variables of interest for environmental covariates


#NEED TO DO:
# import all covariate DFs and bind together
# to look at correlation b/w them


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "GGally", "patchwork")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#nests8 dataset output from the cleaning code "04_nest_survival_dataprep.R"
nests <- read.csv(here("data_outputs", "01_cleaning", 
                       "03_nest_survival",
                         "Nest_survival_data.csv"))


# Select variables of interest --------------------------------------------

colnames(nests)


# Climate variable correlation --------------------------------------------

climate <- nests %>%
  dplyr::select(Tmax, PPT) 

ggpairs(climate)

# All in fledgling model --------------------------------------------------

colnames(fledge)
fledge_corr <- fledge %>%
  distinct(Nest_ID, Nest_Ht, Orientation,Trees_2550,
           Trees_50, pPIPO, Init_day, Tmax, a1000_pland2,
           a1000_np, a1000_Ha, a1000_Bu) %>%
  column_to_rownames(var = "Nest_ID")

ggpairs(fledge_corr)

#only one that is highly correlated is burn-harvest at landscape

vars <- nests %>%
  distinct(Trt_50, Nest_Ht, Tree_sp, Orientation, 
           Trees_2550, Trees_50, pPIPO,  Tmax,
           Init_day, 
            a1000_Ha, a1000_Bu)
ggpairs(vars)

# Graph it ----------------------------------------------------------------

corr_pairs <- ggpairs(nest_corr)  

(corr_pairs2 <- ggpairs(vars))

(corr_pairs3 <- ggpairs(land_vars))

(corr_pairs4 <- ggpairs(land_vars2))

ggpairs(cv)

# Select visit interval variables -----------------------------------------

interval_corr <- nests %>%
  distinct(Nest_ID, Project_ID, Tave_0408, Tmin_0408, 
           Tmax_0408, PPT_0408) %>%
  dplyr::select(-Nest_ID)

# Graph it ----------------------------------------------------------------

#HAHAHA ALL CORRELATED
(int_pairs <- ggpairs(interval_corr))

# Treatment against all ---------------------------------------------------

nests2 <- nests %>%
  distinct(Nest_ID, Project_ID, 
           Trt_50, Nest_Ht,
           Tree_sp, DBH, Orientation,
           Trees_2550,Trees_50, pPIPO)

treated <- function(y){
  
  graph <- ggplot(nests2, aes(x = Trt_50, y = .data[[y]])) +
    geom_boxplot() +
    geom_jitter(position = position_jitter(width = 0.2, height = 0),
                alpha = 0.6, shape = 1) +
    theme_bw()
  
  return(graph)
}

forest <- function(y){
  
  graph <- ggplot(nests2, aes(x = Project_ID, y = .data[[y]])) +
    geom_boxplot() +
    geom_jitter(position = position_jitter(width = 0.2, height = 0),
                alpha = 0.6, shape = 1) +
    theme_bw()
  
  return(graph)
}

cols <- c("Nest_Ht", "DBH",
          "Orientation", 'pPIPO',
          "Trees_2550", "Trees_50")

plots <- lapply(cols, treated)
plots2 <- lapply(cols, forest)
(trt_corr <- wrap_plots(plots))
(for_corr <- wrap_plots(plots2))

treated_facet <- function(x){
  
  dat <- nests2 %>%
    group_by(Trt_50, .data[[x]]) %>%
    tally(name = "count")
  
  graph <- ggplot(dat, aes(x = .data[[x]], y = count)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Trt_50) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(graph)
}

catcols <- c(
             "Tree_sp")

facet_plots <- lapply(catcols, treated_facet)

(cat_corr_trt <- wrap_plots(facet_plots))



