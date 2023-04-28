# Missing predictors - are they correlated to groups?
# Ana Miller-ter Kuile
# June 1, 2022

#  this script looks at how missing predictors may be 
# grouped by forest location and/or treatment category


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "GGally", "patchwork",
                  "MuMIn", "DHARMa")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#nests dataset output from the cleaning code 
nests <- read.csv(here("data_outputs", "01_cleaning", 
                       "03_nest_survival",
                       "Nest_survival_data.csv"))

colnames(nests)

nests1 <- nests %>%
  distinct(Nest_ID, Project_ID,
           Year_located, Trt_50,
           Tmax, PPT, Init_day, Visit_date) %>%
  separate(Visit_date ,
           into = c("Year", "Month", "Day"),
           remove = F) %>%
  dplyr::select( -Year, -Day) 

# Climate -----------------------------------------------------------------

hist(nests$Tmax)

nests2 <- nests1 %>%
  filter(!is.na(Tmax)) %>%
  mutate(Year_located = as.factor(Year_located))

m1 <- lm(Tmax ~ Year_located*Project_ID,
         data = nests2,
         na.action = 'na.fail')

dredge(m1)

m1a <- lm(Tmax ~ Year_located,
          data = nests2,
          na.action = 'na.fail')

m1b <- lm(Tmax ~ Project_ID,
          data = nests2,
          na.action = 'na.fail')

summary(m1)
summary(m1a)
summary(m1b)

nests3 <- nests1 %>%
  filter(!is.na(PPT)) %>%
  mutate(Year_located = as.factor(Year_located))

hist(nests3$PPT)

m2 <- lm(PPT ~ Year_located*Project_ID,
          data = nests3,
          na.action = 'na.fail')

dredge(m2)

m2a <- lm(PPT ~ Year_located,
          data = nests3,
          na.action = 'na.fail')

m2b <- lm(PPT ~ Project_ID,
          data = nests3,
          na.action = 'na.fail')


summary(m2)
summary(m2a)
summary(m2b)


#OK - so for temp - 
# it looks like nothing is a super strong predictor, i think it's okay to
# just model from an average?

# for ppt -
# it looks like a model with year*project is best

# Initiation Day ----------------------------------------------------------

hist(nests$Init_day)

nests4 <- nests %>%
  filter(!is.na(Init_day)) %>%
  mutate(Year_located = as.factor(Year_located))

m3 <- lm(Init_day ~ Project_ID*Year_located,
         data = nests4,
         na.action = "na.fail")

dredge(m3)

m3a <- lm(Init_day ~ Project_ID,
         data = nests4,
         na.action = "na.fail")

m3b <- lm(Init_day ~ Year_located,
         data = nests4,
         na.action = "na.fail")

summary(m3)
summary(m3a)
summary(m3b)

ggplot(nests, aes(x = Init_day, fill = Project_ID)) +
  geom_density() +
  facet_grid(Year_located~.)

#summary: even though the modeling suggests that these variables
# are important, they really don't explain much variation overall
# (R^2 = 0.215)
# if I was going to put in a covariate on initation day, it looks like
# the best predictor would be year with an R^2 = 0.1061


# Tree densities ----------------------------------------------------------

#only one tree is missing tree density data, so yeah, not
# going to worry about grouping that - just going to 
# distribute it around a mu and tau
