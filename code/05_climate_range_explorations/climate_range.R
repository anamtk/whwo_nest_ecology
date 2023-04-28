#Range explorations
#Ana Miller-ter Kuile
#November 18, 2021

#this script extracts temperature and climate data for the entire
#range of WHWO based on the distribution map from eBIRD
# it then imports the points for the plots for the study and extracts
#climate data at them
#then - we can explore where in the range of the overall species range of
# temp and precipitation the locations of our plots are

# Load packages -----------------------------------------------------------

library(raster)
library(sf)
library(fasterize)
library(here)
library(tidyverse)
library(patchwork)
library(hexbin)
theme_set(theme_bw())

# Load climate and distribution data --------------------------------------

#downloaded climate (mean temp and yearly ppt) from PRISM
#from https://prism.oregonstate.edu/normals/
meanTemp <- raster(here("data_raw",
                        "range_maps",
                        "PRISM_tmean_30yr_normal_800mM2_annual_bil",
                        "PRISM_tmean_30yr_normal_800mM2_annual_bil.bil")) #bil.bil file
meanPPT <- raster(here("data_raw",
                       "range_maps",
                       "PRISM_ppt_30yr_normal_800mM2_annual_bil",
                       "PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")) #bil.bil file

#Downloaded eBird range map for WHWO 
#from species range from eBird
speciesRange <- st_read(here("data_raw",
                             "range_maps",
                             'whhwoo-range-2020.gpkg',
                             "whhwoo-rangeRaw-2020.gpkg")) #raw file
plot(speciesRange)


# Match projections for climate and range data ----------------------------

#reproject the species range to the climate variables
spRangeproj <- st_transform(speciesRange, st_crs = crs(meanTemp))

# Make range a raster -----------------------------------------------------

# Rasterize it (make the species range a raster of the same size as the temp data)
spRangeGrid <- fasterize(spRangeproj, meanTemp)
plot(spRangeGrid)

# Clip the temperature and precip to the range ----------------------------

# Mask the two climate grids (clip to just the species ranges)
tempClip <- mask(meanTemp, spRangeGrid)
pptClip <- mask(meanPPT, spRangeGrid)


# Plot the densities of each ----------------------------------------------

# Plot the density of clmiate and ppt values within the range of the bird
density(tempClip)
density(pptClip)

# Subsample for quicker plotting ------------------------------------------

#to get a subsample of the many many points within the clipped climate layers, 
# we can sample regular from them and then remove the NA values (since it
# samples over the whole unclipped range)
sampleTemp <- sampleRegular(tempClip, size = 1e6)
sampleTemp <- sampleTemp[!is.na(sampleTemp)]
hist(sampleTemp)

samplePPT <- sampleRegular(pptClip, size = 1e6)
samplePPT <- samplePPT[!is.na(samplePPT)]
hist(sampleTemp)

# Load data for measurement points ----------------------------------------

# to now add the data from teh sampling points:
#Read in the GPS locations for each site
#site_visits3 dataset
source(here("code", "01_cleaning",
            "00_GPS_all_ptsnests.R"))


# Convert to SF and reproject ---------------------------------------------

#Already converted to SF object
all_points <- st_transform(all_points2, st_crs = crs(meanTemp))

# Extract temp and precip values at these sites ---------------------------

#Use the raster::extract function to get climate data at each plot
sites_temp <- raster::extract(x = meanTemp, 
                                 y = all_points)

sites_ppt <- raster::extract(x = meanPPT, 
                                 y = all_points)


#combine all sites back together

sites_clim <- all_points %>%
  cbind(sites_temp) %>%
  cbind(sites_ppt) %>%
  rename("Temp" = "sites_temp",
         "PPT" = "sites_ppt") %>%
  mutate(Forest = str_sub(Location_ID, 1, 6))

# Graph all together ------------------------------------------------------
#sampleTemp
sampleTemp <- as.data.frame(sampleTemp)
samplePPT <- as.data.frame(samplePPT)
#samplePPT
#sites_clim

samples <- sampleTemp %>%
  cbind(samplePPT)

(overallvals <- ggplot(samples) +
  geom_hex(aes(x = sampleTemp, y = samplePPT), bins = 50) +
  scale_fill_viridis_c() +
  stat_ellipse(data = sites_clim, aes(x = Temp, y = PPT),
               color = "white", size = 1) +
  stat_ellipse(data = sites_clim, aes(x = Temp, y = PPT),
               color = "black", size = 0.5) +
  labs(x = "30-year Avg Annual Temp",
       y = "30-year Avg Annual Precip",
       title = "Temperature and precipitation across entire WHWO distribution"))

ggsave(plot = overallvals,
       filename = 'climate_range.jpeg',
       path = here("pictures", 
                   "Rfigures", 
                   "supplement",
                   "climate"),
       width = 7, height = 5,
       units = "in")

(siteppt <- ggplot(sites_clim, aes(x = Forest, y = PPT, fill = Forest)) +
  geom_boxplot() +
  #geom_jitter(height = 0, width = 0.2, alpha = 0.6, shape = 1) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("#7fc97f",
                               "#beaed4",
                               "#fdc086")) +
  scale_x_discrete(labels = c("EMPAID" = "Payette",
                               "EMMAOR" = "Malheur",
                               "EMFWOR" = "Fremont-Winema")) +
  ylim(0, 5000) +
  labs(y = '30-year Avg Annual Precip',
       title = "Distribution of precipitation values across the EM plots") +
  theme(legend.position = "none"))

(rangeppt <- ggplot(samplePPT, aes(x = samplePPT)) +
  geom_density() +
  xlim(0, 5000) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "Relative Frequency",
       title = "Distribution of precipitation values across WHWO range"))
  
(ppt_graphs <- rangeppt / siteppt +
  plot_layout(nrow = 2, heights = c(2, 1)))

ggsave(plot = ppt_graphs,
       filename = 'ppt_dists.jpeg',
       path = here("pictures", 
                   "Rfigures", 
                   "supplement",
                   "climate"),
       width = 7, height = 5,
       units = "in")

(sitet <- ggplot(sites_clim, aes(x = Forest, y = Temp, fill = Forest)) +
  geom_boxplot() +
  #geom_jitter(height = 0, width = 0.2, alpha = 0.6, shape = 1) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) + 
    scale_fill_manual(values = c("#7fc97f",
                                 "#beaed4",
                                 "#fdc086")) +
    scale_x_discrete(labels = c("EMPAID" = "Payette",
                                "EMMAOR" = "Malheur",
                                "EMFWOR" = "Fremont-Winema")) +
  ylim(-10, 25) +
  labs(y = '30-year Avg Annual Temperature',
       title = "Distribution of temperature values across the EM plots") +
    theme(legend.position = "none"))

(ranget <- ggplot(sampleTemp, aes(x = sampleTemp)) +
  geom_density() +
  xlim(-10, 25) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "Relative Frequency",
       title = "Distribution of temperature values across WHWO range"))

(t_graphs <- ranget / sitet +
  plot_layout(nrow = 2, heights = c(2, 1)))

ggsave(plot = t_graphs,
       filename = 't_dists.jpeg',
       path = here("pictures", 
                   "Rfigures", 
                   "supplement",
                   "climate"),
       width = 7, height = 5,
       units = "in")
