# GPS point compilation for all points + nests
# Ana Miller-ter Kuile
# January 11, 2022

# this script combines all visit points along with nest GPS locations 

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl",
                  "sf")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

nests <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds01_nest_locations.xlsx"))


# Prep nests DF -----------------------------------------------------------

nests2 <- nests %>%
  #select EM sites only
  filter(str_detect(Point_ID, "EMMAOR|EMFWOR|EMPAID")) %>%
  #select variables of interest
  dplyr::select(Nest_ID, UTM_E, UTM_N, UTM_datum_zone) %>%
  #rename things for consistency
  rename("Location_ID" = "Nest_ID",
         "DATUM" = "UTM_datum_zone") %>%
  #set type to nest location vs survey point (probs lots of overlap)
  mutate(Type = "Nest_location") %>%
  mutate(UTM_N_new = case_when(UTM_E > UTM_N ~ UTM_E,
                               TRUE ~ UTM_N),
         UTM_E_new = case_when(UTM_E > UTM_N ~ UTM_N,
                               TRUE ~ UTM_E)) %>%
  mutate(UTM_E = case_when(UTM_E > 1000000 ~ UTM_E_new,
                           TRUE ~ UTM_E),
         UTM_N = case_when(UTM_N < 1000000 ~ UTM_N_new,
                           TRUE ~ UTM_N)) %>%
  dplyr::select(-UTM_E_new, -UTM_N_new)


# Combine DFs -------------------------------------------------------------

all_points <- nests2 %>%
  mutate(DATUM = case_when(DATUM == "NAD83 Z10" ~ "NAD83 10N",
                           DATUM == "NAD83 Z11" ~ "NAD83 11N",
                           TRUE ~ DATUM))


# Convert to spatial object -----------------------------------------------

#set the CRS for each set
crs10 <- '+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
crs11 <- '+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

#Lat long
newcrs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

#split by CRS and convert to SF
points_10 <- all_points %>%
  filter(DATUM == "NAD83 10N") %>%
  st_as_sf(coords = c("UTM_E", "UTM_N"),
           crs = crs10) %>%
  st_transform(crs = newcrs) #transform to new CRS

points_11 <- all_points %>%
  filter(DATUM == "NAD83 11N") %>%
  st_as_sf(coords = c("UTM_E", "UTM_N"),
           crs = crs11) %>%
  st_transform(crs = newcrs) #transform to new CRS

#recombine all data points
all_points2 <- points_10 %>%
  rbind(points_11) %>%
  dplyr::select(-DATUM)

#check with some plotting
plot(all_points2)


# Export spatial object ---------------------------------------------------

#export it!
st_write(all_points2, 
         here("data_outputs",
              "01_cleaning",
              "00_gps",
              "survey_nest_points.shp"),
         append = F)


# Another option - Albers projection --------------------------------------

# albers prj4
prj4_alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#transform all points DF to albers
all_points3 <- all_points2 %>%
  st_transform(crs = prj4_alb)

plot(all_points3)


# Export Albers spatial ---------------------------------------------------

st_write(all_points3, 
         here("data_outputs",
              "01_cleaning",
              "00_gps",
              "survey_nest_points_albers.shp"),
         append = F)

# END SCRIPT
