###
####  This script produces maps of plaice stock boundaries and potential boundaries for WKBPLAICE
###

rm(list = ls())

#===
# Dependencies ----
#===
library(raster)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
#===

#===
# Data ----
#===
icesareas <- read_sf("../Data/ICES_areas", as_tibble = FALSE)
#===

#===
# Define stock areas ----
#===
icesareas$ple_21_23 <- ifelse(icesareas$SubDivisio %in% c("21", "22", "23"), TRUE, FALSE) 
icesareas$ple_24_32 <- ifelse(icesareas$SubDivisio %in% as.character(c(24:32)), TRUE, FALSE) 
icesareas$ple_21_32 <- ifelse(icesareas$SubDivisio %in% as.character(c(21:32)), TRUE, FALSE) 

icesareas$pleStock <- ifelse(icesareas$SubDivisio %in% as.character(c(21:23)), "21-23",
                             ifelse(icesareas$SubDivisio %in% as.character(c(24:32)), "24-32", 
                                    ifelse(icesareas$SubDivisio %in% as.character(c(20)), "NorthSea", NA))) 

#===

#===
# Set Extent and Projection ----
#===
## Reproject to coordinates for cropping
st_crs(icesareas)
icesareas <- st_transform(icesareas, "epsg:4326")
st_crs(icesareas)


## Reduce extent to relevant areas
ices_21_24 <- st_crop(st_make_valid(icesareas), c(xmin=8, xmax=15, ymin=53.8, ymax=59.2))
ices_21_32 <- st_crop(st_make_valid(icesareas), c(xmin=8, xmax=31, ymin=53.8, ymax=66))

## Reproject for best visualisation
ices_21_24 <- st_transform(ices_21_24, "epsg:3035")
ices_21_32 <- st_transform(ices_21_32, "epsg:3035")


#===

#===
# Plot Current stock areas ----
#===
plt_ple2123 <- ggplot(data = ices_21_32) +
  geom_sf(mapping = aes(fill = pleStock)) +
  geom_sf_label(mapping = aes(label = SubDivisio)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_few() +
  scale_fill_viridis(discrete = TRUE, option = "viridis")

plt_ple2123
#===
