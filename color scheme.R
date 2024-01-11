# Basic Imports
library(sf)
library(tidyverse)
library(stringi)
library(ggplot2)
library(colorspace)
library(farver)

# Data Import
path = "E:/ABLE/Modifiers/Outputs/Infrastructure/Grid 5km/Priority score - Infrastructure/"
file = st_read(paste0(path, "Priority score - Infrastructure.shp"))

# Color hex from ArcGIS 
# Adjusted for R
blue   = "#5C09FC"
cyan   = "#16fdff" 
yellow = "#F2FE1E"
red    = "#fe2208"

#  Doesn't work
blue_hcl   = decode_colour(blue, to = "hcl")
cyan_hcl   = decode_colour(cyan, to = "hcl")
yellow_hcl = decode_colour(yellow, to = "hcl")
red_hcl    = decode_colour(red, to = "hcl")
x = sequential_hcl(n = 4, h = c(blue_hcl[1], cyan_hcl[1], yellow_hcl[1], red_hcl[1]), 
                       c = c(blue_hcl[2], cyan_hcl[2], yellow_hcl[2], red_hcl[2]), 
                       l = c(blue_hcl[3], cyan_hcl[3], yellow_hcl[3], red_hcl[3]))

# Simple ggplot
try1 = c(blue, cyan, yellow, red)

# Fill in trans for customizing the distribution
priority_score_map = ggplot() + geom_sf(data= file, aes(fill= .data[[names(file)[2]]]), color= NA) + 
  scale_fill_gradientn(colours = try1, limits= c(0,1))+ 
  ggtitle("Priority map for Infrastructure")
