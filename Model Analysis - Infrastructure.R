
# ABLE WA Modifiers Analysis 
# Model Analysis - INFRASTRUCTURE

###########################################################################################

# Basic Imports
library(sf)
library(tidyverse)
library(stringi)
library(ggplot2)

# Load the grid data - 5Km
grid_path = "E:/ABLE/Modifiers/Data/Modifier Database/Grid_WA/"
grid_5km = st_read(paste0(grid_path, "Base Grid/WA_Grid_5km.shp"))
grid_1km = st_read(paste0(grid_path, "Base Grid/WA_Grid_1km_v2.shp"))

grid_5km$Id = seq(1, nrow(grid_5km), 1)
grid_1km$Id = seq(1, nrow(grid_1km), 1)

grid_in_use = grid_1km
points_in_use = st_centroid(grid_in_use)

###########################################################################################

# Controls 

# Num of layers
p1_layers = 7
p2_layers = 6
p3_layers = 4

# Buffer
p1_buffer = 2500 # m

p2_buffer_1 = 100 # m
p2_buffer_2 = 100000 # m
p2_buffer_3 = 500000

p3_buffer = 100000 # Km

# Score
p1_zone = 0.0
p1_buffer_score = 0.5
p1_out = 1.0

p2_zone = 0.2 ####
p2_buffer_1_score = 0.2
p2_buffer_2_score = 0.8
p2_buffer_3_score = 0.5
p2_out = 0.5 ####

# p3_zone = 0.9 ###
# p3_buffer_score = 0.9
# p3_out = 0.6

# Gamma vector
Gamma_vector <- c(0.9)
Id = seq(1:length(st_geometry(grid_in_use)))

# Functions 

fuzzyGammaValue <- function(gamma, ...) {
  x <- do.call(cbind, list(...)) 
  row_vals <- 1 - x 
  row_prod <- apply(row_vals, 1, prod) 
  fuzzy_sum <- 1 - row_prod 
  fuzzy_prod <- apply(x, 1, prod) 
  data.frame("score" = fuzzy_sum^gamma * fuzzy_prod^(1 - gamma)) 
}

################################ READING DATA ###################################

############# P1 data files #############

p1_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/Priority 1/"
folder_names = list.dirs(path = p1_path)

abs_urban                 = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
major_power_stations      = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
maritime_facilities       = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
contaminated_sites        = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))
main_airports             = st_read(paste0(folder_names[6], "/", unlist(strsplit(folder_names[6], "/"))[9], ".shp"))
major_aviation_terminals  = st_read(paste0(folder_names[7], "/", unlist(strsplit(folder_names[7], "/"))[9], ".shp"))
sensitive_infrastructure  = st_read(paste0(folder_names[8], "/", unlist(strsplit(folder_names[8], "/"))[9], ".shp"))

############# P2 data files #############

p2_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/Priority 2/"
folder_names = list.dirs(path = p2_path)

dmirs_gas_infra             = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
GA_found_elec_greater_110kV = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
northern_mineral_rail       = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
southern_public_rail        = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))
main_roads_state            = st_read(paste0(folder_names[6], "/", unlist(strsplit(folder_names[6], "/"))[9], ".shp"))
wa_railway_stations         = st_read(paste0(folder_names[7], "/", unlist(strsplit(folder_names[7], "/"))[9], ".shp"))

############# P3 data files #############
# 
# p3_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/Priority 3/"
# folder_names = list.dirs(path = p3_path)
# 
# distribution_overhead = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
# GA_found_11_88kV      = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
# main_roads_other      = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
# # Drops the 'M' in the geometry
# main_roads_other      = st_zm(main_roads_other, "M")
# water_linear_infra    = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))

################################ CROPPING THE DATA #################################

############# P1 data files #############

abs_urban_cropped                = st_crop(abs_urban, grid_in_use)
major_power_stations_cropped     = st_crop(major_power_stations, grid_in_use)
maritime_facilities_cropped      = st_crop(maritime_facilities, grid_in_use)
contaminated_sites_cropped       = st_crop(contaminated_sites, grid_in_use)
main_airports_cropped            = st_crop(main_airports, grid_in_use)
major_aviation_terminals_cropped = st_crop(major_aviation_terminals, grid_in_use)
sensitive_infrastructure_cropped = st_crop(sensitive_infrastructure, grid_in_use)

############# P2 data files #############

dmirs_gas_infra_cropped             = st_crop(dmirs_gas_infra, grid_in_use)
GA_found_elec_greater_110kV_cropped = st_crop(GA_found_elec_greater_110kV, grid_in_use)
northern_mineral_rail_cropped       = st_crop(northern_mineral_rail, grid_in_use)
southern_public_rail_cropped        = st_crop(southern_public_rail, grid_in_use)
main_roads_state_cropped            = st_crop(main_roads_state, grid_in_use)
wa_railway_stations_cropped         = st_crop(wa_railway_stations, grid_in_use)

############# P3 data files #############
# 
# distribution_overhead_cropped = st_crop(distribution_overhead, grid_in_use)
# GA_found_11_88kV_cropped      = st_crop(GA_found_11_88kV, grid_in_use)
# main_roads_other_cropped      = st_crop(main_roads_other, grid_in_use)
# water_linear_infra_cropped    = st_crop(water_linear_infra, grid_in_use)

################################ PROCESSING PRIORITY 3 ##########################################
# 
# # distribution_overhead_cropped ##########################################################
# 
# # Create a copy grid in use
# distrib_overhead_cropped_shp = grid_in_use
# 
# # Get the nearest feature points 
# distrib_overhead_cropped_near_feat = st_nearest_feature(points_in_use, distribution_overhead_cropped)
# 
# # get the distance to each grid points up to 3 decimal places
# distrib_overhead_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
#                                                             distribution_overhead_cropped[distrib_overhead_cropped_near_feat,], 
#                                                             by_element = TRUE)), digits = 3)
# # Assign grid-score
# distrib_overhead_cropped_shp$score = 0.0
# distrib_overhead_cropped_shp[distrib_overhead_cropped_shp$dist==0, "score"] = p3_zone
# distrib_overhead_cropped_shp[distrib_overhead_cropped_shp$dist<p3_buffer & distrib_overhead_cropped_shp$dist>0, "score"] = p3_buffer_score
# distrib_overhead_cropped_shp[distrib_overhead_cropped_shp$dist>p3_buffer, "score"] = p3_out
# 
# # GA_found_11_88kV_cropped ##########################################################
# 
# # Create a copy grid in use
# GA_found_11_88kV_cropped_shp = grid_in_use
# 
# # Get the nearest feature points 
# GA_found_11_88kV_cropped_near_feat = st_nearest_feature(points_in_use, GA_found_11_88kV_cropped)
# 
# # get the distance to each grid points up to 3 decimal places
# GA_found_11_88kV_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
#                                                                  GA_found_11_88kV_cropped[GA_found_11_88kV_cropped_near_feat,], 
#                                                                  by_element = TRUE)), digits = 3)
# # Assign grid-score
# GA_found_11_88kV_cropped_shp$score = 0.0
# GA_found_11_88kV_cropped_shp[GA_found_11_88kV_cropped_shp$dist==0, "score"] = p3_zone
# GA_found_11_88kV_cropped_shp[GA_found_11_88kV_cropped_shp$dist<p3_buffer & GA_found_11_88kV_cropped_shp$dist>0, "score"] = p3_buffer_score
# GA_found_11_88kV_cropped_shp[GA_found_11_88kV_cropped_shp$dist>p3_buffer, "score"] = p3_out
# 
# # main_roads_other_cropped ##########################################################
# 
# # Create a copy grid in use
# main_roads_other_cropped_shp = grid_in_use
# 
# # Get the nearest feature points 
# main_roads_other_cropped_near_feat = st_nearest_feature(points_in_use, main_roads_other_cropped)
# 
# # get the distance to each grid points up to 3 decimal places
# main_roads_other_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
#                                                                  main_roads_other_cropped[main_roads_other_cropped_near_feat,], 
#                                                                  by_element = TRUE)), digits = 3)
# # Assign grid-score
# main_roads_other_cropped_shp$score = 0.0
# main_roads_other_cropped_shp[main_roads_other_cropped_shp$dist==0, "score"] = p3_zone
# main_roads_other_cropped_shp[main_roads_other_cropped_shp$dist<p3_buffer & main_roads_other_cropped_shp$dist>0, "score"] = p3_buffer_score
# main_roads_other_cropped_shp[main_roads_other_cropped_shp$dist>p3_buffer, "score"] = p3_out
# 
# # water_linear_infra_cropped ##########################################################
# 
# # Create a copy grid in use
# water_linear_infra_cropped_shp = grid_in_use
# 
# # Get the nearest feature points 
# water_linear_infra_cropped_near_feat = st_nearest_feature(points_in_use, water_linear_infra_cropped)
# 
# # get the distance to each grid points up to 3 decimal places
# water_linear_infra_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
#                                                                  water_linear_infra_cropped[water_linear_infra_cropped_near_feat,], 
#                                                                  by_element = TRUE)), digits = 3)
# # Assign grid-score
# water_linear_infra_cropped_shp$score = 0.0
# water_linear_infra_cropped_shp[water_linear_infra_cropped_shp$dist==0, "score"] = p3_zone
# water_linear_infra_cropped_shp[water_linear_infra_cropped_shp$dist<p3_buffer & water_linear_infra_cropped_shp$dist>0, "score"] = p3_buffer_score
# water_linear_infra_cropped_shp[water_linear_infra_cropped_shp$dist>p3_buffer, "score"] = p3_out
# 
# 
# # Priority 3 ######################################## 
# 
# priority_3_shp = cbind(Id ,st_geometry(grid_in_use), round(fuzzyGammaValue(Gamma_vector[1], distrib_overhead_cropped_shp$score, GA_found_11_88kV_cropped_shp$score, 
#                                  main_roads_other_cropped_shp$score, water_linear_infra_cropped_shp$score), digits =4))


################################ PROCESSING PRIORITY 2 ####################################

# dmirs_gas_infra_cropped ##############################################################

# Create a copy grid in use
dmirs_gas_infra_cropped_shp = grid_in_use

# Get the nearest feature points 
dmirs_gas_infra_cropped_near_feat = st_nearest_feature(points_in_use, dmirs_gas_infra_cropped)

# get the distance to each grid points up to 3 decimal places
dmirs_gas_infra_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                 dmirs_gas_infra_cropped[dmirs_gas_infra_cropped_near_feat,], 
                                                                 by_element = TRUE)), digits = 3)
# Assign grid-score
dmirs_gas_infra_cropped_shp$score = 0.0
dmirs_gas_infra_cropped_shp[dmirs_gas_infra_cropped_shp$dist==0, "score"] = p2_zone
dmirs_gas_infra_cropped_shp[dmirs_gas_infra_cropped_shp$dist<p2_buffer_1 & dmirs_gas_infra_cropped_shp$dist>0, "score"] = p2_buffer_1_score
dmirs_gas_infra_cropped_shp[dmirs_gas_infra_cropped_shp$dist<p2_buffer_2 & dmirs_gas_infra_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
dmirs_gas_infra_cropped_shp[dmirs_gas_infra_cropped_shp$dist<p2_buffer_3 & dmirs_gas_infra_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
dmirs_gas_infra_cropped_shp[dmirs_gas_infra_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# GA_found_elec_greater_110kV_cropped #####################################################

# Create a copy grid in use
GA_found_elec_greater_110kV_cropped_shp = grid_in_use

# Get the nearest feature points 
GA_found_elec_greater_110kV_cropped_near_feat = st_nearest_feature(points_in_use, GA_found_elec_greater_110kV_cropped)

# get the distance to each grid points up to 3 decimal places
GA_found_elec_greater_110kV_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                GA_found_elec_greater_110kV_cropped[GA_found_elec_greater_110kV_cropped_near_feat,], 
                                                                by_element = TRUE)), digits = 3)
# Assign grid-score
GA_found_elec_greater_110kV_cropped_shp$score = 0.0
GA_found_elec_greater_110kV_cropped_shp[GA_found_elec_greater_110kV_cropped_shp$dist==0, "score"] = p2_zone
GA_found_elec_greater_110kV_cropped_shp[GA_found_elec_greater_110kV_cropped_shp$dist<p2_buffer_1 & GA_found_elec_greater_110kV_cropped_shp$dist>0, "score"] = p2_buffer_1_score
GA_found_elec_greater_110kV_cropped_shp[GA_found_elec_greater_110kV_cropped_shp$dist<p2_buffer_2 & GA_found_elec_greater_110kV_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
GA_found_elec_greater_110kV_cropped_shp[GA_found_elec_greater_110kV_cropped_shp$dist<p2_buffer_3 & GA_found_elec_greater_110kV_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
GA_found_elec_greater_110kV_cropped_shp[GA_found_elec_greater_110kV_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# northern_mineral_rail_cropped #####################################################

# Create a copy grid in use
northern_mineral_rail_cropped_shp = grid_in_use

# Get the nearest feature points 
northern_mineral_rail_cropped_near_feat = st_nearest_feature(points_in_use, northern_mineral_rail_cropped)

# get the distance to each grid points up to 3 decimal places
northern_mineral_rail_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                            northern_mineral_rail_cropped[northern_mineral_rail_cropped_near_feat,], 
                                                                            by_element = TRUE)), digits = 3)
# Assign grid-score
northern_mineral_rail_cropped_shp$score = 0.0
northern_mineral_rail_cropped_shp[northern_mineral_rail_cropped_shp$dist==0, "score"] = p2_zone
northern_mineral_rail_cropped_shp[northern_mineral_rail_cropped_shp$dist<p2_buffer_1 & northern_mineral_rail_cropped_shp$dist>0, "score"] = p2_buffer_1_score
northern_mineral_rail_cropped_shp[northern_mineral_rail_cropped_shp$dist<p2_buffer_2 & northern_mineral_rail_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
northern_mineral_rail_cropped_shp[northern_mineral_rail_cropped_shp$dist<p2_buffer_3 & northern_mineral_rail_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
northern_mineral_rail_cropped_shp[northern_mineral_rail_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# southern_public_rail_cropped #####################################################

# Create a copy grid in use
southern_public_rail_cropped_shp = grid_in_use

# Get the nearest feature points 
southern_public_rail_cropped_near_feat = st_nearest_feature(points_in_use, southern_public_rail_cropped)

# get the distance to each grid points up to 3 decimal places
southern_public_rail_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      southern_public_rail_cropped[southern_public_rail_cropped_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
southern_public_rail_cropped_shp$score = 0.0
southern_public_rail_cropped_shp[southern_public_rail_cropped_shp$dist==0, "score"] = p2_zone
southern_public_rail_cropped_shp[southern_public_rail_cropped_shp$dist<p2_buffer_1 & southern_public_rail_cropped_shp$dist>0, "score"] = p2_buffer_1_score
southern_public_rail_cropped_shp[southern_public_rail_cropped_shp$dist<p2_buffer_2 & southern_public_rail_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
southern_public_rail_cropped_shp[southern_public_rail_cropped_shp$dist<p2_buffer_3 & southern_public_rail_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
southern_public_rail_cropped_shp[southern_public_rail_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# main_roads_state_cropped #####################################################

# Create a copy grid in use
main_roads_state_cropped_shp = grid_in_use

# Get the nearest feature points 
main_roads_state_cropped_near_feat = st_nearest_feature(points_in_use, main_roads_state_cropped)

# get the distance to each grid points up to 3 decimal places
main_roads_state_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                     main_roads_state_cropped[main_roads_state_cropped_near_feat,], 
                                                                     by_element = TRUE)), digits = 3)
# Assign grid-score
main_roads_state_cropped_shp$score = 0.0
main_roads_state_cropped_shp[main_roads_state_cropped_shp$dist==0, "score"] = p2_zone
main_roads_state_cropped_shp[main_roads_state_cropped_shp$dist<p2_buffer_1 & main_roads_state_cropped_shp$dist>0, "score"] = p2_buffer_1_score
main_roads_state_cropped_shp[main_roads_state_cropped_shp$dist<p2_buffer_2 & main_roads_state_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
main_roads_state_cropped_shp[main_roads_state_cropped_shp$dist<p2_buffer_3 & main_roads_state_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
main_roads_state_cropped_shp[main_roads_state_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# wa_railway_stations_cropped #####################################################

# Create a copy grid in use
wa_railway_stations_cropped_shp = grid_in_use

# Get the nearest feature points 
wa_railway_stations_cropped_near_feat = st_nearest_feature(points_in_use, wa_railway_stations_cropped)

# get the distance to each grid points up to 3 decimal places
wa_railway_stations_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                 wa_railway_stations_cropped[wa_railway_stations_cropped_near_feat,], 
                                                                 by_element = TRUE)), digits = 3)
# Assign grid-score
wa_railway_stations_cropped_shp$score = 0.0
wa_railway_stations_cropped_shp[wa_railway_stations_cropped_shp$dist==0, "score"] = p2_zone
wa_railway_stations_cropped_shp[wa_railway_stations_cropped_shp$dist<p2_buffer_1 & wa_railway_stations_cropped_shp$dist>0, "score"] = p2_buffer_1_score
wa_railway_stations_cropped_shp[wa_railway_stations_cropped_shp$dist<p2_buffer_2 & wa_railway_stations_cropped_shp$dist>p2_buffer_1, "score"] = p2_buffer_2_score
wa_railway_stations_cropped_shp[wa_railway_stations_cropped_shp$dist<p2_buffer_3 & wa_railway_stations_cropped_shp$dist>p2_buffer_2, "score"] = p2_buffer_3_score
wa_railway_stations_cropped_shp[wa_railway_stations_cropped_shp$dist>p2_buffer_3, "score"] = p2_out

# Priority 2  ######################################## 

priority_2_shp = cbind(Id, st_geometry(grid_in_use), round(fuzzyGammaValue(Gamma_vector[1], 
                                                                       dmirs_gas_infra_cropped_shp$score, 
                                                                       GA_found_elec_greater_110kV_cropped_shp$score, 
                                                                       northern_mineral_rail_cropped_shp$score,
                                                                       southern_public_rail_cropped_shp$score, 
                                                                       main_roads_state_cropped_shp$score, 
                                                                       wa_railway_stations_cropped_shp$score), digits =4))

################################ PROCESSING PRIORITY 1 ####################################

# abs_urban_cropped #################################################################

# Create a copy grid in use
abs_urban_cropped_shp = grid_in_use

# Get the nearest feature points 
abs_urban_cropped_near_feat = st_nearest_feature(points_in_use, abs_urban_cropped)

# Takes around 40 to 50 mins to rn
# get the distance to each grid points up to 3 decimal places
abs_urban_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                          abs_urban_cropped[abs_urban_cropped_near_feat,], 
                                          by_element = TRUE)), digits = 3)
# Assign grid-score
abs_urban_cropped_shp$score = 0.0
abs_urban_cropped_shp[abs_urban_cropped_shp$dist==0, "score"] = p1_zone
abs_urban_cropped_shp[abs_urban_cropped_shp$dist<p1_buffer & abs_urban_cropped_shp$dist>0, "score"] = p1_buffer_score
abs_urban_cropped_shp[abs_urban_cropped_shp$dist>p1_buffer, "score"] = p1_out

# major_power_stations_cropped #################################################################

# Create a copy grid in use
major_power_stations_cropped_shp = grid_in_use

# Get the nearest feature points 
major_power_stations_cropped_near_feat = st_nearest_feature(points_in_use, major_power_stations_cropped)

# get the distance to each grid points up to 3 decimal places
major_power_stations_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                          major_power_stations_cropped[major_power_stations_cropped_near_feat,], 
                                                          by_element = TRUE)), digits = 3)
# Assign grid-score
major_power_stations_cropped_shp$score = 0.0
major_power_stations_cropped_shp[major_power_stations_cropped_shp$dist==0, "score"] = p1_zone
major_power_stations_cropped_shp[major_power_stations_cropped_shp$dist<p1_buffer & major_power_stations_cropped_shp$dist>0, "score"] = p1_buffer_score
major_power_stations_cropped_shp[major_power_stations_cropped_shp$dist>p1_buffer, "score"] = p1_out

# maritime_facilities_cropped #################################################################

# Create a copy grid in use
maritime_facilities_cropped_shp = grid_in_use

# Get the nearest feature points 
maritime_facilities_cropped_near_feat = st_nearest_feature(points_in_use, maritime_facilities_cropped)

# get the distance to each grid points up to 3 decimal places
maritime_facilities_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                     maritime_facilities_cropped[maritime_facilities_cropped_near_feat,], 
                                                                     by_element = TRUE)), digits = 3)
# Assign grid-score
maritime_facilities_cropped_shp$score = 0.0
maritime_facilities_cropped_shp[maritime_facilities_cropped_shp$dist==0, "score"] = p1_zone
maritime_facilities_cropped_shp[maritime_facilities_cropped_shp$dist<p1_buffer & maritime_facilities_cropped_shp$dist>0, "score"] = p1_buffer_score
maritime_facilities_cropped_shp[maritime_facilities_cropped_shp$dist>p1_buffer, "score"] = p1_out

# contaminated_sites_cropped #################################################################

# Create a copy grid in use
contaminated_sites_cropped_shp = grid_in_use

# Get the nearest feature points 
contaminated_sites_cropped_near_feat = st_nearest_feature(points_in_use, contaminated_sites_cropped)

# get the distance to each grid points up to 3 decimal places
contaminated_sites_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                    contaminated_sites_cropped[contaminated_sites_cropped_near_feat,], 
                                                                    by_element = TRUE)), digits = 3)
# Assign grid-score
contaminated_sites_cropped_shp$score = 0.0
contaminated_sites_cropped_shp[contaminated_sites_cropped_shp$dist==0, "score"] = p1_zone
contaminated_sites_cropped_shp[contaminated_sites_cropped_shp$dist<p1_buffer & contaminated_sites_cropped_shp$dist>0, "score"] = p1_buffer_score
contaminated_sites_cropped_shp[contaminated_sites_cropped_shp$dist>p1_buffer, "score"] = p1_out

# main_airports_cropped #################################################################

# Create a copy grid in use
main_airports_cropped_shp = grid_in_use

# Get the nearest feature points 
main_airports_cropped_near_feat = st_nearest_feature(points_in_use, main_airports_cropped)

# get the distance to each grid points up to 3 decimal places
main_airports_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                   main_airports_cropped[main_airports_cropped_near_feat,], 
                                                                   by_element = TRUE)), digits = 3)
# Assign grid-score
main_airports_cropped_shp$score = 0.0
main_airports_cropped_shp[main_airports_cropped_shp$dist==0, "score"] = p1_zone
main_airports_cropped_shp[main_airports_cropped_shp$dist<p1_buffer & main_airports_cropped_shp$dist>0, "score"] = p1_buffer_score
main_airports_cropped_shp[main_airports_cropped_shp$dist>p1_buffer, "score"] = p1_out

# major_aviation_terminals_cropped #################################################################

# Create a copy grid in use
major_aviation_terminals_cropped_shp = grid_in_use

# Get the nearest feature points 
major_aviation_terminals_cropped_near_feat = st_nearest_feature(points_in_use, major_aviation_terminals_cropped)

# get the distance to each grid points up to 3 decimal places
major_aviation_terminals_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                              major_aviation_terminals_cropped[major_aviation_terminals_cropped_near_feat,], 
                                                              by_element = TRUE)), digits = 3)
# Assign grid-score
major_aviation_terminals_cropped_shp$score = 0.0
major_aviation_terminals_cropped_shp[major_aviation_terminals_cropped_shp$dist==0, "score"] = p1_zone
major_aviation_terminals_cropped_shp[major_aviation_terminals_cropped_shp$dist<p1_buffer & major_aviation_terminals_cropped_shp$dist>0, "score"] = p1_buffer_score
major_aviation_terminals_cropped_shp[major_aviation_terminals_cropped_shp$dist>p1_buffer, "score"] = p1_out

# sensitive_infrastructure_cropped #################################################################

# Create a copy grid in use
sensitive_infrastructure_cropped_shp = grid_in_use

# Get the nearest feature points 
sensitive_infrastructure_cropped_near_feat = st_nearest_feature(points_in_use, sensitive_infrastructure_cropped)

# get the distance to each grid points up to 3 decimal places
sensitive_infrastructure_cropped_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                         sensitive_infrastructure_cropped[sensitive_infrastructure_cropped_near_feat,], 
                                                                         by_element = TRUE)), digits = 3)
# Assign grid-score
sensitive_infrastructure_cropped_shp$score = 0.0
sensitive_infrastructure_cropped_shp[sensitive_infrastructure_cropped_shp$dist==0, "score"] = p1_zone
sensitive_infrastructure_cropped_shp[sensitive_infrastructure_cropped_shp$dist<p1_buffer & sensitive_infrastructure_cropped_shp$dist>0, "score"] = p1_buffer_score
sensitive_infrastructure_cropped_shp[sensitive_infrastructure_cropped_shp$dist>p1_buffer, "score"] = p1_out


######################################################################################################

# Priority 

priority = do.call(cbind, list( abs_urban_cropped_shp$score, 
                                major_power_stations_cropped_shp$score, 
                                maritime_facilities_cropped_shp$score,
                                contaminated_sites_cropped_shp$score, 
                                main_airports_cropped_shp$score, 
                                major_aviation_terminals_cropped_shp$score, 
                                sensitive_infrastructure_cropped_shp$score, 
                                priority_2_shp$score))

score = apply(priority, 1, min)


priority_score = st_as_sf(cbind(Id, data.frame(st_geometry(grid_in_use)), score))
unique(priority_score$score)


priority_score_map = ggplot() + geom_sf(data= priority_score, aes(fill= .data[[names(priority_score)[2]]]), color= NA) + 
                                scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw() + 
                                ggtitle("Priority map for Infrastructure")

#####################################################################################################

# Save_files

save_path = "E:/ABLE/Modifiers/Outputs/"
dir.create(paste0(save_path, "Infrastructure", "/"))
save_path = paste0(save_path, "Infrastructure/1km Grid/")

dir.create(paste0(save_path, "figures", "/"))

########### Priority 3 ############### 

# distrib_overhead_cropped 
# GA_found_11_88kV_cropped, 
# main_roads_other_cropped, 
# water_linear_infra_cropped

# dir.create(paste0(save_path, "figures/Priority 3/"))
# path = paste0(save_path, "figures/Priority 3/")
# 
# colm = 4
# p = main_roads_other_cropped_shp
# name = paste0("main_roads_other_cropped", ".jpeg")
# 
# map = ggplot() + geom_sf(data= p, aes(fill= .data[[names(p)[colm]]]), color= NA) +
#   scale_fill_gradientn(colours = c("red", "blue"), limits= c(0,1))+ theme_bw()
# 
# ggsave(paste0(path, name), map, width = 10, height = 10)

########### Priority 2 ############### 

# dmirs_gas_infra_cropped 
# GA_found_elec_greater_110kV_cropped, 
# northern_mineral_rail_cropped, 
# southern_public_rail_cropped
# main_roads_state_cropped
# wa_railway_stations_cropped

# DMIRS Gas Infrastructure
# GA electric greater than 110kV
# Northern Mineral Railway
# Southern Public Railway
# Main Roads - State
# WA Railway Stations


dir.create(paste0(save_path, "figures/Priority 2/"))
path = paste0(save_path, "figures/Priority 2/")

colm = 5
p = main_roads_state_cropped_shp
name = paste0("Main Roads - State", ".jpeg")

map = ggplot() + geom_sf(data= p, aes(fill= .data[[names(p)[colm]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw() + 
  ggtitle(paste0("Main Roads - State", " - Priority 2"))

ggsave(paste0(path, name), map, width = 10, height = 10)

########### Priority 1 ############### 

# abs_urban_cropped, 
# major_power_stations_cropped, 
# maritime_facilities_cropped,
# contaminated_sites_cropped, 
# main_airports_cropped, 
# major_aviation_terminals_cropped, 
# sensitive_infrastructure_cropped

# ABS Urban 
# Major Power Stations 
# Maritime Facilities
# Contaminated Sites, 
# Main Airports, 
# Major Aviation Terminals, 
# Sensitive Infrastructure

dir.create(paste0(save_path, "figures/Priority 1/"))
path = paste0(save_path, "figures/Priority 1/")

colm = 5
p = major_aviation_terminals_cropped_shp
name = paste0("Major Aviation Terminals", ".jpeg")

map = ggplot() + geom_sf(data= p, aes(fill= .data[[names(p)[colm]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw()+ 
  ggtitle(paste0("Major Aviation Terminals", " - Priority 1"))

ggsave(paste0(path, name), map, width = 10, height = 10)

########### Priority ################# 

# Priority 2
colm = 2
p = st_as_sf(priority_2_shp)
name = "Priority 2.jpeg"

map = ggplot() + geom_sf(data= p, aes(fill= .data[[names(p)[colm]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw() +
  ggtitle("Priority 2 map for Infrastructure")

ggsave(paste0(save_path, name), map, width = 10, height = 10)

## Priority 2 and 3
# colm = 2
# p = st_as_sf(priority_2_3_shp)
# name = "Priority_2_and_3.jpeg"
# 
# map = ggplot() + geom_sf(data= p, aes(fill= .data[[names(p)[colm]]]), color= NA) +
#   scale_fill_gradientn(colours = c("red", "blue"), limits= c(0,1))+ theme_bw()
# 
# ggsave(paste0(path, name), map, width = 10, height = 10)


# Save
name = "Priority Map Infrastructure.jpeg"
ggsave(paste0(save_path, "figures/", name), priority_score_map, width = 10, height = 10)


dir.create(paste0(save_path, 
                  "/Priority score - Infrastructure/"))
# Save file 
st_write(priority_score, paste0(save_path, 
                                "/Priority score - Infrastructure/", 
                                "Priority score - Infrastructure.shp") )

#####################################################################################################

# CREATING VECTOR FILES - PRIORITY 1 ########################################

dmirs_gas_infra_buf         = st_buffer(dmirs_gas_infra, p2_buffer_1)
GA_fnd_elec_gtr_110kV_buf   = st_buffer(GA_found_elec_greater_110kV, p2_buffer_1)
northern_mineral_rail_buf   = st_buffer(northern_mineral_rail, p2_buffer_1)
southern_public_rail_buf    = st_buffer(southern_public_rail, p2_buffer_1)
main_roads_state_buf        = st_buffer(main_roads_state, p2_buffer_1)
wa_railway_stations_buf     = st_buffer(wa_railway_stations, p2_buffer_1)


dmirs_gas_infra_vect        = ggplot() + geom_sf(data=dmirs_gas_infra_buf) + ggtitle("DMIRS Gas Infrastructure")
GA_fnd_elec_gtr_110kV_vect  = ggplot() + geom_sf(data=GA_fnd_elec_gtr_110kV_buf) + ggtitle("GA Electric greater than 110kV")
northern_mineral_rail_vect  = ggplot() + geom_sf(data=northern_mineral_rail_buf) + ggtitle("Northern Mineral Railway")
southern_public_rail_vect   = ggplot() + geom_sf(data=southern_public_rail_buf) + ggtitle("Southern Public Railway")
main_roads_state_vect       = ggplot() + geom_sf(data=main_roads_state_buf) + ggtitle("Main Roads - State")
wa_railway_stations_vect    = ggplot() + geom_sf(data=wa_railway_stations_buf) + ggtitle("WA Railway Stations")


priority_2_vect            =  ggplot() + geom_sf(data=dmirs_gas_infra_buf) +
                              geom_sf(data=GA_fnd_elec_gtr_110kV_buf) +
                              geom_sf(data=northern_mineral_rail_buf) +
                              geom_sf(data=southern_public_rail_buf) + 
                              geom_sf(data=main_roads_state_buf) + 
                              geom_sf(data=wa_railway_stations_buf) + 
                              ggtitle("Priority 2 Vector")


priority_2_vect_color      =  ggplot() + geom_sf(data=dmirs_gas_infra_buf, color = "red") +
                              geom_sf(data=GA_fnd_elec_gtr_110kV_buf, color = "orange") +
                              geom_sf(data=northern_mineral_rail_buf, color = "yellow") +
                              geom_sf(data=southern_public_rail_buf, color = "green") + 
                              geom_sf(data=main_roads_state_buf, color = "blue") + 
                              geom_sf(data=wa_railway_stations_buf, color = "violet")+
                              ggtitle("Priority 2 Vector")

save_path = "E:/ABLE/Modifiers/Outputs/Infrastructure/"

dir.create(paste0(save_path, "vectors/"))
path = paste0(save_path, "vectors/")

name = paste0("Priority 2 Vector color", ".jpeg")
ggsave(paste0(path, name), priority_2_vect_color, width = 10, height = 10)


overlay = ggplot() + geom_sf(data= priority_score, aes(fill= .data[[names(priority_score)[2]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw() + geom_sf(data=dmirs_gas_infra_buf) +
  geom_sf(data=GA_fnd_elec_gtr_110kV_buf) +
  geom_sf(data=northern_mineral_rail_buf) +
  geom_sf(data=southern_public_rail_buf) + 
  geom_sf(data=main_roads_state_buf) + 
  geom_sf(data=wa_railway_stations_buf) + ggtitle("Final plot overlay with Priority 2 vectors")


name = paste0("Final plot overlay with Priority 2 vectors", ".jpeg")
ggsave(paste0(save_path, name), overlay, width = 10, height = 10)







