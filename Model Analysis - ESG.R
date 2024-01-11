
# ABLE WA Modifiers Analysis 
# Model Analysis - ESG

###########################################################################################

# Basic Imports
library(sf)
library(tidyverse)
library(stringi)
library(ggplot2)

# Load the grid data - 5Km
grid_path   = "E:/ABLE/Modifiers/Data/Modifier Database/Grid_WA/"
# grid_5km = st_read(paste0(grid_path, "Base Grid/WA_Grid_5km.shp"))
grid_1km = st_read(paste0(grid_path, "Base Grid/WA_Grid_1km_v2.shp"))

# grid_5km$Id = seq(1, nrow(grid_5km), 1)
grid_1km$Id = seq(1, nrow(grid_1km), 1)

grid_in_use = grid_1km
points_in_use = st_centroid(grid_in_use)


correct_corrupted = function(corrupt_file){
  corrupt_invalid = corrupt_file[!st_is_valid(corrupt_file), ]
  corrupt_invalid = st_make_valid(corrupt_invalid)
  corrupt_valid   = corrupt_file[st_is_valid(corrupt_file),]
  return(rbind(corrupt_invalid, corrupt_valid))
}

##########################################################################################

# Controls 

# Num of layers
p1_layers = 7
p2_layers = 24
p3_layers = 20 # Not included 
p3_layers = 6  # Not included


Id = seq(1:length(st_geometry(grid_in_use)))

###########################################################################################

############# P1 data files

p1_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/Priority 1/"
folder_names = list.dirs(path = p1_path)

commonwealth_heritage          = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
IUCN_protected_area_cats_I_III = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
national_heritage              = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
ramsar_sites                   = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))
aboriginal_heritage_places     = st_read(paste0(folder_names[6], "/", unlist(strsplit(folder_names[6], "/"))[9], ".shp"))
UNESCO_bio_reserves_core_areas = st_read(paste0(folder_names[7], "/", unlist(strsplit(folder_names[7], "/"))[9], ".shp"))
world_heritage_sites           = st_read(paste0(folder_names[8], "/", unlist(strsplit(folder_names[8], "/"))[9], ".shp"))

############# P2 data files

p2_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/Priority 2/"
folder_names = list.dirs(path = p2_path)

CAPAD_51gReserve            = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
CAPAD_51hReserve            = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
CAPAD_BotanicGardens        = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
CAPAD_ConservationCovenant  = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))
CAPAD_ConservationPark      = st_read(paste0(folder_names[6], "/", unlist(strsplit(folder_names[6], "/"))[9], ".shp"))
CAPAD_ConservationReserve   = st_read(paste0(folder_names[7], "/", unlist(strsplit(folder_names[7], "/"))[9], ".shp"))
CAPAD_ManagementArea        = st_read(paste0(folder_names[8], "/", unlist(strsplit(folder_names[8], "/"))[9], ".shp"))
CAPAD_NationalPark          = st_read(paste0(folder_names[9], "/", unlist(strsplit(folder_names[9], "/"))[9], ".shp"))
CAPAD_NatureReserve         = st_read(paste0(folder_names[10], "/", unlist(strsplit(folder_names[10], "/"))[9], ".shp"))
CAPAD_NRS_add_gazProgress   = st_read(paste0(folder_names[11], "/", unlist(strsplit(folder_names[11], "/"))[9], ".shp"))
CAPAD_Other                 = st_read(paste0(folder_names[12], "/", unlist(strsplit(folder_names[12], "/"))[9], ".shp"))
CAPAD_pvtNatureReserve      = st_read(paste0(folder_names[13], "/", unlist(strsplit(folder_names[13], "/"))[9], ".shp"))
CAPAD_StateReserve          = st_read(paste0(folder_names[14], "/", unlist(strsplit(folder_names[14], "/"))[9], ".shp"))
eco_communities             = st_read(paste0(folder_names[15], "/", unlist(strsplit(folder_names[15], "/"))[9], ".shp"))
IUCN_protected_cats_IV      = st_read(paste0(folder_names[16], "/", unlist(strsplit(folder_names[16], "/"))[9], ".shp"))
listed_migratory_species    = st_read(paste0(folder_names[17], "/", unlist(strsplit(folder_names[17], "/"))[9], ".shp"))
listed_threatned_species    = st_read(paste0(folder_names[18], "/", unlist(strsplit(folder_names[18], "/"))[9], ".shp"))
section_19                  = st_read(paste0(folder_names[19], "/", unlist(strsplit(folder_names[19], "/"))[9], ".shp"))
aboriginal_heritage_places  = st_read(paste0(folder_names[20], "/", unlist(strsplit(folder_names[20], "/"))[9], ".shp"))
aboriginal_land_estate      = st_read(paste0(folder_names[21], "/", unlist(strsplit(folder_names[21], "/"))[9], ".shp"))
threatened_priority_fauna   = st_read(paste0(folder_names[22], "/", unlist(strsplit(folder_names[22], "/"))[9], ".shp"))
threatened_priority_flora   = st_read(paste0(folder_names[23], "/", unlist(strsplit(folder_names[23], "/"))[9], ".shp"))
threatened_eco_community    = st_read(paste0(folder_names[24], "/", unlist(strsplit(folder_names[24], "/"))[9], ".shp"))
UNESCO_bio_reserves_buffer  = st_read(paste0(folder_names[25], "/", unlist(strsplit(folder_names[25], "/"))[9], ".shp"))

############# P3 data files

p3_path = "E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/Priority 3/"
folder_names = list.dirs(path = p3_path)

crown_freehold_dept         = st_read(paste0(folder_names[2], "/", unlist(strsplit(folder_names[2], "/"))[9], ".shp"))
native_title                = st_read(paste0(folder_names[3], "/", unlist(strsplit(folder_names[3], "/"))[9], ".shp"))
native_title_fa_notices     = st_read(paste0(folder_names[4], "/", unlist(strsplit(folder_names[4], "/"))[9], ".shp"))
native_title_fa_objections  = st_read(paste0(folder_names[5], "/", unlist(strsplit(folder_names[5], "/"))[9], ".shp"))
native_title_fada           = st_read(paste0(folder_names[6], "/", unlist(strsplit(folder_names[6], "/"))[9], ".shp"))
native_title_ILUA_register  = st_read(paste0(folder_names[7], "/", unlist(strsplit(folder_names[7], "/"))[9], ".shp"))
native_title_ntd_register   = st_read(paste0(folder_names[8], "/", unlist(strsplit(folder_names[8], "/"))[9], ".shp"))
native_title_ntda_register  = st_read(paste0(folder_names[9], "/", unlist(strsplit(folder_names[9], "/"))[9], ".shp"))
native_title_ntda_schedule  = st_read(paste0(folder_names[10], "/", unlist(strsplit(folder_names[10], "/"))[9], ".shp"))
native_title_ntda_transac   = st_read(paste0(folder_names[11], "/", unlist(strsplit(folder_names[11], "/"))[9], ".shp"))
native_title_ratsib         = st_read(paste0(folder_names[12], "/", unlist(strsplit(folder_names[12], "/"))[9], ".shp"))
native_title_s31_agreemnt   = st_read(paste0(folder_names[13], "/", unlist(strsplit(folder_names[13], "/"))[9], ".shp"))
rntbc_native_title          = st_read(paste0(folder_names[14], "/", unlist(strsplit(folder_names[14], "/"))[9], ".shp"))
state_forests               = st_read(paste0(folder_names[15], "/", unlist(strsplit(folder_names[15], "/"))[9], ".shp"))
timber_reserves             = st_read(paste0(folder_names[16], "/", unlist(strsplit(folder_names[16], "/"))[9], ".shp"))
aboriginal_community_town   = st_read(paste0(folder_names[17], "/", unlist(strsplit(folder_names[17], "/"))[9], ".shp"))
wa_dmirs_sec57_4            = st_read(paste0(folder_names[18], "/", unlist(strsplit(folder_names[18], "/"))[9], ".shp"))
wa_hydrography_linear       = st_read(paste0(folder_names[19], "/", unlist(strsplit(folder_names[19], "/"))[9], ".shp"))
wa_overview_towns           = st_read(paste0(folder_names[20], "/", unlist(strsplit(folder_names[20], "/"))[9], ".shp"))
wa_pastoral_stations        = st_read(paste0(folder_names[21], "/", unlist(strsplit(folder_names[21], "/"))[9], ".shp"))

###########################################################################################

# CROPPING THE DATA

############# P1 data files #############

# # Correcting corrupted IUCN_protected_area_cats_I_III file
IUCN_protected_area_cats_I_III_corrected = correct_corrupted(IUCN_protected_area_cats_I_III)

commonwealth_heritage_crp           = st_crop(commonwealth_heritage, grid_in_use)
IUCN_protected_area_cats_I_III_crp  = st_crop(IUCN_protected_area_cats_I_III, grid_in_use)
national_heritage_crp               = st_crop(national_heritage, grid_in_use)
ramsar_sites_crp                    = st_crop(ramsar_sites, grid_in_use)
aboriginal_heritage_places_crp      = st_crop(aboriginal_heritage_places, grid_in_use)
UNESCO_bio_reserves_core_areas_crp  = st_crop(UNESCO_bio_reserves_core_areas, grid_in_use)
world_heritage_sites_crp            = st_crop(world_heritage_sites, grid_in_use)

############# P2 data files #############

CAPAD_51gReserve_crp            = st_crop(CAPAD_51gReserve, grid_in_use)
CAPAD_51hReserve_crp            = st_crop(CAPAD_51hReserve, grid_in_use)
CAPAD_BotanicGardens_crp        = st_crop(CAPAD_BotanicGardens, grid_in_use)
CAPAD_ConservationCovenant_crp  = st_crop(CAPAD_ConservationCovenant, grid_in_use)
CAPAD_ConservationPark_crp      = st_crop(CAPAD_ConservationPark, grid_in_use)
CAPAD_ConservationReserve_crp   = st_crop(CAPAD_ConservationReserve, grid_in_use)
CAPAD_ManagementArea_crp        = st_crop(CAPAD_ManagementArea, grid_in_use)
CAPAD_NationalPark_crp          = st_crop(CAPAD_NationalPark, grid_in_use)
CAPAD_NatureReserve_crp         = st_crop(CAPAD_NatureReserve, grid_in_use)
CAPAD_NRS_add_gazProgress_crp   = st_crop(CAPAD_NRS_add_gazProgress, grid_in_use)
CAPAD_Other_crp                 = st_crop(CAPAD_Other, grid_in_use)
CAPAD_pvtNatureReserve_crp      = st_crop(CAPAD_pvtNatureReserve, grid_in_use)
CAPAD_StateReserve_crp          = st_crop(CAPAD_StateReserve, grid_in_use)
eco_communities_crp             = st_crop(eco_communities, grid_in_use)
IUCN_protected_cats_IV_crp      = st_crop(IUCN_protected_cats_IV, grid_in_use)
section_19_crp                  = st_crop(section_19, grid_in_use)
aboriginal_heritage_places_crp  = st_crop(aboriginal_heritage_places, grid_in_use)
aboriginal_land_estate_crp      = st_crop(aboriginal_land_estate, grid_in_use)
threatened_priority_fauna_crp   = st_crop(threatened_priority_fauna, grid_in_use)
threatened_priority_flora_crp   = st_crop(threatened_priority_flora, grid_in_use)
threatened_eco_community_crp    = st_crop(threatened_eco_community, grid_in_use)
UNESCO_bio_reserves_buffer_crp  = st_crop(UNESCO_bio_reserves_buffer, grid_in_use)

############# P3 data files #############

# Correcting corrupted 
native_title_corrected    = correct_corrupted(native_title)
nat_fa_notices_corrected  = correct_corrupted(native_title_fa_notices)
nat_ntd_reg_corrected     = correct_corrupted(native_title_ntd_register)
nat_ntda_transc_corrected = correct_corrupted(native_title_ntda_transac)


native_title_crp                = st_crop(native_title_corrected, grid_in_use)
native_title_fa_notices_crp     = st_crop(nat_fa_notices_corrected, grid_in_use)
native_title_ntd_register_crp   = st_crop(nat_ntd_reg_corrected, grid_in_use)
native_title_ntda_transac_crp   = st_crop(nat_ntda_transc_corrected, grid_in_use)


crown_freehold_dept_crp         = st_crop(crown_freehold_dept, grid_in_use)
native_title_fa_objections_crp  = st_crop(native_title_fa_objections, grid_in_use)
native_title_fada_crp           = st_crop(native_title_fada, grid_in_use)
native_title_ILUA_register_crp  = st_crop(native_title_ILUA_register, grid_in_use)
native_title_ntda_register_crp  = st_crop(native_title_ntda_register, grid_in_use)
native_title_ntda_schedule_crp  = st_crop(native_title_ntda_schedule, grid_in_use)
native_title_ratsib_crp         = st_crop(native_title_ratsib, grid_in_use)
native_title_s31_agreemnt_crp   = st_crop(native_title_s31_agreemnt, grid_in_use)
rntbc_native_title_crp          = st_crop(rntbc_native_title, grid_in_use)
state_forests_crp               = st_crop(state_forests, grid_in_use)
timber_reserves_crp             = st_crop(timber_reserves, grid_in_use)
aboriginal_community_town_crp   = st_crop(aboriginal_community_town, grid_in_use)
wa_dmirs_sec57_4_crp            = st_crop(wa_dmirs_sec57_4, grid_in_use)
wa_hydrography_linear_crp       = st_crop(wa_hydrography_linear, grid_in_use)
wa_overview_towns_crp           = st_crop(wa_overview_towns, grid_in_use)
wa_pastoral_stations_crp        = st_crop(wa_pastoral_stations, grid_in_use)

###########################################################################################

# PROCESSING PRIORITY 1

# Controls
p1_zone = 0.0
p1_buffer_score = 0.9
p1_out = 1

p1_buffer = 1000 # m

# commonwealth_heritage_crp #####################################################

# Create a copy grid in use
commonwealth_heritage_crp_shp = grid_in_use

# Get the nearest feature points 
commonwealth_heritage_crp_near_feat = st_nearest_feature(points_in_use, commonwealth_heritage_crp)

# get the distance to each grid points up to 3 decimal places
commonwealth_heritage_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                           commonwealth_heritage_crp[commonwealth_heritage_crp_near_feat,], 
                                                                           by_element = TRUE)), digits = 3)
commonwealth_heritage_crp_shp$score = 0.0
commonwealth_heritage_crp_shp[commonwealth_heritage_crp_shp$dist==0, "score"] = p1_zone
commonwealth_heritage_crp_shp[commonwealth_heritage_crp_shp$dist<p1_buffer & commonwealth_heritage_crp_shp$dist>0, "score"] = p1_buffer_score
commonwealth_heritage_crp_shp[commonwealth_heritage_crp_shp$dist>p1_buffer, "score"] = p1_out

# IUCN_protected_area_cats_I_III_crp #####################################################

# Create a copy grid in use
IUCN_protected_area_cats_I_III_crp_shp = grid_in_use

# Get the nearest feature points 
IUCN_protected_area_cats_I_III_crp_near_feat = st_nearest_feature(points_in_use, IUCN_protected_area_cats_I_III_crp)

# get the distance to each grid points up to 3 decimal places
IUCN_protected_area_cats_I_III_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                    IUCN_protected_area_cats_I_III_crp[IUCN_protected_area_cats_I_III_crp_near_feat,], 
                                                    by_element = TRUE)), digits = 3)
IUCN_protected_area_cats_I_III_crp_shp$score = 0.0
IUCN_protected_area_cats_I_III_crp_shp[IUCN_protected_area_cats_I_III_crp_shp$dist==0, "score"] = p1_zone
IUCN_protected_area_cats_I_III_crp_shp[IUCN_protected_area_cats_I_III_crp_shp$dist<p1_buffer & IUCN_protected_area_cats_I_III_crp_shp$dist>0, "score"] = p1_buffer_score
IUCN_protected_area_cats_I_III_crp_shp[IUCN_protected_area_cats_I_III_crp_shp$dist>p1_buffer, "score"] = p1_out

# national_heritage_crp #####################################################

# Create a copy grid in use
national_heritage_crp_shp = grid_in_use

# Get the nearest feature points 
national_heritage_crp_near_feat = st_nearest_feature(points_in_use, national_heritage_crp)

# get the distance to each grid points up to 3 decimal places
national_heritage_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                  national_heritage_crp[national_heritage_crp_near_feat,], 
                                                                  by_element = TRUE)), digits = 3)
national_heritage_crp_shp$score = 0.0
national_heritage_crp_shp[national_heritage_crp_shp$dist==0, "score"] = p1_zone
national_heritage_crp_shp[national_heritage_crp_shp$dist<p1_buffer & national_heritage_crp_shp$dist>0, "score"] = p1_buffer_score
national_heritage_crp_shp[national_heritage_crp_shp$dist>p1_buffer, "score"] = p1_out

# ramsar_sites_crp ####################################################################

# Create a copy grid in use
ramsar_sites_crp_shp = grid_in_use

# Get the nearest feature points 
ramsar_sites_crp_near_feat = st_nearest_feature(points_in_use, ramsar_sites_crp)

# get the distance to each grid points up to 3 decimal places
ramsar_sites_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                         ramsar_sites_crp[ramsar_sites_crp_near_feat,], 
                                                         by_element = TRUE)), digits = 3)
# Assign grid-score
ramsar_sites_crp_shp$score = 0.0
ramsar_sites_crp_shp[ramsar_sites_crp_shp$dist==0, "score"] = p1_zone
ramsar_sites_crp_shp[ramsar_sites_crp_shp$dist<p1_buffer & ramsar_sites_crp_shp$dist>0, "score"] = p1_buffer_score
ramsar_sites_crp_shp[ramsar_sites_crp_shp$dist>p1_buffer, "score"] = p1_out

# aboriginal_heritage_places_crp ##########################################################

# Create a copy grid in use
aboriginal_heritage_places_crp_shp = grid_in_use

# Get the nearest feature points 
aboriginal_heritage_places_crp_near_feat = st_nearest_feature(points_in_use, aboriginal_heritage_places_crp)

# get the distance to each grid points up to 3 decimal places
aboriginal_heritage_places_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                         aboriginal_heritage_places_crp[aboriginal_heritage_places_crp_near_feat,], 
                                                         by_element = TRUE)), digits = 3)
# Assign grid-score
aboriginal_heritage_places_crp_shp$score = 0.0
aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist==0, "score"] = p1_zone
aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist<p1_buffer & aboriginal_heritage_places_crp_shp$dist>0, "score"] = p1_buffer_score
aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist>p1_buffer, "score"] = p1_out

# UNESCO_bio_reserves_core_areas_crp ######################################################

# Create a copy grid in use
UNESCO_bio_reserves_core_areas_crp_shp = grid_in_use

# Get the nearest feature points 
UNESCO_bio_reserves_core_areas_crp_near_feat = st_nearest_feature(points_in_use, UNESCO_bio_reserves_core_areas_crp)

# get the distance to each grid points up to 3 decimal places
UNESCO_bio_reserves_core_areas_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       UNESCO_bio_reserves_core_areas_crp[UNESCO_bio_reserves_core_areas_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
UNESCO_bio_reserves_core_areas_crp_shp$score = 0.0
UNESCO_bio_reserves_core_areas_crp_shp[UNESCO_bio_reserves_core_areas_crp_shp$dist==0, "score"] = p1_zone
UNESCO_bio_reserves_core_areas_crp_shp[UNESCO_bio_reserves_core_areas_crp_shp$dist<p1_buffer & UNESCO_bio_reserves_core_areas_crp_shp$dist>0, "score"] = p1_buffer_score
UNESCO_bio_reserves_core_areas_crp_shp[UNESCO_bio_reserves_core_areas_crp_shp$dist>p1_buffer, "score"] = p1_out

# world_heritage_sites_crp ###############################################################

# Create a copy grid in use
world_heritage_sites_crp_shp = grid_in_use

# Get the nearest feature points 
UNESCO_bio_reserves_core_areas_crp_near_feat = st_nearest_feature(points_in_use, UNESCO_bio_reserves_core_areas_crp)

# get the distance to each grid points up to 3 decimal places
world_heritage_sites_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                    UNESCO_bio_reserves_core_areas_crp[UNESCO_bio_reserves_core_areas_crp_near_feat,], 
                                              by_element = TRUE)), digits = 3)
# Assign grid-score
world_heritage_sites_crp_shp$score = 0.0
world_heritage_sites_crp_shp[world_heritage_sites_crp_shp$dist==0, "score"] = p1_zone
world_heritage_sites_crp_shp[world_heritage_sites_crp_shp$dist<p1_buffer & world_heritage_sites_crp_shp$dist>0, "score"] = p1_buffer_score
world_heritage_sites_crp_shp[world_heritage_sites_crp_shp$dist>p1_buffer, "score"] = p1_out

############################## PRIORITY 1 #################################################

priority_1 = do.call(cbind, list(commonwealth_heritage_crp_shp$score,
                                 IUCN_protected_area_cats_I_III_crp_shp$score, 
                                 national_heritage_crp_shp$score,
                                 ramsar_sites_crp_shp$score, 
                                 aboriginal_heritage_places_crp_shp$score,
                                 UNESCO_bio_reserves_core_areas_crp_shp$score, 
                                 world_heritage_sites_crp_shp$score))

score = apply(priority_1, 1, min)

priority_1_score = st_as_sf(cbind(Id, data.frame(st_geometry(grid_in_use)), score))
unique(priority_1_score$score)

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/Priority scores - ESG P1/"))
path = paste0(save_path, "ESG/Priority scores - ESG P1/")

# Save file
st_write(priority_1_score, paste0(path, "Priority score 1 - ESG.shp") )

###########################################################################################

# PROCESSING PRIORITY 2

# eco_communities_crp, listed_migratory_species_crp
# listed_threatned_species_crp, section_19_crp, 
# 
p2_zone_a = 0.3

# CAPAD_51gReserve_crp, CAPAD_51hReserve_crp, CAPAD_BotanicGardens_crp
# CAPAD_ConservationPark_crp, CAPAD_ConservationReserve_crp
# CAPAD_ManagementArea_crp, CAPAD_NationalPark_crp
# CAPAD_NatureReserve_crp, CAPAD_StateReserve_crp
# IUCN_protected_cats_IV_crp, aboriginal_heritage_places_crp
# UNESCO_bio_reserves_buffer_crp, threatened_eco_community_crp
p2_zone_b = 0.4

# CAPAD_ConservationCovenant_crp, CAPAD_NRS_add_gazProgress_crp,
# CAPAD_Other_crp,CAPAD_pvtNatureReserve_crp, aboriginal_land_estate_crp
# threatened_priority_fauna_crp, threatened_priority_flora_crp
p2_zone_c = 0.5


p2_out = 1.0

p2_buffer = 0
# p2_buffer_Score = 

# CAPAD_51gReserve_crp ####################################################################

# Create a copy grid in use
CAPAD_51gReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_51gReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_51gReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_51gReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                     CAPAD_51gReserve_crp[CAPAD_51gReserve_crp_near_feat,], 
                                     by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_51gReserve_crp_shp$score = 0.0
CAPAD_51gReserve_crp_shp[CAPAD_51gReserve_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_51gReserve_crp_shp[CAPAD_51gReserve_crp_shp$dist<p2_buffer & CAPAD_51gReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_51gReserve_crp_shp[CAPAD_51gReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_51hReserve_crp ####################################################################

# Create a copy grid in use
CAPAD_51hReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_51hReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_51hReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_51hReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                             CAPAD_51hReserve_crp[CAPAD_51hReserve_crp_near_feat,], 
                                                             by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_51hReserve_crp_shp$score = 0.0
CAPAD_51hReserve_crp_shp[CAPAD_51hReserve_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_51hReserve_crp_shp[CAPAD_51hReserve_crp_shp$dist<p2_buffer & CAPAD_51hReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_51hReserve_crp_shp[CAPAD_51hReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_BotanicGardens_crp ################################################################

# Create a copy grid in use
CAPAD_BotanicGardens_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_BotanicGardens_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_BotanicGardens_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_BotanicGardens_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                             CAPAD_BotanicGardens_crp[CAPAD_BotanicGardens_crp_near_feat,], 
                                                             by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_BotanicGardens_crp_shp$score = 0.0
CAPAD_BotanicGardens_crp_shp[CAPAD_BotanicGardens_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_BotanicGardens_crp_shp[CAPAD_BotanicGardens_crp_shp$dist<p2_buffer & CAPAD_BotanicGardens_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_BotanicGardens_crp_shp[CAPAD_BotanicGardens_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_ConservationCovenant_crp ##########################################################

# Create a copy grid in use
CAPAD_ConservationCovenant_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_ConservationCovenant_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_ConservationCovenant_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_ConservationCovenant_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                 CAPAD_ConservationCovenant_crp[CAPAD_ConservationCovenant_crp_near_feat,], 
                                                                 by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_ConservationCovenant_crp_shp$score = 0.0
CAPAD_ConservationCovenant_crp_shp[CAPAD_ConservationCovenant_crp_shp$dist==0, "score"] = p2_zone_c
# CAPAD_ConservationCovenant_crp_shp[CAPAD_ConservationCovenant_crp_shp$dist<p2_buffer & CAPAD_ConservationCovenant_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_ConservationCovenant_crp_shp[CAPAD_ConservationCovenant_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_ConservationPark_crp #############################################################

# Create a copy grid in use
CAPAD_ConservationPark_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_ConservationPark_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_ConservationPark_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_ConservationPark_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       CAPAD_ConservationPark_crp[CAPAD_ConservationPark_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_ConservationPark_crp_shp$score = 0.0
CAPAD_ConservationPark_crp_shp[CAPAD_ConservationPark_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_ConservationPark_crp_shp[CAPAD_ConservationPark_crp_shp$dist<p2_buffer & CAPAD_ConservationPark_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_ConservationPark_crp_shp[CAPAD_ConservationPark_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_ConservationReserve_crp ##########################################################

# Create a copy grid in use
CAPAD_ConservationReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_ConservationReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_ConservationReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_ConservationReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                   CAPAD_ConservationReserve_crp[CAPAD_ConservationReserve_crp_near_feat,], 
                                                                   by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_ConservationReserve_crp_shp$score = 0.0
CAPAD_ConservationReserve_crp_shp[CAPAD_ConservationReserve_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_ConservationReserve_crp_shp[CAPAD_ConservationReserve_crp_shp$dist<p2_buffer & CAPAD_ConservationReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_ConservationReserve_crp_shp[CAPAD_ConservationReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_ManagementArea_crp ###############################################################

# Create a copy grid in use
CAPAD_ManagementArea_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_ManagementArea_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_ManagementArea_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_ManagementArea_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      CAPAD_ManagementArea_crp[CAPAD_ManagementArea_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_ManagementArea_crp_shp$score = 0.0
CAPAD_ManagementArea_crp_shp[CAPAD_ManagementArea_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_ManagementArea_crp_shp[CAPAD_ManagementArea_crp_shp$dist<p2_buffer & CAPAD_ManagementArea_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_ManagementArea_crp_shp[CAPAD_ManagementArea_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_NationalPark_crp ##################################################################

# Create a copy grid in use
CAPAD_NationalPark_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_NationalPark_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_NationalPark_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_NationalPark_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                 CAPAD_NationalPark_crp[CAPAD_NationalPark_crp_near_feat,], 
                                                                 by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_NationalPark_crp_shp$score = 0.0
CAPAD_NationalPark_crp_shp[CAPAD_NationalPark_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_NationalPark_crp_shp[CAPAD_NationalPark_crp_shp$dist<p2_buffer & CAPAD_NationalPark_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_NationalPark_crp_shp[CAPAD_NationalPark_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_NatureReserve_crp #################################################################

# Create a copy grid in use
CAPAD_NatureReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_NatureReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_NatureReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_NatureReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                               CAPAD_NatureReserve_crp[CAPAD_NatureReserve_crp_near_feat,], 
                                                               by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_NatureReserve_crp_shp$score = 0.0
CAPAD_NatureReserve_crp_shp[CAPAD_NatureReserve_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_NatureReserve_crp_shp[CAPAD_NatureReserve_crp_shp$dist<p2_buffer & CAPAD_NatureReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_NatureReserve_crp_shp[CAPAD_NatureReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_NRS_add_gazProgress_crp ##########################################################

# Create a copy grid in use
CAPAD_NRS_add_gazProgress_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_NRS_add_gazProgress_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_NRS_add_gazProgress_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_NRS_add_gazProgress_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                CAPAD_NRS_add_gazProgress_crp[CAPAD_NRS_add_gazProgress_crp_near_feat,], 
                                                                by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_NRS_add_gazProgress_crp_shp$score = 0.0
CAPAD_NRS_add_gazProgress_crp_shp[CAPAD_NRS_add_gazProgress_crp_shp$dist==0, "score"] = p2_zone_c
# CAPAD_NRS_add_gazProgress_crp_shp[CAPAD_NRS_add_gazProgress_crp_shp$dist<p2_buffer & CAPAD_NRS_add_gazProgress_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_NRS_add_gazProgress_crp_shp[CAPAD_NRS_add_gazProgress_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_Other_crp ########################################################################

# Create a copy grid in use
CAPAD_Other_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_Other_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_Other_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_Other_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      CAPAD_Other_crp[CAPAD_Other_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_Other_crp_shp$score = 0.0
CAPAD_Other_crp_shp[CAPAD_Other_crp_shp$dist==0, "score"] = p2_zone_c
# CAPAD_Other_crp_shp[CAPAD_Other_crp_shp$dist<p2_buffer & CAPAD_Other_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_Other_crp_shp[CAPAD_Other_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_pvtNatureReserve_crp ###############################################################

# Create a copy grid in use
CAPAD_pvtNatureReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_pvtNatureReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_pvtNatureReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_pvtNatureReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                        CAPAD_pvtNatureReserve_crp[CAPAD_pvtNatureReserve_crp_near_feat,], 
                                                        by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_pvtNatureReserve_crp_shp$score = 0.0
CAPAD_pvtNatureReserve_crp_shp[CAPAD_pvtNatureReserve_crp_shp$dist==0, "score"] = p2_zone_c
# CAPAD_pvtNatureReserve_crp_shp[CAPAD_pvtNatureReserve_crp_shp$dist<p2_buffer & CAPAD_pvtNatureReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_pvtNatureReserve_crp_shp[CAPAD_pvtNatureReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# CAPAD_StateReserve_crp ####################################################################

# Create a copy grid in use
CAPAD_StateReserve_crp_shp = grid_in_use

# Get the nearest feature points 
CAPAD_StateReserve_crp_near_feat = st_nearest_feature(points_in_use, CAPAD_StateReserve_crp)

# get the distance to each grid points up to 3 decimal places
CAPAD_StateReserve_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                   CAPAD_StateReserve_crp[CAPAD_StateReserve_crp_near_feat,], 
                                                                   by_element = TRUE)), digits = 3)
# Assign grid-score
CAPAD_StateReserve_crp_shp$score = 0.0
CAPAD_StateReserve_crp_shp[CAPAD_StateReserve_crp_shp$dist==0, "score"] = p2_zone_b
# CAPAD_StateReserve_crp_shp[CAPAD_StateReserve_crp_shp$dist<p2_buffer & CAPAD_StateReserve_crp_shp$dist>0, "score"] = p2_buffer_score
CAPAD_StateReserve_crp_shp[CAPAD_StateReserve_crp_shp$dist>p2_buffer, "score"] = p2_out

# eco_communities_crp ######################################################################

# Create a copy grid in use
eco_communities_crp_shp = grid_in_use

# Get the nearest feature points 
eco_communities_crp_near_feat = st_nearest_feature(points_in_use, eco_communities_crp)

# get the distance to each grid points up to 3 decimal places
eco_communities_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                               eco_communities_crp[eco_communities_crp_near_feat,], 
                                                               by_element = TRUE)), digits = 3)
# Assign grid-score
eco_communities_crp_shp$score = 0.0
eco_communities_crp_shp[eco_communities_crp_shp$dist==0, "score"] = p2_zone_a
# eco_communities_crp_shp[eco_communities_crp_shp$dist<p2_buffer & eco_communities_crp_shp$dist>0, "score"] = p2_buffer_score
eco_communities_crp_shp[eco_communities_crp_shp$dist>p2_buffer, "score"] = p2_out

# IUCN_protected_cats_IV_crp ######################################################################

# Create a copy grid in use
IUCN_protected_cats_IV_crp_shp = grid_in_use

# Get the nearest feature points 
IUCN_protected_cats_IV_crp_near_feat = st_nearest_feature(points_in_use, IUCN_protected_cats_IV_crp)

# get the distance to each grid points up to 3 decimal places
IUCN_protected_cats_IV_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                            IUCN_protected_cats_IV_crp[IUCN_protected_cats_IV_crp_near_feat,], 
                                                            by_element = TRUE)), digits = 3)
# Assign grid-score
IUCN_protected_cats_IV_crp_shp$score = 0.0
IUCN_protected_cats_IV_crp_shp[IUCN_protected_cats_IV_crp_shp$dist==0, "score"] = p2_zone_b
# IUCN_protected_cats_IV_crp_shp[IUCN_protected_cats_IV_crp_shp$dist<p2_buffer & IUCN_protected_cats_IV_crp_shp$dist>0, "score"] = p2_buffer_score
IUCN_protected_cats_IV_crp_shp[IUCN_protected_cats_IV_crp_shp$dist>p2_buffer, "score"] = p2_out

# section_19_crp #############################################################################

# Create a copy grid in use
section_19_crp_shp = grid_in_use

# Get the nearest feature points 
section_19_crp_near_feat = st_nearest_feature(points_in_use, section_19_crp)

# get the distance to each grid points up to 3 decimal places
section_19_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, section_19_crp[section_19_crp_near_feat,], 
                                                                    by_element = TRUE)), digits = 3)
# Assign grid-score
section_19_crp_shp$score = 0.0
section_19_crp_shp[section_19_crp_shp$dist==0, "score"] = p2_zone_a
# section_19_crp_shp[section_19_crp_shp$dist<p2_buffer & section_19_crp_shp$dist>0, "score"] = p2_buffer_score
section_19_crp_shp[section_19_crp_shp$dist>p2_buffer, "score"] = p2_out

# aboriginal_heritage_places_crp ##################################################################

# Create a copy grid in use
aboriginal_heritage_places_crp_shp = grid_in_use

# Get the nearest feature points 
aboriginal_heritage_places_crp_near_feat = st_nearest_feature(points_in_use, aboriginal_heritage_places_crp)

# get the distance to each grid points up to 3 decimal places
aboriginal_heritage_places_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, aboriginal_heritage_places_crp[aboriginal_heritage_places_crp_near_feat,], 
                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
aboriginal_heritage_places_crp_shp$score = 0.0
aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist==0, "score"] = p2_zone_b
# aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist<p2_buffer & aboriginal_heritage_places_crp_shp$dist>0, "score"] = p2_buffer_score
aboriginal_heritage_places_crp_shp[aboriginal_heritage_places_crp_shp$dist>p2_buffer, "score"] = p2_out

# aboriginal_land_estate_crp #######################################################################

# Create a copy grid in use
aboriginal_land_estate_crp_shp = grid_in_use

# Get the nearest feature points 
aboriginal_land_estate_crp_near_feat = st_nearest_feature(points_in_use, aboriginal_land_estate_crp)

# get the distance to each grid points up to 3 decimal places
aboriginal_land_estate_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, aboriginal_land_estate_crp[aboriginal_land_estate_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
aboriginal_land_estate_crp_shp$score = 0.0
aboriginal_land_estate_crp_shp[aboriginal_land_estate_crp_shp$dist==0, "score"] = p2_zone_c
# aboriginal_land_estate_crp_shp[aboriginal_land_estate_crp_shp$dist<p2_buffer & aboriginal_land_estate_crp_shp$dist>0, "score"] = p2_buffer_score
aboriginal_land_estate_crp_shp[aboriginal_land_estate_crp_shp$dist>p2_buffer, "score"] = p2_out

# threatened_priority_fauna_crp #####################################################################

# Create a copy grid in use
threatened_priority_fauna_crp_shp = grid_in_use

# Get the nearest feature points 
threatened_priority_fauna_crp_near_feat = st_nearest_feature(points_in_use, threatened_priority_fauna_crp)

# get the distance to each grid points up to 3 decimal places
threatened_priority_fauna_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, threatened_priority_fauna_crp[threatened_priority_fauna_crp_near_feat,], 
                                                                   by_element = TRUE)), digits = 3)
# Assign grid-score
threatened_priority_fauna_crp_shp$score = 0.0
threatened_priority_fauna_crp_shp[threatened_priority_fauna_crp_shp$dist==0, "score"] = p2_zone_c
# threatened_priority_fauna_crp_shp[threatened_priority_fauna_crp_shp$dist<p2_buffer & threatened_priority_fauna_crp_shp$dist>0, "score"] = p2_buffer_score
threatened_priority_fauna_crp_shp[threatened_priority_fauna_crp_shp$dist>p2_buffer, "score"] = p2_out

# threatened_priority_flora_crp #####################################################################

# Create a copy grid in use
threatened_priority_flora_crp_shp = grid_in_use

# Get the nearest feature points 
threatened_priority_flora_crp_near_feat = st_nearest_feature(points_in_use, threatened_priority_flora_crp)

# get the distance to each grid points up to 3 decimal places
threatened_priority_flora_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, threatened_priority_flora_crp[threatened_priority_flora_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
threatened_priority_flora_crp_shp$score = 0.0
threatened_priority_flora_crp_shp[threatened_priority_flora_crp_shp$dist==0, "score"] = p2_zone_c
# threatened_priority_flora_crp_shp[threatened_priority_flora_crp_shp$dist<p2_buffer & threatened_priority_flora_crp_shp$dist>0, "score"] = p2_buffer_score
threatened_priority_flora_crp_shp[threatened_priority_flora_crp_shp$dist>p2_buffer, "score"] = p2_out

# threatened_eco_community_crp #####################################################################

# Create a copy grid in use
threatened_eco_community_crp_shp = grid_in_use

# Get the nearest feature points 
threatened_eco_community_crp_near_feat = st_nearest_feature(points_in_use, threatened_eco_community_crp)

# get the distance to each grid points up to 3 decimal places
threatened_eco_community_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, threatened_eco_community_crp[threatened_eco_community_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
threatened_eco_community_crp_shp$score = 0.0
threatened_eco_community_crp_shp[threatened_eco_community_crp_shp$dist==0, "score"] = p2_zone_b
# threatened_eco_community_crp_shp[threatened_eco_community_crp_shp$dist<p2_buffer & threatened_eco_community_crp_shp$dist>0, "score"] = p2_buffer_score
threatened_eco_community_crp_shp[threatened_eco_community_crp_shp$dist>p2_buffer, "score"] = p2_out

# UNESCO_bio_reserves_buffer_crp #####################################################################

# Create a copy grid in use
UNESCO_bio_reserves_buffer_crp_shp = grid_in_use

# Get the nearest feature points 
UNESCO_bio_reserves_buffer_crp_near_feat = st_nearest_feature(points_in_use, UNESCO_bio_reserves_buffer_crp)

# get the distance to each grid points up to 3 decimal places
UNESCO_bio_reserves_buffer_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, UNESCO_bio_reserves_buffer_crp[UNESCO_bio_reserves_buffer_crp_near_feat,], 
                                                                     by_element = TRUE)), digits = 3)
# Assign grid-score
UNESCO_bio_reserves_buffer_crp_shp$score = 0.0
UNESCO_bio_reserves_buffer_crp_shp[UNESCO_bio_reserves_buffer_crp_shp$dist==0, "score"] = p2_zone_b
# UNESCO_bio_reserves_buffer_crp_shp[UNESCO_bio_reserves_buffer_crp_shp$dist<p2_buffer & UNESCO_bio_reserves_buffer_crp_shp$dist>0, "score"] = p2_buffer_score
UNESCO_bio_reserves_buffer_crp_shp[UNESCO_bio_reserves_buffer_crp_shp$dist>p2_buffer, "score"] = p2_out

############################## PRIORITY 2 #################################################

priority_2 = do.call(cbind, list( CAPAD_51gReserve_crp_shp$score, 
                                  CAPAD_51hReserve_crp_shp$score, 
                                  CAPAD_BotanicGardens_crp_shp$score,
                                  CAPAD_ConservationCovenant_crp_shp$score,
                                  CAPAD_ConservationPark_crp_shp$score,
                                  CAPAD_ConservationReserve_crp_shp$score,
                                  CAPAD_ManagementArea_crp_shp$score,
                                  CAPAD_NationalPark_crp_shp$score,
                                  CAPAD_NatureReserve_crp_shp$score,
                                  CAPAD_NRS_add_gazProgress_crp_shp$score,
                                  CAPAD_Other_crp_shp$score,
                                  CAPAD_pvtNatureReserve_crp_shp$score,
                                  CAPAD_StateReserve_crp_shp$score,
                                  eco_communities_crp_shp$score,
                                  IUCN_protected_cats_IV_crp_shp$score,
                                  section_19_crp_shp$score,
                                  aboriginal_heritage_places_crp_shp$score,
                                  aboriginal_land_estate_crp_shp$score,
                                  threatened_priority_fauna_crp_shp$score,
                                  threatened_priority_flora_crp_shp$score,
                                  threatened_eco_community_crp_shp$score,
                                  UNESCO_bio_reserves_buffer_crp_shp$score))
score = apply(priority_2, 1, min) 
priority_2_score = st_as_sf(cbind(Id, data.frame(st_geometry(grid_in_use)), score))
unique(priority_2_score$score)

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/Priority scores - ESG P2/"))
path = paste0(save_path, "ESG/Priority scores - ESG P2/")

# Save file
st_write(priority_2_score, paste0(path, "Priority score 2 - ESG.shp") )

###########################################################################################

# PROCESSING PRIORITY 3 

# Controls

# crown_freehold_dept_crp, state_forests_crp
# timber_reserves_crp
p3_zone_a = 0.6

# aboriginal_community_town_crp, wa_dmirs_sec57_4_crp
# wa_hydrography_linear_crp, wa_overview_towns_crp
p3_zone_b = 0.7

# native_title_crp, native_title_fa_notices_crp
# native_title_fa_objections_crp, native_title_fada_crp
# native_title_ILUA_register_crp, native_title_ntd_register_crp
# native_title_ntda_register_crp, native_title_ntda_schedule_crp
# native_title_ntda_transac_crp, native_title_ratsib_crp
# native_title_s31_agreemnt_crp, wa_pastoral_stations_crp
p3_zone_c = 0.8

p3_buffer_score = 0
p2_buffer = 0

p3_out = 1

# crown_freehold_dept_crp #####################################################

# Create a copy grid in use
crown_freehold_dept_crp_shp = grid_in_use

# Get the nearest feature points 
crown_freehold_dept_crp_near_feat = st_nearest_feature(points_in_use, crown_freehold_dept_crp)

# get the distance to each grid points up to 3 decimal places
crown_freehold_dept_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                       crown_freehold_dept_crp[crown_freehold_dept_crp_near_feat,], 
                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
crown_freehold_dept_crp_shp$score = 0.0
crown_freehold_dept_crp_shp[crown_freehold_dept_crp_shp$dist==0, "score"] = p3_zone_a
# crown_freehold_dept_crp_shp[crown_freehold_dept_crp_shp$dist<p2_buffer & crown_freehold_dept_crp_shp$dist>0, "score"] = p3_buffer_score
crown_freehold_dept_crp_shp[crown_freehold_dept_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_crp ########################################################

# Create a copy grid in use
native_title_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_crp_near_feat = st_nearest_feature(points_in_use, native_title_crp)

# get the distance to each grid points up to 3 decimal places
native_title_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                native_title_crp[native_title_crp_near_feat,], 
                                                                by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_crp_shp$score = 0.0
native_title_crp_shp[native_title_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_crp_shp[native_title_crp_shp$dist<p2_buffer & native_title_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_crp_shp[native_title_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_fa_notices_crp ########################################################

# Create a copy grid in use
native_title_fa_notices_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_fa_notices_crp_near_feat = st_nearest_feature(points_in_use, native_title_fa_notices_crp)

# get the distance to each grid points up to 3 decimal places
native_title_fa_notices_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                         native_title_fa_notices_crp[native_title_fa_notices_crp_near_feat,], 
                                                         by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_fa_notices_crp_shp$score = 0.0
native_title_fa_notices_crp_shp[native_title_fa_notices_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_fa_notices_crp_shp[native_title_fa_notices_crp_shp$dist<p2_buffer & native_title_fa_notices_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_fa_notices_crp_shp[native_title_fa_notices_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_fa_objections_crp ####################################################

# Create a copy grid in use
native_title_fa_objections_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_fa_objections_crp_near_feat = st_nearest_feature(points_in_use, native_title_fa_objections_crp)

# get the distance to each grid points up to 3 decimal places
native_title_fa_objections_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                    native_title_fa_objections_crp[native_title_fa_objections_crp_near_feat,], 
                                                                    by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_fa_objections_crp_shp$score = 0.0
native_title_fa_objections_crp_shp[native_title_fa_objections_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_fa_objections_crp_shp[native_title_fa_objections_crp_shp$dist<p2_buffer & native_title_fa_objections_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_fa_objections_crp_shp[native_title_fa_objections_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_fada_crp ####################################################

# Create a copy grid in use
native_title_fada_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_fada_crp_near_feat = st_nearest_feature(points_in_use, native_title_fada_crp)

# get the distance to each grid points up to 3 decimal places
native_title_fada_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       native_title_fada_crp[native_title_fada_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_fada_crp_shp$score = 0.0
native_title_fada_crp_shp[native_title_fada_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_fada_crp_shp[native_title_fada_crp_shp$dist<p2_buffer & native_title_fada_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_fada_crp_shp[native_title_fada_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ILUA_register_crp ####################################################

# Create a copy grid in use
native_title_ILUA_register_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ILUA_register_crp_near_feat = st_nearest_feature(points_in_use, native_title_ILUA_register_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ILUA_register_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                              native_title_ILUA_register_crp[native_title_ILUA_register_crp_near_feat,], 
                                                              by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ILUA_register_crp_shp$score = 0.0
native_title_ILUA_register_crp_shp[native_title_ILUA_register_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ILUA_register_crp_shp[native_title_ILUA_register_crp_shp$dist<p2_buffer & native_title_ILUA_register_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ILUA_register_crp_shp[native_title_ILUA_register_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ntd_register_crp ####################################################

# Create a copy grid in use
native_title_ntd_register_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ntd_register_crp_near_feat = st_nearest_feature(points_in_use, native_title_ntd_register_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ntd_register_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       native_title_ntd_register_crp[native_title_ntd_register_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ntd_register_crp_shp$score = 0.0
native_title_ntd_register_crp_shp[native_title_ntd_register_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ntd_register_crp_shp[native_title_ntd_register_crp_shp$dist<p2_buffer & native_title_ntd_register_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ntd_register_crp_shp[native_title_ntd_register_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ntda_register_crp ####################################################

# Create a copy grid in use
native_title_ntda_register_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ntda_register_crp_near_feat = st_nearest_feature(points_in_use, native_title_ntda_register_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ntda_register_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      native_title_ntda_register_crp[native_title_ntda_register_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ntda_register_crp_shp$score = 0.0
native_title_ntda_register_crp_shp[native_title_ntda_register_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ntda_register_crp_shp[native_title_ntda_register_crp_shp$dist<p2_buffer & native_title_ntda_register_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ntda_register_crp_shp[native_title_ntda_register_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ntda_schedule_crp ####################################################

# Create a copy grid in use
native_title_ntda_schedule_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ntda_schedule_crp_near_feat = st_nearest_feature(points_in_use, native_title_ntda_schedule_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ntda_schedule_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       native_title_ntda_schedule_crp[native_title_ntda_schedule_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ntda_schedule_crp_shp$score = 0.0
native_title_ntda_schedule_crp_shp[native_title_ntda_schedule_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ntda_schedule_crp_shp[native_title_ntda_schedule_crp_shp$dist<p2_buffer & native_title_ntda_schedule_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ntda_schedule_crp_shp[native_title_ntda_schedule_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ntda_transac_crp ####################################################

# Create a copy grid in use
native_title_ntda_transac_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ntda_transac_crp_near_feat = st_nearest_feature(points_in_use, native_title_ntda_transac_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ntda_transac_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                       native_title_ntda_transac_crp[native_title_ntda_transac_crp_near_feat,], 
                                                                       by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ntda_transac_crp_shp$score = 0.0
native_title_ntda_transac_crp_shp[native_title_ntda_transac_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ntda_transac_crp_shp[native_title_ntda_transac_crp_shp$dist<p2_buffer & native_title_ntda_transac_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ntda_transac_crp_shp[native_title_ntda_transac_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_ratsib_crp ####################################################

# Create a copy grid in use
native_title_ratsib_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_ratsib_crp_near_feat = st_nearest_feature(points_in_use, native_title_ratsib_crp)

# get the distance to each grid points up to 3 decimal places
native_title_ratsib_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      native_title_ratsib_crp[native_title_ratsib_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_ratsib_crp_shp$score = 0.0
native_title_ratsib_crp_shp[native_title_ratsib_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_ratsib_crp_shp[native_title_ratsib_crp_shp$dist<p2_buffer & native_title_ratsib_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_ratsib_crp_shp[native_title_ratsib_crp_shp$dist>p2_buffer, "score"] = p3_out

# native_title_s31_agreemnt_crp ####################################################

# Create a copy grid in use
native_title_s31_agreemnt_crp_shp = grid_in_use

# Get the nearest feature points 
native_title_s31_agreemnt_crp_near_feat = st_nearest_feature(points_in_use, native_title_s31_agreemnt_crp)

# get the distance to each grid points up to 3 decimal places
native_title_s31_agreemnt_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                native_title_s31_agreemnt_crp[native_title_s31_agreemnt_crp_near_feat,], 
                                                                by_element = TRUE)), digits = 3)
# Assign grid-score
native_title_s31_agreemnt_crp_shp$score = 0.0
native_title_s31_agreemnt_crp_shp[native_title_s31_agreemnt_crp_shp$dist==0, "score"] = p3_zone_c
# native_title_s31_agreemnt_crp_shp[native_title_s31_agreemnt_crp_shp$dist<p2_buffer & native_title_s31_agreemnt_crp_shp$dist>0, "score"] = p3_buffer_score
native_title_s31_agreemnt_crp_shp[native_title_s31_agreemnt_crp_shp$dist>p2_buffer, "score"] = p3_out

# rntbc_native_title_crp ####################################################

# Create a copy grid in use
rntbc_native_title_crp_shp = grid_in_use

# Get the nearest feature points 
rntbc_native_title_crp_near_feat = st_nearest_feature(points_in_use, rntbc_native_title_crp)

# get the distance to each grid points up to 3 decimal places
rntbc_native_title_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      rntbc_native_title_crp[rntbc_native_title_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
rntbc_native_title_crp_shp$score = 0.0
rntbc_native_title_crp_shp[rntbc_native_title_crp_shp$dist==0, "score"] = p3_zone_c
# rntbc_native_title_crp_shp[rntbc_native_title_crp_shp$dist<p2_buffer & rntbc_native_title_crp_shp$dist>0, "score"] = p3_buffer_score
rntbc_native_title_crp_shp[rntbc_native_title_crp_shp$dist>p2_buffer, "score"] = p3_out

# state_forests_crp ####################################################

# Create a copy grid in use
state_forests_crp_shp = grid_in_use

# Get the nearest feature points 
state_forests_crp_near_feat = st_nearest_feature(points_in_use, state_forests_crp)

# get the distance to each grid points up to 3 decimal places
state_forests_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                               state_forests_crp[state_forests_crp_near_feat,], 
                                                               by_element = TRUE)), digits = 3)
# Assign grid-score
state_forests_crp_shp$score = 0.0
state_forests_crp_shp[state_forests_crp_shp$dist==0, "score"] = p3_zone_a
# state_forests_crp_shp[state_forests_crp_shp$dist<p2_buffer & state_forests_crp_shp$dist>0, "score"] = p3_buffer_score
state_forests_crp_shp[state_forests_crp_shp$dist>p2_buffer, "score"] = p3_out

# timber_reserves_crp ####################################################

# Create a copy grid in use
timber_reserves_crp_shp = grid_in_use

# Get the nearest feature points 
timber_reserves_crp_near_feat = st_nearest_feature(points_in_use, timber_reserves_crp)

# get the distance to each grid points up to 3 decimal places
timber_reserves_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                          timber_reserves_crp[timber_reserves_crp_near_feat,], 
                                                          by_element = TRUE)), digits = 3)
# Assign grid-score
timber_reserves_crp_shp$score = 0.0
timber_reserves_crp_shp[timber_reserves_crp_shp$dist==0, "score"] = p3_zone_a
# timber_reserves_crp_shp[timber_reserves_crp_shp$dist<p2_buffer & timber_reserves_crp_shp$dist>0, "score"] = p3_buffer_score
timber_reserves_crp_shp[timber_reserves_crp_shp$dist>p2_buffer, "score"] = p3_out

# aboriginal_community_town_crp ####################################################

# Create a copy grid in use
aboriginal_community_town_crp_shp = grid_in_use

# Get the nearest feature points 
aboriginal_community_town_crp_near_feat = st_nearest_feature(points_in_use, aboriginal_community_town_crp)

# get the distance to each grid points up to 3 decimal places
aboriginal_community_town_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                            aboriginal_community_town_crp[aboriginal_community_town_crp_near_feat,], 
                                                            by_element = TRUE)), digits = 3)
# Assign grid-score
aboriginal_community_town_crp_shp$score = 0.0
aboriginal_community_town_crp_shp[aboriginal_community_town_crp_shp$dist==0, "score"] = p3_zone_b
# aboriginal_community_town_crp_shp[aboriginal_community_town_crp_shp$dist<p2_buffer & aboriginal_community_town_crp_shp$dist>0, "score"] = p3_buffer_score
aboriginal_community_town_crp_shp[aboriginal_community_town_crp_shp$dist>p2_buffer, "score"] = p3_out

# wa_dmirs_sec57_4_crp ####################################################

# Create a copy grid in use
wa_dmirs_sec57_4_crp_shp = grid_in_use

# Get the nearest feature points 
wa_dmirs_sec57_4_crp_near_feat = st_nearest_feature(points_in_use, wa_dmirs_sec57_4_crp)

# get the distance to each grid points up to 3 decimal places
wa_dmirs_sec57_4_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                      wa_dmirs_sec57_4_crp[wa_dmirs_sec57_4_crp_near_feat,], 
                                                                      by_element = TRUE)), digits = 3)
# Assign grid-score
wa_dmirs_sec57_4_crp_shp$score = 0.0
wa_dmirs_sec57_4_crp_shp[wa_dmirs_sec57_4_crp_shp$dist==0, "score"] = p3_zone_b
# wa_dmirs_sec57_4_crp_shp[wa_dmirs_sec57_4_crp_shp$dist<p2_buffer & wa_dmirs_sec57_4_crp_shp$dist>0, "score"] = p3_buffer_score
wa_dmirs_sec57_4_crp_shp[wa_dmirs_sec57_4_crp_shp$dist>p2_buffer, "score"] = p3_out

# wa_hydrography_linear_crp ####################################################

# Create a copy grid in use
wa_hydrography_linear_crp_shp = grid_in_use

# Get the nearest feature points 
wa_hydrography_linear_crp_near_feat = st_nearest_feature(points_in_use, wa_hydrography_linear_crp)

# get the distance to each grid points up to 3 decimal places
wa_hydrography_linear_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                             wa_hydrography_linear_crp[wa_hydrography_linear_crp_near_feat,], 
                                                             by_element = TRUE)), digits = 3)
# Assign grid-score
wa_hydrography_linear_crp_shp$score = 0.0
wa_hydrography_linear_crp_shp[wa_hydrography_linear_crp_shp$dist==0, "score"] = p3_zone_b
# wa_hydrography_linear_crp_shp[wa_hydrography_linear_crp_shp$dist<p2_buffer & wa_hydrography_linear_crp_shp$dist>0, "score"] = p3_buffer_score
wa_hydrography_linear_crp_shp[wa_hydrography_linear_crp_shp$dist>p2_buffer, "score"] = p3_out

# wa_overview_towns_crp ####################################################

# Create a copy grid in use
wa_overview_towns_crp_shp = grid_in_use

# Get the nearest feature points 
wa_overview_towns_crp_near_feat = st_nearest_feature(points_in_use, wa_overview_towns_crp)

# get the distance to each grid points up to 3 decimal places
wa_overview_towns_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                                  wa_overview_towns_crp[wa_overview_towns_crp_near_feat,], 
                                                                  by_element = TRUE)), digits = 3)
# Assign grid-score
wa_overview_towns_crp_shp$score = 0.0
wa_overview_towns_crp_shp[wa_overview_towns_crp_shp$dist==0, "score"] = p3_zone_b
# wa_overview_towns_crp_shp[wa_overview_towns_crp_shp$dist<p2_buffer & wa_overview_towns_crp_shp$dist>0, "score"] = p3_buffer_score
wa_overview_towns_crp_shp[wa_overview_towns_crp_shp$dist>p2_buffer, "score"] = p3_out

# wa_pastoral_stations_crp ####################################################

# Create a copy grid in use
wa_pastoral_stations_crp_shp = grid_in_use

# Get the nearest feature points 
wa_pastoral_stations_crp_near_feat = st_nearest_feature(points_in_use, wa_pastoral_stations_crp)

# get the distance to each grid points up to 3 decimal places
wa_pastoral_stations_crp_shp$dist = round(as.numeric(st_distance(grid_in_use, 
                                                              wa_pastoral_stations_crp[wa_pastoral_stations_crp_near_feat,], 
                                                              by_element = TRUE)), digits = 3)
# Assign grid-score
wa_pastoral_stations_crp_shp$score = 0.0
wa_pastoral_stations_crp_shp[wa_pastoral_stations_crp_shp$dist==0, "score"] = p3_zone_c
# wa_pastoral_stations_crp_shp[wa_pastoral_stations_crp_shp$dist<p2_buffer & wa_pastoral_stations_crp_shp$dist>0, "score"] = p3_buffer_score
wa_pastoral_stations_crp_shp[wa_pastoral_stations_crp_shp$dist>p2_buffer, "score"] = p3_out


############################## PRIORITY 3 #################################################

priority_3 = do.call(cbind, list(native_title_crp_shp$score,               
                                 native_title_fa_notices_crp_shp$score,      
                                 native_title_ntd_register_crp_shp$score,    
                                 native_title_ntda_transac_crp_shp$score,   
                                 crown_freehold_dept_crp_shp$score,    
                                 native_title_fa_objections_crp_shp$score,  
                                 native_title_fada_crp_shp$score,    
                                 native_title_ILUA_register_crp_shp$score,    
                                 native_title_ntda_register_crp_shp$score,
                                 native_title_ntda_schedule_crp_shp$score,
                                 native_title_s31_agreemnt_crp_shp$score,     
                                 rntbc_native_title_crp_shp$score,    
                                 state_forests_crp_shp$score,      
                                 timber_reserves_crp_shp$score,   
                                 aboriginal_community_town_crp_shp$score,  
                                 wa_dmirs_sec57_4_crp_shp$score,  
                                 wa_hydrography_linear_crp_shp$score,   
                                 wa_overview_towns_crp_shp$score,   
                                 wa_pastoral_stations_crp_shp$score))

score = apply(priority_3, 1, min) 

priority_3_score = st_as_sf(cbind(Id, data.frame(st_geometry(grid_in_use)), score))
unique(priority_3_score$score)

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/Priority scores - ESG P3/"))
path = paste0(save_path, "ESG/Priority scores - ESG P3/")

# Save file
st_write(priority_3_score, paste0(path, "Priority score 3 - ESG.shp") )

###########################################################################################

# Data Import for grid 1km
save_path = "E:/ABLE/Modifiers/Outputs/"
path = paste0(save_path, "ESG/")
priority_1_score = st_read(paste0(path, "Priority scores - ESG P1/Priority score 1 - ESG.shp"))
priority_2_score = st_read(paste0(path, "Priority scores - ESG P2/Priority score 2 - ESG.shp"))


###########################################################################################

############################## PRIORITY MAP #################################################

priority_score = do.call(cbind, list(priority_1_score$score,   
                                     priority_2_score$score
                                     # priority_3_score$score
                                     ))

score = apply(priority_score, 1, min) 

priority_score = st_as_sf(cbind(Id, data.frame(st_geometry(grid_in_use)), score))
unique(priority_score$score)

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/Priority scores - ESG/"))
path = paste0(save_path, "ESG/Priority scores - ESG/")

# Save file
st_write(priority_score, paste0(path, "Priority score - ESG.shp") )

###########################################################################################

# # PRIORITY 1

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/"))
dir.create(paste0(save_path, "ESG/figures/"))
dir.create(paste0(save_path, "ESG/figures/Priority 1/"))

writeTo1 = paste0(save_path, "ESG/figures/Priority 1/")

priority_1_score_map = ggplot() + geom_sf(data= priority_1_score, aes(fill= .data[[names(priority_1_score)[2]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw()+ 
  ggtitle("Priority 1 map for ESG")

# Save
name = "Priority_1_map.jpeg"
ggsave(paste0(writeTo1, name), priority_1_score_map, width = 10, height = 10)


# ###########################################################################################
# 
# # PRIORITY 2

save_path = "E:/ABLE/Modifiers/Outputs/"

dir.create(paste0(save_path, "ESG/figures/Priority 2/"))

writeTo1 = paste0(save_path, "ESG/figures/Priority 2/")

priority_2_score_map = ggplot() + geom_sf(data= priority_2_score, aes(fill= .data[[names(priority_2_score)[2]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw()+ 
  ggtitle("Priority 2 map for ESG")

# Save
name = "Priority_2_map.jpeg"
ggsave(paste0(writeTo1, name), priority_2_score_map, width = 10, height = 10)
# 
# ###########################################################################################
# 
# # PRIORITY 3
# 
# save_path = "E:/ABLE/Modifiers/Outputs/"
# 
# dir.create(paste0(save_path, "ESG/figures/Priority 3/"))
# 
# writeTo1 = paste0(save_path, "ESG/figures/Priority 3/")
# 
# priority_3_score_map = ggplot() + geom_sf(data= priority_3_score, aes(fill= .data[[names(priority_3_score)[2]]]), color= NA) +
#   scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw()
# 
# # Save
# name = "Priority_3_map.jpeg"
# ggsave(paste0(writeTo1, name), priority_3_score_map, width = 10, height = 10)
# 
# ###########################################################################################
# 
# # PRIORITY MAP
# 
save_path = "E:/ABLE/Modifiers/Outputs/"

writeTo1 = paste0(save_path, "ESG/figures/")

priority_map = ggplot() + geom_sf(data= priority_score, aes(fill= .data[[names(priority_score)[2]]]), color= NA) +
  scale_fill_gradientn(colours = c("#23CCFF", "yellow","#FF0000"), limits= c(0,1))+ theme_bw()+ 
  ggtitle("Priority map for ESG")

# Save
name = "Priority_map.jpeg"
ggsave(paste0(writeTo1, name), priority_map, width = 10, height = 10)


###########################################################################################

# dir.create(paste0(save_path, "ESG/Priority scores - ESG/"))
# path = paste0(save_path, "ESG/Priority scores - ESG/")
# 
# # Save file 
# st_write(priority_score, paste0(path, 
#                                 "Priority score - ESG.kml") )
# 
# 
# st_write(priority_1_score, paste0(path, 
#                                 "Priority score 1 - ESG.shp") )
# 
# st_write(priority_2_score, paste0(path, 
#                                   "Priority score 2 - ESG.shp") )
# 
# st_write(priority_3_score, paste0(path, 
#                                   "Priority score 3 - ESG.shp") )
# 
# x = st_rasterize(priority_score[,2], dx=1000, dy=1000)
# write_stars(x, paste0(path, "Priority score - ESG.kmz") )

