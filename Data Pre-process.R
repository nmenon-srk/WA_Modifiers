
# ABLE WA Modifiers Analysis 
# ESG and INFRASTRUCTURES 

###########################################################################################

# Basic Imports
library(sf)
library(tidyverse)
library(stringi)
library(ggplot2)
library(FuzzyLogic)


# Load the grid data
grid_path = "E:/ABLE/Modifiers/Data/Modifier Database/Grid_WA/"
grid_5km = st_read(paste0(grid_path, "Base Grid/WA_Grid_5km.shp"))

grid_in_use = grid_5km
CRS = st_crs(grid_in_use)

############################################################################################

# INFRASTRUCTURE PREPROCESSING

dir.create("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/")
dir.create("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/")

# Priority 1,2,3
i=1

dir.create(paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/Priority ", i, "/"))
writeTo = paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/Infrastructure/Priority ", i, "/")

geo_path = paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database/Infrastructure/Priority ", i, "/")
folder_names = list.dirs(path = geo_path)

for (j in 2:length(folder_names)){
  layer = st_read(folder_names[j])
  layer = st_transform(layer, crs=CRS)
  
  parts = unlist(strsplit(folder_names[j], "/"))
  dir.create(paste0(writeTo, parts[9], "/"))
  st_write(layer, paste0(writeTo, parts[9], "/", parts[9], ".shp"))
}

############################################################################################

# ESG PREPROCESSING

dir.create("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/")

# Priority 1,2,3
i=1

dir.create(paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/Priority ", i, "/"))
writeTo = paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database_transformed/ESG/Priority ", i, "/")

geo_path = paste0("E:/ABLE/Modifiers/Data/Modifier Database/WA_Modifier_database/ESG/Priority ", i, "/")
folder_names = list.dirs(path = geo_path)

for (j in 2:length(folder_names)){
  layer = st_read(folder_names[j])
  layer = st_transform(layer, crs=CRS)
  
  parts = unlist(strsplit(folder_names[j], "/"))
  dir.create(paste0(writeTo, parts[9], "/"))
  st_write(layer, paste0(writeTo, parts[9], "/", parts[9], ".shp"))
}


############################################################################################

