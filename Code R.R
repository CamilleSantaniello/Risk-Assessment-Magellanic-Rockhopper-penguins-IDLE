#-------------------------------------------------------------------------------------------------------------------
##For any questions regarding the following script, please contact corresponding author Camille Santaniello
# camille.santaniello@gmail.com
#---------------------------------------------------------------------------------------------------------------------


## Risk Assessment for Magellanic and Southern Rockhopper penguins from Isla de los Estados


# setwd("C:/Users/..../Data")
#--------------------------------------------------------------------------------------------------------------
#1) Format the data and remove at-sea coordinates here for Magellanic 2014, individual "232G_puesta1" 
library(dplyr)    
library(tidyverse)

newfolder<- "./Results/Spheniscus_magellanicus_2014"

if (!dir.exists(newfolder)) {
 dir.create(newfolder)
  print(paste("Folder", newfolder, "created"))
} else {
  print(paste("Folder", newfolder, "already exists"))
}

ID  <- "232G_puesta1"
tag <- "232G_puesta1"
Species <- "Spheniscus_magellanicus_2014"

# for A files 
gps_ppa <- read.table(glue::glue("./RawData/{Species}/{tag}.TXT"), skip = 20) %>%
  setNames(c("Date", "Time", "lat1", "lat2", "S", "long1", "long2","W","x1","x2"))%>%
  mutate(lat = lat1 + lat2/60) %>%
  mutate(lat = if_else(S == "S", -1*lat, lat))%>%
  mutate(long = long1 + long2/60) %>%
  mutate(long = if_else(W == "W", -1*long, long))%>%
  unite(col = "Datetime", c(Date, Time), sep = " ") %>%
  mutate(Datetime = as.POSIXct(Datetime, format="%d.%m.%Y %H:%M:%S")) %>%
  mutate(Datetime = format(Datetime, "%d/%m/%Y %H:%M:%S"))%>%
  select(c("Datetime","lat","long"))

#for B files (Magellanic 2015, Rockhopper 2015)
gps_ppa <- read.table(glue::glue("./RawData/{Species}/{tag}.TXT"), skip = 20) %>%
  setNames(c("Date", "Time", "lat", "S", "long", "W","v7","v8","v9","v10","v11"))%>%
  mutate(lat = if_else(S == "S", -1*lat, lat))%>%
  mutate(long = if_else(W == "W", -1*long, long))%>%
  unite(col = "Datetime", c(Date, Time), sep = " ") %>%
  mutate(Datetime = as.POSIXct(Datetime, format="%d.%m.%Y %H:%M:%S")) %>%
  mutate(Datetime = format(Datetime, "%d/%m/%Y %H:%M:%S"))%>%
  select(c("Datetime","lat","long"))

view(gps_ppa)
write.csv(gps_ppa, file = glue::glue("./Results/{Species}/{ID}_gps_filtered.csv"), row.names = FALSE)

setwd("C:/...../Data/Results/Spheniscus_magellanicus_2014")
allfilesM2014 <-list.files(pattern="\\.csv$")
all_data = lapply(allfilesM2014, read.csv)
head(allfilesM2014)
all_data <- bind_rows(all_data, .id = "File")
write.csv(all_data, "All_data_Spheniscus_Magellanicus_2014.csv", row.names = FALSE)

##Remove points on-land:
library(sf)
library(rnaturalearth)
library(mapview)
world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
world<- st_make_valid(world) 
lat <- all_data$lat
long <- all_data$long
points <- data.frame("Var1" = long, "Var2" = lat)
pts <- st_as_sf(points, coords=1:2, crs=4326)
ii <- !is.na(as.numeric(st_intersects(pts, world)))
all_data$on_land<- ii
points_sea <- filter(all_data, !grepl("TRUE", all_data$on_land))
mapview(points_sea, xcol="long", ycol="lat")
write.csv(points_sea, "All_data_Spheniscus_Magellanicus_2014_on_sea.csv", row.names = FALSE)
#Repeat for each individual 

#---------------------------------------------------------------------------------------------------------------
setwd("C:/Users/....../Data")
#2) Download rasters and convert/prepare them for analysis 

#Fisheries activities from Global Fishing Watch (already in TIF)
#Maritime Traffic/Vessel density from Global Maritime Traffic (GMTDS, already in TIF)
#Chlorophyll-a and SST from NASA Ocean biology, SSTA from NOAA Coral Reef Watch are in format .nc :
library(raster)
library(ncdf4)
nc_open("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.CHL.chlor_a.4km.nc")
RDec_2014<- raster("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.CHL.chlor_a.4km.nc")
writeRaster(RDec_2014, "./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.CHL.chlor_a.4km.tiff", format = "GTiff")

#---------------------------------------------------------------------------------------------------------------
#3) Create dataframe with all raster values:
fisheries2014<- raster("./Rasters/public-global-fishing-effort-v20231026.tif")
MT2014<- raster("./Rasters/grid_float_All_2014_12_converted.tif")
CHL2014<- raster("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.CHL.chlor_a.4km.tiff")
SST2014<- raster("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.SST4.sst4.4km.tiff")
SSTA2014 <- raster("./Rasters/dhw_5km_fbdc_b4e2_b975_U1708569965747.tif")

    #Upload points 
#rockhopper2014 <- read.csv("./Results/Rockhopper_2014/All_data_Rockhopper_2014_on_sea.csv")
magellanic2014 <- read.csv("./Results/Spheniscus_Magellanicus_2014/All_data_Spheniscus_Magellanicus_2014_on_sea.csv")
    #Check if rasters have same extension:
extents <- lapply(list(fisheries2014, MT2014, SSTA2014, SST2014, CHL2014), extent)
    #Apply same extent for all:
aligned_rasters <- lapply(list(fisheries2014, MT2014, SSTA2014, SST2014, CHL2014), function(r){
  resample(r, MT2014, method = "bilinear")
})
    #Put all rasters together:
climStack <- stack(aligned_rasters)
    # Create an empty dataframe to store the results
extracted_values <- data.frame(matrix(nrow = nrow(magellanic2014), ncol = nlayers(climStack)))
colnames(extracted_values) <- names(climStack)
    # Extract raster values for each point
for (i in 1:nrow(magellanic2014)) {
  extracted_values[i, ] <- extract(climStack, magellanic2014[i, c("long", "lat")])
}
    # Combine extracted values with rockhopper2014 dataframe
Magellanic <- cbind(magellanic2014, extracted_values)
    #Change names columns
names(Magellanic) <- c("File", "Datetime", "lat", "long", "on_land", "Fishing effort", "Maritime Traffic", "SSTA", "SST", "Chlorophyll-a")
cols<- c("File", "Datetime", "lat", "long", "on_land", "Maritime Traffic", "Fishing effort", "SST", "SSTA", "Chlorophyll-a")
Magellanic[, cols][is.na(Magellanic[, cols])] <- 0
  
write.csv(Magellanic, "./Results/Spheniscus_magellanicus_2014/All_values_Magellanic_2014.csv")
#Repeat for every year and for Rockhopper penguins

#-------------------------------------------------------------------------------------------------------------------
#4) Create a grid of 0.5°x0.5° cells containing occurrences of penguins and rasters values 
Mag2014 <- read.csv("./Results/Spheniscus_magellanicus_2014/All_values_Magellanic_2014.csv")

    #Breaks 
lat_breaks <- seq(-56, -54, by = 0.05)
long_breaks <- seq(-66.5, -62.5, by = 0.05)
    # Create cells groups
Mag_grouped <- Mag2014 %>%
  mutate(lat_group = cut(lat, breaks = lat_breaks, labels = FALSE),
         long_group = cut(long, breaks = long_breaks, labels = FALSE)) %>%
  group_by(lat_group, long_group, .drop = FALSE) %>%
  summarize(across(c(Fishing.effort, Maritime.Traffic, SSTA, SST, Chlorophyll.a), ~mean(., na.rm = TRUE)), .groups = "drop")
    # Names groups
names(Mag_grouped)[1:2] <- c("lat_group", "long_group")
    # Calculate center coordinates of each cell 
lat_center <- (lat_breaks[-1] + lat_breaks[-length(lat_breaks)]) / 2
long_center <- (long_breaks[-1] + long_breaks[-length(long_breaks)]) / 2
    # Add center coordinates to dataframe
Mag_grouped <- Mag_grouped %>%
  mutate(lat_center = lat_center[lat_group],
         long_center = long_center[long_group])
    # Count occurrences in each cell
Mag_counts <- Mag2014 %>%
  mutate(lat_group = cut(lat, breaks = lat_breaks, labels = FALSE),
         long_group = cut(long, breaks = long_breaks, labels = FALSE)) %>%
  group_by(lat_group, long_group) %>%
  summarize(count = n(), .groups = "drop")
    # Add occurrences to dataframe
Mag_grouped <- left_join(Mag_grouped, Mag_counts, by = c("lat_group", "long_group"))
head(Mag_grouped)

write.csv(Mag_grouped, "./Results/Spheniscus_magellanicus_2014/Pixels_Magellanic_2014.csv")
  #Repeat for each year and both species

#----------------------------------------------------------------------------------------------------------------

#5) Interaction scores and heatmaps 
library(ggplot2)
library(ggspatial)
library(dplyr)
library(scales)
library(raster)
library(sf)
library(viridis)
library(dplyr)
library(sp)
library(marmap)
library(tidyverse)
library(rnaturalearth)
library(cowplot)
library(ggimage)

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
bat <- getNOAA.bathy(-62.5, -66.5, -54, -56, res = 0.5, keep = TRUE)
bat <- read.bathy("marmap_coord_-66.5;-56;-62.5;-54_res_0.5.csv", header = TRUE)
bat_xyz<- as.xyz(bat)
bat_xyz_filtered <- bat_xyz[bat_xyz$V3 >= -5000 & bat_xyz$V3 <= 500, ]
transparent_value <- 0  

bath_plot2 <- ggplot() + 
  geom_tile(data = bat_xyz_filtered, aes(x = V1, y = V2, fill = V3)) +
  geom_contour(data = subset(bat_xyz_filtered, V3 <= transparent_value), 
               aes(x = V1, y = V2, z = V3),
               colour = "grey99", linewidth = 0.00, alpha = 0.03) +
  geom_sf(data = world, fill = "white", color="transparent") +
  coord_sf(xlim = c(-62.5, -66.5), 
           ylim = c(-54, -56), expand=FALSE) +
  labs(x = "", y = "", fill = "") +
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(location = "bl") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = seq(-66, -62, by = 1))

#Interaction scores with fisheries and Maritime Traffic 
Mag2014 <- read.csv("./Results/Spheniscus_magellanicus_2014/Pixels_Magellanic_2014.csv")
#Rock2014 <- ...
#Rock2015 <- 
#Mag2015 <- 
#Rock2016<-
#Mag2016<- 
#Rock2017 <- 

liste_mag_Rock <- list(Mag2014=Mag2014, Mag2015=Mag2015, Mag2016=Mag2016,Rock2014=Rock2014, 
                       Rock2015=Rock2015, Rock2016=Rock2016, Rock2017=Rock2017)

##Scale the stressors from 0 (min across all years for both sp) to 1(max across all years for both sp): 
abs_min <- c(min(sapply(liste_mag_Rock, function(df) min(df$Fishing.effort))),
             min(sapply(liste_mag_Rock, function(df) min(df$Maritime.Traffic))))

abs_max <- c(max(sapply(liste_mag_Rock, function(df) max(df$Fishing.effort))),
             max(sapply(liste_mag_Rock, function(df) max(df$Maritime.Traffic))))

for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]]$fishing.effort.scaled <- (liste_mag_Rock[[i]]$Fishing.effort - abs_min[1]) / (abs_max[1] - abs_min[1])
  liste_mag_Rock[[i]]$maritime.traffic.scaled <- (liste_mag_Rock[[i]]$Maritime.Traffic - abs_min[2]) / (abs_max[2] - abs_min[2])
}

#Proportion (0-1) of the overall distribution for 1 year for 1 species in this pixel
for (i in seq_along(liste_mag_Rock)) {
  totalcounts <- sum(liste_mag_Rock[[i]]$count)
  liste_mag_Rock[[i]]$proportion <- liste_mag_Rock[[i]]$count / totalcounts *100
}
##Calcul interaction score 
for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]]$fishing.effort_indice <- liste_mag_Rock[[i]]$proportion * liste_mag_Rock[[i]]$fishing.effort.scaled
  liste_mag_Rock[[i]]$maritime.traffic_indice <- liste_mag_Rock[[i]]$proportion * liste_mag_Rock[[i]]$maritime.traffic.scaled
}

# Exporter indices values
for (nom_liste in names(liste_mag_Rock)) {
  nom_fichier <- paste0("Calculated_index", nom_liste, ".csv")
  write.csv(liste_mag_Rock[[nom_liste]], file = nom_fichier, row.names = FALSE)
  cat("Fichier exporté:", nom_fichier, "\n")
}

# Create 14 maps for both species, each year with both stressors
names(liste_mag_Rock) <- c("Magellanic_2014", "Magellanic_2015", "Magellanic_2016", "Southern_Rockhopper_2014", "Southern_Rockhopper_2015", "Southern_Rockhopper_2016", "Southern_Rockhopper_2017")
interaction_types <- c("fishing.effort_indice", "maritime.traffic_indice")

for (interaction_type in interaction_types) {
  min_values <- sapply(liste_mag_Rock, function(df) min(df[[interaction_type]]))
  max_values <- sapply(liste_mag_Rock, function(df) max(df[[interaction_type]]))
  global_min <- min(min_values)
  global_max <- max(max_values)
  
  for (df_name in names(liste_mag_Rock)) {
    species_and_year <- unlist(strsplit(df_name, "_"))
    species <- if (length(species_and_year) > 2) {
      paste(species_and_year[1], species_and_year[2])
    } else {
      species_and_year[1]  
    }
    year <- species_and_year[length(species_and_year)]
    
    # Title
    index_type <- gsub("_indice", "", interaction_type)
    index_type <- gsub("\\.", " ", index_type)
    title <- paste("Exposure score to", index_type, "for", species, "penguins", "\n", "in", year)
    
    # Main heatmap
    heatmap <- ggplot(data = liste_mag_Rock[[df_name]]) +
      geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100), name = "Exposure score", limits = c(global_min, global_max)) +  # Spécifier les limites de l'échelle de couleur
      #labs(title = title, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(legend.position = "none") +
      guides(fill = FALSE) +
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    
    #Legend
    heatmap_légende <- ggplot(data = liste_mag_Rock[[df_name]]) +
      geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100),name = "Exposure score", limits = c(global_min, global_max)) +  # Spécifier les limites de l'échelle de couleur
      #labs(title = title, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    
    #Combine plots
    combined_plot <- bath_plot2 +
      annotation_custom(ggplotGrob(heatmap + theme_void()), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
      labs(title = title)+
      theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=12))
    
    
    legend_heatmap <- cowplot::get_legend(heatmap_légende)
    combined_plot_with_legend <- cowplot::plot_grid(combined_plot, legend_heatmap, ncol = 2, 
                                                    rel_widths = c(4, 1)) 
    png(paste("heatmap_", year, "_", species, "_", index_type, ".png", sep = ""), width = 800, height = 600, units = "px", res = 96)
    print(combined_plot_with_legend)
    dev.off()
  }
}

# Exposure score to SSTA (different scaling than previously so it is calculated aside)
#Scale the stressors from -1 (min across all years for both sp) to 1(max across all years for both sp) : 
min_val <- min((sapply(liste_mag_Rock, function(df) min((df$SSTA)))))
max_val <- max((sapply(liste_mag_Rock, function(df) max((df$SSTA))))) 

scale_values <- function(x) {
  scaled <- (x - min_val) / (max_val - min_val)  
  scaled <- scaled * 2 - 1  
  return(scaled)
}

for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]]$SSTA.scaled <- scale_values(liste_mag_Rock[[i]]$SSTA)
}
## Calculation exposure score
for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]]$SSTA_indice <- liste_mag_Rock[[i]]$proportion * liste_mag_Rock[[i]]$SSTA.scaled
}

# Exporter .csv with indice
for (nom_liste in names(liste_mag_Rock)) {
  nom_fichier <- paste0("Calculated_index", nom_liste, ".csv")
  write.csv(liste_mag_Rock[[nom_liste]], file = nom_fichier, row.names = FALSE)
  cat("Fichier exporté:", nom_fichier, "\n")
}

# 7 heatmaps for exposure each year with SSTA for both species
names(liste_mag_Rock) <- c("Magellanic_2014", "Magellanic_2015", "Magellanic_2016", "Southern_Rockhopper_2014", "Southern_Rockhopper_2015", "Southern_Rockhopper_2016", "Southern_Rockhopper_2017")
interaction_types <- "SSTA_indice"

for (interaction_type in interaction_types) {
  min_values <- sapply(liste_mag_Rock, function(df) min(df[[interaction_type]]))
  max_values <- sapply(liste_mag_Rock, function(df) max(df[[interaction_type]]))
  global_min <- min(min_values)
  global_max <- max(max_values)
  
  for (df_name in names(liste_mag_Rock)) {
    species_and_year <- unlist(strsplit(df_name, "_"))
    species <- if (length(species_and_year) > 2) {
      paste(species_and_year[1], species_and_year[2])
    } else {
      species_and_year[1]  
    }
    year <- species_and_year[length(species_and_year)]
    # Title
    index_type <- gsub("_indice", "", interaction_type)
    index_type <- gsub("\\.", " ", index_type)
    title <- paste("Exposure score to", "sea surface temperature anomalies \n for", species, "penguins in", year)
    # Main heatmap
    heatmap <- ggplot(data = liste_mag_Rock[[df_name]]) +
      geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100), name = "Exposure score", limits = c(global_min, global_max)) +  # Spécifier les limites de l'échelle de couleur
      #labs(title = title, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(legend.position = "none") +
      guides(fill = FALSE) +
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    #Legend
    heatmap_légende <- ggplot(data = liste_mag_Rock[[df_name]]) +
      geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100),name = "Exposure score", limits = c(global_min, global_max)) +  # Spécifier les limites de l'échelle de couleur
      #labs(title = title, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    #Combine plots
    combined_plot <- bath_plot2 +
      annotation_custom(ggplotGrob(heatmap + theme_void()), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
      labs(title = title)+
      theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=12))
    
    
    legend_heatmap <- cowplot::get_legend(heatmap_légende)
    combined_plot_with_legend <- cowplot::plot_grid(combined_plot, legend_heatmap, ncol = 2, 
                                                    rel_widths = c(4, 1)) 
    
    # Export heatmap in pdf
    pdf(paste("heatmap_", year, "_", species, "_", index_type, ".pdf", sep = ""), width = 7, height = 5)
    print(combined_plot_with_legend)
    dev.off()
  }
}

## Mean interannual chlorophyll-a concentration and SST variations:
##----Interaction map between trends SST, CHLA and penguins (all years)-----------
CHL2014<- raster("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.CHL.chlor_a.4km.tiff")
# CHL2015<- 
# CHL2016<- 
# CHL2017<- 

SST2014<- raster("./Rasters/AQUA_MODIS.20141201_20141231.L3m.MO.SST4.sst4.4km.tiff")
# SST2015<- 
# SST2016<- 
# SST2017<- 

reference_extent <- extent(-66.5, -62.5, -56, -54)

CHL2014_cropped <- crop(CHL2014, reference_extent)
CHL2015_cropped <- crop(CHL2015, reference_extent)
CHL2016_cropped <- crop(CHL2016, reference_extent)
CHL2017_cropped <- crop(CHL2017, reference_extent)

SST2014_cropped <- crop(SST2014, reference_extent)
SST2015_cropped <- crop(SST2015, reference_extent)
SST2016_cropped <- crop(SST2016, reference_extent)
SST2017_cropped <- crop(SST2017, reference_extent)

# CHL 3 years for Magellanic
diff_2015_2014 <- CHL2015_cropped - CHL2014_cropped
diff_2016_2015 <- CHL2016_cropped - CHL2015_cropped
average_difference <- (diff_2015_2014 + diff_2016_2015) / 2
plot(average_difference)
writeRaster(average_difference, "./Rasters/CHL2014-2016", format = "GTiff")

#CHL 4 years for Rockhopper
diff_2015_2014 <- CHL2015_cropped - CHL2014_cropped
diff_2016_2015 <- CHL2016_cropped - CHL2015_cropped
diff_2017_2016 <- CHL2017_cropped - CHL2016_cropped
average_difference <- (diff_2015_2014 + diff_2016_2015 + diff_2017_2016 ) / 3
plot(average_difference)
writeRaster(average_difference, "./Rasters/CHL2014-2017", format = "GTiff")

#SST 3 years
diff_2015_2014 <- SST2015_cropped - SST2014_cropped
diff_2016_2015 <- SST2016_cropped - SST2015_cropped
average_difference <- (diff_2015_2014 + diff_2016_2015) / 2
plot(average_difference)
writeRaster(average_difference, "./Rasters/SST2014-2016", format = "GTiff")

#SST 4 years
diff_2015_2014 <- SST2015_cropped - SST2014_cropped
diff_2016_2015 <- SST2016_cropped - SST2015_cropped
diff_2017_2016 <- SST2017_cropped - SST2016_cropped
average_difference <- (diff_2015_2014 + diff_2016_2015 + diff_2017_2016 ) / 3
plot(average_difference)
writeRaster(average_difference, "./Rasters/SST2014-2017", format = "GTiff")

SSTRock<- raster("./Rasters/SST2014-2017.Tif")
CHLRock <- raster("./Rasters/CHL2014-2017.Tif")
SSTMag <- raster("./Rasters/SST2014-2016.Tif")
CHLMag <- raster("./Rasters/CHL2014-2016.Tif")

magellanic_files <- c( 
  "./Results/Spheniscus_magellanicus_2014/Pixels_Magellanic_2014.csv",
  "..../Pixels_Magellanic_2015.csv",
  "..../Pixels_Magellanic_2016.csv")

rockhopper_files <- c(
  ".... /Pixels_Rockhopper_2014.csv",
  "..../Pixels_Rockhopper_2015.csv",
  "..../Pixels_Rockhopper_2016.csv",
  "..../Pixels_Rockhopper_2017.csv")

rockhopper_data <- lapply(rockhopper_files, read.csv)
magellanic_data <- lapply(magellanic_files, read.csv)

rockhopper_merged <- do.call(rbind, rockhopper_data)
magellanic_merged <- do.call(rbind, magellanic_data)

magellanic_counts_total <- aggregate(count ~ lat_center + long_center, data = magellanic_merged, sum)
rockhopper_counts_total <- aggregate(count ~ lat_center + long_center, data = rockhopper_merged, sum)

total_counts_magellanic <- sum(magellanic_counts_total$count)
total_counts_rockhopper <- sum(rockhopper_counts_total$count)

magellanic_counts_total$proportion <- magellanic_counts_total$count / total_counts_magellanic *100
rockhopper_counts_total$proportion <- rockhopper_counts_total$count / total_counts_rockhopper *100

# Exposure index to Mean interannual variation of SST
library(raster)
library(sp)

sst_values_Rock <- extract(SSTRock, rockhopper_counts_total[, c("long_center", "lat_center")])
sst_values_Mag <- extract(SSTMag, magellanic_counts_total[, c("long_center", "lat_center")])

sst_values_Rock <- raster::extract(SSTRock, rockhopper_counts_total[, c("long_center", "lat_center")])
sst_values_Mag <- raster::extract(SSTMag, magellanic_counts_total[, c("long_center", "lat_center")])

rockhopper_counts_total$SST <- sst_values_Rock
magellanic_counts_total$SST  <- sst_values_Mag

liste_sst_mag_Rock <- list(SST_Rock=rockhopper_counts_total, SST_Mag=magellanic_counts_total)

#Scale SST values
abs_max <- max(abs(sapply(liste_sst_mag_Rock, function(df) c(min(magellanic_counts_total$SST, na.rm=TRUE), max(magellanic_counts_total$SST, na.rm=TRUE)))))
for (i in seq_along(liste_sst_mag_Rock)) {
  liste_sst_mag_Rock[[i]]$Scaled_SST <- liste_sst_mag_Rock[[i]]$SST / abs_max
}

rockhopper_interaction_index_SST <- rockhopper_counts_total$proportion * liste_sst_mag_Rock[[1]]$Scaled_SST
magellanic_interaction_index_SST <- magellanic_counts_total$proportion * liste_sst_mag_Rock[[2]]$Scaled_SST

#Exposure index to mean interannual variation of Chl-a concentration 
chl_values_Rock <- extract(CHLRock, rockhopper_counts_total[, c("long_center", "lat_center")])
chl_values_Mag <- extract(CHLMag, magellanic_counts_total[, c("long_center", "lat_center")])

chl_values_Rock <- raster::extract(CHLRock, rockhopper_counts_total[, c("long_center", "lat_center")])
chl_values_Mag <- raster::extract(CHLMag, magellanic_counts_total[, c("long_center", "lat_center")])

df_chl_values_Rock <- data.frame(CHL=chl_values_Rock)
df_chl_values_Mag <- data.frame(CHL=chl_values_Mag)

rockhopper_counts_total$CHL <- chl_values_Rock
magellanic_counts_total$CHL  <- chl_values_Mag

liste_chl_mag_Rock <- list(CHL_Rock=df_chl_values_Rock, CHL_Mag=df_chl_values_Mag)

#---Scale SST values
abs_max2 <- max(abs(sapply(liste_chl_mag_Rock, function(df) c(min(df$CHL, na.rm=TRUE), max(df$CHL, na.rm=TRUE)))))
for (i in seq_along(liste_chl_mag_Rock)) {
  liste_chl_mag_Rock[[i]]$Scaled_CHL <- liste_chl_mag_Rock[[i]]$CHL / abs_max2
}

rockhopper_interaction_index_CHL <- rockhopper_counts_total$proportion * liste_chl_mag_Rock[[1]]$Scaled_CHL
magellanic_interaction_index_CHL <- magellanic_counts_total$proportion * liste_chl_mag_Rock[[2]]$Scaled_CHL

#Min and Max values for colourscale
min_valueSST <-min(c(min(rockhopper_interaction_index_SST, na.rm = TRUE), min(magellanic_interaction_index_SST, na.rm = TRUE)), na.rm = TRUE)
max_valueSST <- max(c(max(rockhopper_interaction_index_SST, na.rm = TRUE), max(magellanic_interaction_index_SST, na.rm = TRUE)), na.rm = TRUE)
min_valueCHL <- min(c(max(rockhopper_interaction_index_CHL, na.rm = TRUE), min(magellanic_interaction_index_CHL, na.rm = TRUE)), na.rm = TRUE)
max_valueCHL <- max(c(max(rockhopper_interaction_index_CHL, na.rm = TRUE), max(magellanic_interaction_index_CHL, na.rm = TRUE)), na.rm = TRUE)

#Export .csv indices
indices <- list(
  Rockhopper_SST = sst_values_Rock,
  Rockhopper_CHL = chl_values_Rock,
  Magellanic_SST = sst_values_Mag,
  Magellanic_CHL = chl_values_Mag)
for (nom_indice in names(indices)) {
  nom_fichier <- paste0("Exposition", nom_indice, ".csv")
  write.csv(indices[[nom_indice]], file = nom_fichier, row.names = FALSE)
  cat("Fichier exporté:", nom_fichier, "\n")
}

# Export exposure score as .csv
indices <- list(
  Rockhopper_SST = rockhopper_interaction_index_SST,
  Rockhopper_CHL = rockhopper_interaction_index_CHL,
  Magellanic_SST = magellanic_interaction_index_SST,
  Magellanic_CHL = magellanic_interaction_index_CHL)

for (name_indice in names(indices)) {
  nom_fichier <- paste0("Indice_", nom_indice, ".csv")
  write.csv(indices[[name_indice]], file = name_fichier, row.names = FALSE)
  cat("Fichier exporté:", name_fichier, "\n")
}

#Heatmaps
species <- c("Southern Rockhopper", "Magellanic")
index_types <- c("sea surface temperature", "chlorophyll a")

for (s in species) {
  for (i in index_types) {
    if ((s == "Southern Rockhopper" && i == "sea surface temperature") || (s == "Southern Rockhopper" && i == "chlorophyll a")) {
      year <- "2014-2017"
    } else if ((s == "Magellanic" && i == "sea surface temperature") || (s == "Magellanic" && i == "chlorophyll a")) {
      year <- "2014-2016"
    }
    counts_total <- switch(s, "Southern Rockhopper" = rockhopper_counts_total, "Magellanic" = magellanic_counts_total)
    interaction_index <- switch(i, "sea surface temperature" = switch(s, "Southern Rockhopper" = rockhopper_interaction_index_SST, "Magellanic" = magellanic_interaction_index_SST), "chlorophyll a" = switch(s, "Southern Rockhopper" = rockhopper_interaction_index_CHL, "Magellanic" = magellanic_interaction_index_CHL))
    min_value <- switch(i, "sea surface temperature" = min_valueSST, "chlorophyll a" = min_valueCHL)
    max_value <- switch(i, "sea surface temperature" = max_valueSST, "chlorophyll a" = max_valueCHL)
    
    # Main heatmap
    heatmap <- ggplot(data = counts_total) +
      geom_tile(aes(x = long_center, y = lat_center, fill = interaction_index), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100), name = "Exposure score", limits = c(min_value, max_value)) +  
      #labs(title = paste("Exposure index between", i, "trend", year, "and", s, "penguin"), x = NULL, y = NULL) + +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(legend.position = "none") +
      guides(fill = FALSE) +
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    
    #Legend
    heatmap_légende <- ggplot(data = counts_total) +
      geom_tile(aes(x = long_center, y = lat_center, fill = interaction_index), width = 0.05, height = 0.05) +
      geom_sf(data = world, fill = "grey20", color = NA) +
      coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
      scale_fill_gradientn(colours=inferno(100),name = "Exposure score", limits = c(min_value, max_value)) +  
      #labs(title = title, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = NA, color = NA),
            plot.title = element_text(size = 16))+
      theme(axis.text = element_text(size = 10)) +
      annotation_scale(location = "bl") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_x_continuous(breaks = seq(-66, -62, by = 1))
    
    #Combine plots
    combined_plot <- bath_plot2 +
      annotation_custom(ggplotGrob(heatmap + theme_void()), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
      labs(title = paste("Exposure score to mean interannual", i, "variation", "\n", "for", s, "penguins in", year))+
      theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=12))
    
    legend_heatmap <- cowplot::get_legend(heatmap_légende)
    combined_plot_with_legend <- cowplot::plot_grid(combined_plot, legend_heatmap, ncol = 2, 
                                                    rel_widths = c(4, 1)) 
    # Export heatmap
    png(paste("heatmap_", year, "_", s, "_", i, ".png", sep = ""), width = 800, height = 600, units = "px", res = 96)
    print(combined_plot_with_legend)
    dev.off()
  }
}

#-------------------------------------------------------------------------------------------------------
#                               Cumulative Maps with 2014-2017 data
#-------------------------------------------------------------------------------------------------------
#1)Bathymetric background map: 
    #see bath_plot2 above

#2)Cumulative maps/year/species (7maps with 3variables, Index 6)
Mag2014 <- read.csv("./Results/Spheniscus_magellanicus_2014/Calculated_indexMag2014.csv")
#Rock2014 <- ...
#Rock2015 <- 
#Mag2015 <- 
#Rock2016<-
#Mag2016<- 
#Rock2017 <- 

selected_cols <- c("lat_center", "long_center", "fishing.effort.scaled", "maritime.traffic.scaled", "SSTA", "count", "proportion")

Rock2014 <- Rock2014[selected_cols]
Rock2015 <- Rock2015[selected_cols]
Rock2016 <- Rock2016[selected_cols]
Rock2017 <- Rock2017[selected_cols]
Mag2014 <- Mag2014[selected_cols]
Mag2015 <- Mag2015[selected_cols]
Mag2016 <- Mag2016[selected_cols]

liste_mag_Rock <- list(
  Rock2014 = Rock2014,
  Rock2015 = Rock2015,
  Rock2016 = Rock2016,
  Rock2017 = Rock2017,
  Mag2014 = Mag2014,
  Mag2015 = Mag2015,
  Mag2016 = Mag2016
)

##Scale SSTA from 0 (min across all years for both sp) to 1(max across all years for both sp) 
abs_min <- min(abs(sapply(liste_mag_Rock, function(df) min(abs(df$SSTA)))))
abs_max <- max(abs(sapply(liste_mag_Rock, function(df) max(abs(df$SSTA))))) 

for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]]$SSTA <- (abs(liste_mag_Rock[[i]]$SSTA/abs_max))
}

for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]] <- liste_mag_Rock[[i]] %>%
    mutate(stressors_sum = SSTA + maritime.traffic.scaled + fishing.effort.scaled)
}

for (i in seq_along(liste_mag_Rock)) {
  liste_mag_Rock[[i]] <- liste_mag_Rock[[i]] %>%
    mutate(exposure_indice = stressors_sum * proportion)
}

# Exporter .csv with indice
for (nom_liste in names(liste_mag_Rock)) {
  nom_fichier <- paste0("Indice_calculé_", nom_liste, ".csv")
  write.csv(liste_mag_Rock[[nom_liste]], file = nom_fichier, row.names = FALSE)
  cat("Fichier exporté:", nom_fichier, "\n")
}

# Heatmaps for Index 6: 
names(liste_mag_Rock) <- c( "Southern_Rockhopper_2014", "Southern_Rockhopper_2015", "Southern_Rockhopper_2016", "Southern_Rockhopper_2017","Magellanic_2014", "Magellanic_2015", "Magellanic_2016")
interaction_type <- "exposure_indice"

min_value <- min(unlist(lapply(liste_mag_Rock, function(df) min(df[[interaction_type]]))))
max_value <- max(unlist(lapply(liste_mag_Rock, function(df) max(df[[interaction_type]]))))

for (df_name in names(liste_mag_Rock)) {
  df <- liste_mag_Rock[[df_name]]
  species_and_year <- unlist(strsplit(df_name, "_"))
  species <- if (length(species_and_year) > 2) {
    paste(species_and_year[1], species_and_year[2])
  } else {
    species_and_year[1]  
  }
  year <- species_and_year[length(species_and_year)]
  
  # Title
  index_type <- gsub("_indice", "", interaction_type)
  index_type <- gsub("\\.", " ", index_type)
  title <- paste("Cumulative exposure score to fishing effort, maritime traffic and SSTA", "\n", "for", species, "penguins", "in", year)
  
  # Main heatmap
  heatmap <- ggplot(data = liste_mag_Rock[[df_name]]) +
    geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
    geom_sf(data = world, fill = "grey20", color = NA) +
    coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
    scale_fill_gradientn(colours=inferno(100), name = "Exposure score", limits = c(min_value, max_value)) +  
    #labs(title = title, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(size = 16))+
    theme(legend.position = "none") +
    guides(fill = FALSE) +
    theme(axis.text = element_text(size = 10)) +
    annotation_scale(location = "bl") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(breaks = seq(-66, -62, by = 1))
  
  #Legend
  heatmap_légende <- ggplot(data = liste_mag_Rock[[df_name]]) +
    geom_tile(aes(x = long_center, y = lat_center, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
    geom_sf(data = world, fill = "grey20", color = NA) +
    coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
    scale_fill_gradientn(colours=inferno(100),name = "Exposure score", limits = c(min_value, max_value)) +  # Spécifier les limites de l'échelle de couleur
    #labs(title = title, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(size = 16))+
    theme(axis.text = element_text(size = 10)) +
    annotation_scale(location = "bl") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(breaks = seq(-66, -62, by = 1))
  
  #Combine plots
  combined_plot <- bath_plot2 +
    annotation_custom(ggplotGrob(heatmap + theme_void()), 
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    labs(title = title)+
    theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=12))
  
  
  legend_heatmap <- cowplot::get_legend(heatmap_légende)
  combined_plot_with_legend <- cowplot::plot_grid(combined_plot, legend_heatmap, ncol = 2, 
                                                  rel_widths = c(4, 1)) 

  png(paste("heatmap_", year, "_", species, "_", index_type, ".png", sep = ""), width = 800, height = 600, units = "px", res = 96)
  print(combined_plot_with_legend)
  dev.off()
}


###---- 2 main summary maps (Index 7): 
lat_breaks <- seq(-55.925, -54.225, by = 0.05)
long_breaks <- seq(-65.625, -62.675, by = 0.05)
coordinates <- expand.grid(Latitude = lat_breaks, Longitude = long_breaks)
head(coordinates)

R2014 <- merge(coordinates, liste_mag_Rock$Southern_Rockhopper_2014, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
R2015 <- merge(coordinates, liste_mag_Rock$Southern_Rockhopper_2015, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
R2016 <- merge(coordinates, liste_mag_Rock$Southern_Rockhopper_2016, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
R2017 <- merge(coordinates, liste_mag_Rock$Southern_Rockhopper_2017, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)

R2014[is.na(R2014)] <- 0
R2015[is.na(R2015)] <- 0
R2016[is.na(R2016)] <- 0
R2017[is.na(R2017)] <- 0

moyenne_ROCK <- data.frame(
  Latitude = R2014$Latitude, 
  Longitude = R2014$Longitude,
  fishing.effort.scaled = numeric(nrow(R2014)), 
  maritime.traffic.scaled = numeric(nrow(R2014)),
  SSTA = numeric(nrow(R2014)),
  count = numeric(nrow(R2014)),
  proportion = numeric(nrow(R2014)),
  stressors_sum = numeric(nrow(R2014)),
  exposure_indice = numeric(nrow(R2014))
)

moyenne_ROCK$fishing.effort.scaled <- rowMeans(cbind(R2014$fishing.effort.scaled, R2015$fishing.effort.scaled, R2016$fishing.effort.scaled, R2017$fishing.effort.scaled), na.rm = TRUE)
moyenne_ROCK$maritime.traffic.scaled <- rowMeans(cbind(R2014$maritime.traffic.scaled, R2015$maritime.traffic.scaled, R2016$maritime.traffic.scaled, R2017$maritime.traffic.scaled), na.rm = TRUE)
moyenne_ROCK$SSTA <- rowMeans(cbind(R2014$SSTA, R2015$SSTA, R2016$SSTA, R2017$SSTA), na.rm = TRUE)
moyenne_ROCK$count <- rowMeans(cbind(R2014$count, R2015$count, R2016$count, R2017$count), na.rm = TRUE)
moyenne_ROCK$proportion <- rowMeans(cbind(R2014$proportion, R2015$proportion, R2016$proportion, R2017$proportion), na.rm = TRUE)
moyenne_ROCK$stressors_sum <- rowMeans(cbind(R2014$stressors_sum, R2015$stressors_sum, R2016$stressors_sum, R2017$stressors_sum), na.rm = TRUE)
moyenne_ROCK$exposure_indice <- rowMeans(cbind(R2014$exposure_indice, R2015$exposure_indice, R2016$exposure_indice, R2017$exposure_indice), na.rm = TRUE)

M2014 <- merge(coordinates, liste_mag_Rock$Magellanic_2014, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
M2015 <- merge(coordinates, liste_mag_Rock$Magellanic_2015, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
M2016 <- merge(coordinates, liste_mag_Rock$Magellanic_2016, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)

M2014[is.na(M2014)] <- 0
M2015[is.na(M2015)] <- 0
M2016[is.na(M2016)] <- 0

moyenne_MAG <- data.frame(
  Latitude = M2014$Latitude, 
  Longitude = M2014$Longitude,
  fishing.effort.scaled = numeric(nrow(M2014)), 
  maritime.traffic.scaled = numeric(nrow(M2014)),
  SSTA = numeric(nrow(M2014)),
  count = numeric(nrow(M2014)),
  proportion = numeric(nrow(M2014)),
  stressors_sum = numeric(nrow(M2014)),
  exposure_indice = numeric(nrow(M2014))
)

moyenne_MAG$fishing.effort.scaled <- rowMeans(cbind(M2014$fishing.effort.scaled, M2015$fishing.effort.scaled, M2016$fishing.effort.scaled), na.rm = TRUE)
moyenne_MAG$maritime.traffic.scaled <- rowMeans(cbind(M2014$maritime.traffic.scaled, M2015$maritime.traffic.scaled, M2016$maritime.traffic.scaled), na.rm = TRUE)
moyenne_MAG$SSTA <- rowMeans(cbind(M2014$SSTA, M2015$SSTA, M2016$SSTA), na.rm = TRUE)
moyenne_MAG$count <- rowMeans(cbind(M2014$count, M2015$count, M2016$count), na.rm = TRUE)
moyenne_MAG$proportion <- rowMeans(cbind(M2014$proportion, M2015$proportion, M2016$proportion), na.rm = TRUE)
moyenne_MAG$stressors_sum <- rowMeans(cbind(M2014$stressors_sum, M2015$stressors_sum, M2016$stressors_sum), na.rm = TRUE)
moyenne_MAG$exposure_indice <- rowMeans(cbind(M2014$exposure_indice, M2015$exposure_indice, M2016$exposure_indice), na.rm = TRUE)

ClimateRock<-read.csv("./Indices_Rockhopper.csv")
ClimateMag<- read.csv("./Indices_Magellanic.csv")

TablaRock <- merge(moyenne_ROCK, ClimateRock, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
TablaRock$SST[is.na(TablaRock$SST)] <- 0
TablaRock$CHL[is.na(TablaRock$CHL)] <- 0

TablaMag <- merge(moyenne_MAG, ClimateMag, by.x = c("Latitude", "Longitude"), by.y = c("lat_center", "long_center"), all.x = TRUE)
TablaMag$SST[is.na(TablaMag$SST)] <- 0
TablaMag$CHL[is.na(TablaMag$CHL)] <- 0

abs_max <- max(abs(TablaMag$CHL))
TablaMag$CHL_rescaled <- (abs(TablaMag$CHL/abs_max))

abs_max2 <- max(abs(TablaMag$SST))
TablaMag$SST_Rescaled <- (abs(TablaMag$SST/abs_max2))

abs_max3 <- max(abs(TablaRock$SST))
TablaRock$SST_Rescaled <- (abs(TablaRock$SST/abs_max3))

abs_max4 <- max(abs(TablaRock$CHL))
TablaRock$CHL_Rescaled <- (abs(TablaRock$CHL/abs_max4))

TablaRock$sum <- rowSums(TablaRock[, c("CHL_Rescaled", "SST_Rescaled", "fishing.effort.scaled", "SSTA", "maritime.traffic.scaled")], na.rm = TRUE)
TablaMag$sum <- rowSums(TablaMag[, c("CHL_rescaled", "SST_Rescaled", "fishing.effort.scaled", "SSTA", "maritime.traffic.scaled")], na.rm = TRUE)

TablaMag <- subset(TablaMag, select = -c(count.x, proportion.x, stressors_sum, exposure_indice, X))
TablaRock <- subset(TablaRock, select = -c(count.x, proportion.x, stressors_sum, exposure_indice, X))

TablaRock$exposure <- TablaRock$sum*TablaRock$proportion.y 
TablaMag$exposure <- TablaMag$sum*TablaMag$proportion.y

TablaMag <- subset(TablaMag, !is.na(count.y))
TablaRock <- subset(TablaRock, !is.na(count.y))

liste_mag_Rock2<- list(TablaRock, TablaMag)

max(liste_mag_Rock2[["Magellanic"]]$exposure)

write.csv(TablaRock, "./TablaRock.csv")
write.csv(TablaMag, "./TablaMag.csv")


# ------------------------------Création des 2 heatmaps Summary--------------------
names(liste_mag_Rock2) <- c( "Southern_Rockhopper", "Magellanic")
interaction_type <- "exposure"

min_value <- min(unlist(lapply(liste_mag_Rock2, function(df) min(df[[interaction_type]]))))
max_value <- max(unlist(lapply(liste_mag_Rock2, function(df) max(df[[interaction_type]]))))

for (df_name in names(liste_mag_Rock2)) {
  df <- liste_mag_Rock2[[df_name]]
 
  title <- paste("Cumulative exposure score to five environmental and anthropogenic stressors", "\n", "for", df_name, "penguins")
  
  # Main heatmap
  heatmap <- ggplot(data = liste_mag_Rock2[[df_name]]) +
    geom_tile(aes(x = Longitude, y = Latitude, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
    geom_sf(data = world, fill = "grey20", color = NA) +
    coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
    scale_fill_gradientn(colours=inferno(100), name = "Exposure score", limits = c(min_value, max_value)) +  
    #labs(title = title, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(size = 16))+
    theme(legend.position = "none") +
    guides(fill = FALSE) +
    theme(axis.text = element_text(size = 10)) +
    annotation_scale(location = "bl") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(breaks = seq(-66, -62, by = 1))
  
  #Legend
  heatmap_légende <- ggplot(data = liste_mag_Rock2[[df_name]]) +
    geom_tile(aes(x = Longitude, y = Latitude, fill = !!rlang::sym(interaction_type)), width = 0.05, height = 0.05) +
    geom_sf(data = world, fill = "grey20", color = NA) +
    coord_sf(xlim = c(-66.5, -62.5), ylim = c(-56, -54), expand = FALSE) +
    scale_fill_gradientn(colours=inferno(100),name = "Exposure score", limits = c(min_value, max_value)) +  # Spécifier les limites de l'échelle de couleur
    #labs(title = title, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(size = 16))+
    theme(axis.text = element_text(size = 10)) +
    annotation_scale(location = "bl") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(breaks = seq(-66, -62, by = 1))
  
  #Combine plots
  combined_plot <- bath_plot2 +
    annotation_custom(ggplotGrob(heatmap + theme_void()), 
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    labs(title = title)+
    theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=12))
  
  
  legend_heatmap <- cowplot::get_legend(heatmap_légende)
  combined_plot_with_legend <- cowplot::plot_grid(combined_plot, legend_heatmap, ncol = 2, 
                                                  rel_widths = c(4, 1)) 
  
  png(paste("heatmap_", df_name, "_", index_type, ".png", sep = ""), width = 800, height = 600, units = "px", res = 96)
  print(combined_plot_with_legend)
  dev.off()
}

#----------------------------------------------------------------------------------------------------
#                SPATIAL ANALYSIS
#------------------------------------------------------------------------------------------
library(dplyr)
library(stats)
library(nlme)
library(MuMIn)

rock2014 <- read.csv("./Indice_calculé_Rock2014.csv")
rock2015 <- read.csv("./Indice_calculé_Rock2015.csv")
rock2016<- read.csv("./Indice_calculé_Rock2016.csv")
rock2017 <- read.csv("./Indice_calculé_Rock2017.csv")

rock2014$year <- 2014
rock2015$year <- 2015
rock2016$year <- 2016
rock2017$year <- 2017
rock2014$species <- "Rockhopper"
rock2015$species <- "Rockhopper"
rock2016$species <- "Rockhopper"
rock2017$species <- "Rockhopper"

mag2014 <- read.csv("./Indice_calculé_Mag2014.csv")
mag2015 <- read.csv("./Indice_calculé_Mag2015.csv")
mag2016<- read.csv("./Indice_calculé_Mag2016.csv")

mag2014$year <- 2014
mag2015$year <- 2015
mag2016$year <- 2016

mag2014$species <- "Magellanic"
mag2015$species <- "Magellanic"
mag2016$species <- "Magellanic"

Rock<- bind_rows(rock2014, rock2015, rock2016, rock2017)
Mag <- bind_rows(mag2014, mag2015, mag2016)
combined_data <- bind_rows(rock2014, rock2015, rock2016, mag2014, mag2015, mag2016)

liste_mag_Rock <- list( Rock2014 = rock2014,  Rock2015 = rock2015,   Rock2016 = rock2016,   Rock2017 = rock2017,   Mag2014 = mag2014,   Mag2015 = mag2015,   Mag2016 = mag2016)
combined_data <- bind_rows(rock2014, rock2015, rock2016, rock2017)


#----1) Check correlation between explicative variables with Pearson correlation coeff

cor(rock2014$SST, rock2014$SSTA) #-0.56
cor(rock2014$SST, rock2014$Chlorophyll.a) #0.83
cor(rock2014$SST, rock2014$marine.traffic) #0.22

cor(rock2015$SST, rock2015$SSTA) #-0.79
cor(rock2015$SST, rock2015$Chlorophyll.a,  use = "pairwise.complete.obs") #0.86
cor(rock2015$SST, rock2015$marine.traffic) #0.03

cor(rock2016$SST, rock2016$SSTA) #0.66
cor(rock2016$SST, rock2016$Chlorophyll.a) #0.79
cor(rock2016$SST, rock2016$marine.traffic) #0.36

cor(rock2017$SST, rock2017$SSTA) #-0.19
cor(rock2017$SST, rock2017$Chlorophyll.a) #0.81
cor(rock2017$SST, rock2017$marine.traffic) #0.24

#-----2) Check distribution and normality of the latitude among all years 
test_shapiro <- function(df) {
  test_result <- shapiro.test(df$lat_center)
  return(test_result$p.value)
}
p_value <- test_shapiro(combined_data)
print(p_value) #1.430492e-05

qqnorm(combined_data$lat_center)
qqline(combined_data$lat_center)

#QQplot for each year, globally quite similar 
qqnorm(rock2014$lat_center)
qqline(rock2014$lat_center)

qqnorm(rock2015$lat_center)
qqline(rock2015$lat_center)

qqnorm(rock2016$lat_center)
qqline(rock2016$lat_center)

qqnorm(rock2017$lat_center)
qqline(rock2017$lat_center)

#3) GLS models

#----------------------------------Maritime Traffic-------------------------------------------
#   1) For each species, variation in year 
#a- Magellanic 
Peso <- varIdent(form= ~ 1|year)

MTPM <- gls(maritime.traffic_indice~ year,weights = Peso, data=Mag)
summary(MTPM)
dredge(MTPM)
anova(MTPM)
games_howell_test(data=Mag, maritime.traffic_indice~year, conf.level = 0.95, detailed=FALSE)

#b- PPA
Peso <- varIdent(form= ~ 1|year)

MTPPA <- gls(maritime.traffic_indice~ year,weights = Peso, data=Rock)
summary(MTPPA)
dredge(MTPPA)
anova(MTPPA)

mean(mag2014$maritime.traffic_indice)
mean(mag2015$maritime.traffic_indice)
mean(mag2016$maritime.traffic_indice)
mean(rock2017$maritime.traffic_indice)

max(rock2014$maritime.traffic_indice)


#     2) Variation between species 
#combined_data$SpYear<-interaction(combined_data$year, combined_data$species)
#combined_data$SpYear = as.factor(combined_data$SpYear)
vf2 <- varIdent(form= ~ 1|SpYear)

GLSMT <- gls(maritime.traffic_indice~SpYear,weights = vf2, data=combined_data)
summary(GLSMT)
dredge(GLSMT)
tab_model(GLSMT)

games_howell_test(data=combined_data, maritime.traffic_indice~SpYear, conf.level = 0.95, detailed=FALSE)

#-----------------------------------SSTA------------------------------------------------------------

#1) For each species, variation in year
##Scale SSTA from 0 (min across all years for both sp) to 1(max across all years for both sp) : 
abs_min <- min(abs(sapply(liste_mag_Rock, function(df) min(abs(df$SSTA)))))
abs_max <- max(abs(sapply(liste_mag_Rock, function(df) max(abs(df$SSTA))))) 

combined_data$SSTA_scaled <- (abs(combined_data$SSTA/abs_max))
Rock$SSTA_scaled <- (abs(Rock$SSTA/abs_max))
Mag$SSTA_scaled <- (abs(Mag$SSTA/abs_max))

combined_data$SSTA_indice <- combined_data$SSTA_scaled * combined_data$proportion
Rock$SSTA_indice <- Rock$SSTA_scaled * Rock$proportion
Mag$SSTA_indice <- Mag$SSTA_scaled * Mag$proportion

#a- Magellanic 
Peso <- varIdent(form= ~ 1|year)

SSTAPM <- gls(SSTA_indice~ year,weights = Peso, data=Mag)
summary(SSTAPM)
dredge(SSTAPM)
anova(SSTAPM)
games_howell_test(data=Mag, SSTA_indice~year, conf.level = 0.95, detailed=FALSE)

mean(Mag$SSTA_indice)

filtered_data <- filter(Mag, year == 2016)
min(filtered_data$SSTA_indice)
max(filtered_data$SSTA_indice)

#b- PPA
Peso <- varIdent(form= ~ 1|year)

SSTAPPA <- gls(SSTA_indice~ year,weights = Peso, data=Rock)
summary(SSTAPPA)
dredge(SSTAPPA)
anova(SSTAPPA)
games_howell_test(data=Rock, SSTA_indice~year, conf.level = 0.95, detailed=FALSE)

filtered_data <- filter(Rock, year == 2017)
min(filtered_data$SSTA_indice)
max(filtered_data$SSTA_indice)
mean(filtered_data$SSTA_indice)

#     2) Variation between species 
combined_data$SpYear<-interaction(combined_data$year, combined_data$species)
combined_data$SpYear = as.factor(combined_data$SpYear)
vf2 <- varIdent(form= ~ 1|SpYear)

GLSSSTA <- gls(SSTA_indice~SpYear,weights = vf2, data=combined_data)

summary(GLSSSTA)
dredge(GLSSSTA)
tab_model(GLSSSTA)

games_howell_test(data=combined_data, SSTA_indice~SpYear, conf.level = 0.95, detailed=FALSE)

## -------------------With SSTA scaled from -1 to +1------------------------------------
min_val <- min((sapply(liste_mag_Rock, function(df) min((df$SSTA)))))
max_val <- max((sapply(liste_mag_Rock, function(df) max((df$SSTA))))) 

scale_values <- function(x) {
  scaled <- (x - min_val) / (max_val - min_val)  
  scaled <- scaled * 2 - 1  
  return(scaled)
}

combined_data$SSTA_scaled_2 <- scale_values(combined_data$SSTA)
Rock$SSTA_scaled_2 <- scale_values(Rock$SSTA)
Mag$SSTA_scaled_2 <- scale_values(Mag$SSTA)

combined_data$SSTA_indice_2 <- combined_data$SSTA_scaled_2 * combined_data$proportion
Rock$SSTA_indice_2 <- Rock$SSTA_scaled_2 * Rock$proportion
Mag$SSTA_indice_2 <- Mag$SSTA_scaled_2 * Mag$proportion

#a- Magellanic 
Peso <- varIdent(form= ~ 1|year)

SSTAPM2 <- gls(SSTA_indice_2~ year,weights = Peso, data=Mag)
summary(SSTAPM2)
dredge(SSTAPM2)
games_howell_test(data=Mag, SSTA_indice_2~year, conf.level = 0.95, detailed=FALSE)

#b- PPA
Peso <- varIdent(form= ~ 1|year)

SSTAPPA2 <- gls(SSTA_indice_2~ year,weights = Peso, data=Rock)
summary(SSTAPPA2)
dredge(SSTAPPA2)
games_howell_test(data=Rock, SSTA_indice_2~year, conf.level = 0.95, detailed=FALSE)

#     2) Variation between species 
combined_data$SpYear<-interaction(combined_data$year, combined_data$species)
combined_data$SpYear = as.factor(combined_data$SpYear)
vf2 <- varIdent(form= ~ 1|SpYear)

GLSSSTA2 <- gls(SSTA_indice_2~SpYear,weights = vf2, data=combined_data)

summary(GLSSSTA2)
dredge(GLSSSTA2)
tab_model(GLSSSTA2)

games_howell_test(data=combined_data, SSTA_indice_2~SpYear, conf.level = 0.95, detailed=FALSE)



## Difference in exposition to mean interannual variation for SST and Chl between species
ClimateRock<-read.csv("./Indices_Rockhopper.csv")
ClimateMag<- read.csv("./Indices_Magellanic.csv")

ClimateRock$species <- "Rockhopper"
ClimateMag$species <- "Magellanic"

Climate_combined_data <- bind_rows(ClimateRock, ClimateMag)

SST <- gls(SST~ species,data=Climate_combined_data,  na.action = na.exclude)
summary(SST)
dredge(SST)
games_howell_test(data=Climate_combined_data, SST~species, conf.level = 0.95, detailed=FALSE)

CHL <- gls(CHL~ species,data=Climate_combined_data,  na.action = na.exclude)
summary(CHL)
dredge(CHL)

games_howell_test(data=Climate_combined_data, CHL~species, conf.level = 0.95, detailed=FALSE)
