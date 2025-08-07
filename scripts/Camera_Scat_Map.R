# Plot PORE Beach Camera Map #################################################
# Author: Frankie Gerraty ####################################################
##############################################################################


# Part 1: Load Packages, Import Datasets ---------------

# Load packages
packages<- c("tidyverse", "readxl", "janitor", "sf", "ggspatial")

pacman::p_load(packages, character.only = TRUE)


# Import Point Reyes National Seashore from NPS administrative boundaries
# Originally downloaded from https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True

PORE <-  st_read("data/shapefiles/Administrative_Boundaries of_National Park_System_Units/nps_boundary.shp") %>% 
  subset(UNIT_CODE == "PORE")%>% 
  st_transform(crs= "WGS84")


#Import california counties shapefile, originally downloaded from https://purl.stanford.edu/jm667wq2232
counties <- st_read("data/shapefiles/stanford-jm667wq2232-shapefile/jm667wq2232.shp") %>% 
  st_make_valid() %>% 
  st_union() 


#Create new shapefile with overlap of counties and PORE shapefiles
PORE_land <-st_intersection (counties, PORE) 



#Import cam data

deployments <- read_csv("data/processed/deployments.csv") %>% 
  select(placename, longitude, latitude) %>% 
  unique() %>% 
  mutate(site_type = if_else(placename %in% 
                             c("BCAM15", "BCAM16", "BCAM17", "BCAM18", "BCAM19", 
                               "BCAM20", "BCAM21", "BCAM22", "BCAM24", "BCAM25", "BCAM27"),
                             "non-seal", "seal"))

#Import scat data

PR_scat <- read_csv("data/raw/PR_scat.csv", 
                    skip = 1) %>% 
  clean_names()



#Inset Map without scat ----------------

inset <- ggplot() +
  geom_sf(data = counties, fill="#FAEED9")+
  geom_sf(data=PORE_land, fill = "#D1E3B3")+
#  geom_point(data = PR_scat, 
#             mapping = aes(longitude, latitude),
#             color = "grey50", alpha = .7, shape = 16)+ 
  geom_point(data = deployments, 
             mapping = aes(longitude, latitude, color = site_type))+
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.88),
           ylim = c(37.98, 38.1),
           expand = FALSE)+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#BDE8FE"))+
  scale_x_continuous(breaks=c(-123, -122.9), name = "")+ # Sets the x (longitude) labels 
  scale_y_continuous(breaks = c(38.0, 38.05), name = "")+
  scale_color_manual(values = c("darkgreen",  "#F27F0C"), 
                     labels = c("Non-Rookery", "Rookery"))+
  labs(color = c("Camera Trap\nSite Type"))+
  theme(legend.position = "inside", 
        legend.position.inside = c(.18, .82),
        legend.box.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(linewidth = 2))+
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2)+
  # Add north arrow
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())


inset


#Export Map

ggsave("output/map/camera_map.png", inset, 
       width = 5, height = 4, units = "in", dpi = 600)




#Inset Map with scat ----------------

inset <- ggplot() +
  geom_sf(data = counties, fill="#FAEED9")+
  geom_sf(data=PORE_land, fill = "#D1E3B3")+
  geom_point(data = PR_scat, 
             mapping = aes(longitude, latitude),
             color = "grey50", alpha = .7, shape = 16)+ 
  geom_point(data = deployments, 
             mapping = aes(longitude, latitude, color = site_type))+
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.88),
           ylim = c(37.98, 38.1),
           expand = FALSE)+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#BDE8FE"))+
  scale_x_continuous(breaks=c(-123, -122.9), name = "")+ # Sets the x (longitude) labels 
  scale_y_continuous(breaks = c(38.0, 38.05), name = "")+
  scale_color_manual(values = c("darkgreen",  "#F27F0C"), 
                     labels = c("Non-Rookery", "Rookery"))+
  labs(color = c("Camera Trap\nSite Type"))+
  theme(legend.position = "inside", 
        legend.position.inside = c(.18, .82),
       legend.box.background = element_rect(color = "black", linewidth = 1),
       panel.border = element_rect(linewidth = 2))+
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2)+
  # Add north arrow
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())


inset

#Export Map

ggsave("output/map/camera_map_scat.png", inset, 
       width = 5, height = 4, units = "in", dpi = 600)


#Main Map without scat ----------------

plot <- ggplot() +
  geom_sf(data = counties, fill="#FAEED9")+
  geom_sf(data=PORE_land, fill = "#D1E3B3")+
#  geom_point(data = PR_scat, 
#             mapping = aes(longitude, latitude),
#             color = "grey50", alpha = .7, shape = 16)+ 
  geom_point(data = deployments, 
             mapping = aes(longitude, latitude, color = site_type))+
  geom_rect(aes(  # outer bounding box / border
    xmin = -123.04, 
    xmax = -122.88, 
    ymin = 37.98, 
    ymax = 38.1),
    fill = NA, 
    colour = "black",
    linewidth = .75)+   
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.7),
           ylim = c(37.85, 38.25),
           expand = FALSE)+
  theme_bw()+
  scale_color_manual(values = c("darkgreen",  "#F27F0C"), 
                     labels = c("Non-Rookery", "Rookery"))+
  
  theme(panel.background = element_rect(fill = "#BDE8FE"),
        panel.border = element_rect(linewidth = 2),
        legend.position = "none")+
  scale_x_continuous(breaks=c(-123, -122.8, -122.6), name = "")+ # Sets the x (longitude) labels 
  scale_y_continuous(breaks = c(37.9, 38.0, 38.1, 38.2), name = "")
plot

ggsave("output/map/main_map.png", plot, 
       width = 3.5, height = 4, units = "in", dpi = 600)



#Main Map with scat ----------------

plot <- ggplot() +
  geom_sf(data = counties, fill="#FAEED9")+
  geom_sf(data=PORE_land, fill = "#D1E3B3")+
  geom_point(data = PR_scat, 
             mapping = aes(longitude, latitude),
             color = "grey50", alpha = .7, shape = 16)+ 
  geom_point(data = deployments, 
             mapping = aes(longitude, latitude, color = site_type))+
  geom_rect(aes(  # outer bounding box / border
    xmin = -123.04, 
    xmax = -122.88, 
    ymin = 37.98, 
    ymax = 38.1),
    fill = NA, 
    colour = "black",
    linewidth = .75)+   
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.7),
           ylim = c(37.85, 38.25),
           expand = FALSE)+
  theme_bw()+
  scale_color_manual(values = c("darkgreen",  "#F27F0C"), 
                     labels = c("Non-Rookery", "Rookery"))+
  
  theme(panel.background = element_rect(fill = "#BDE8FE"),
        panel.border = element_rect(linewidth = 2),
        legend.position = "none")+
  scale_x_continuous(breaks=c(-123, -122.8, -122.6), name = "")+ # Sets the x (longitude) labels 
  scale_y_continuous(breaks = c(37.9, 38.0, 38.1, 38.2), name = "")
plot

ggsave("output/map/main_map_scat.png", plot, 
       width = 3, height = 4, units = "in", dpi = 600)



#California Map 


states <- map_data("state")
ca_df <- subset(states, region == "california")

california <- ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgrey") +
  geom_rect(aes(xmin = -123.05, xmax = -122.7, ymin = 37.85, ymax = 38.25),
            color = "red", fill = NA, linewidth = 1) +
  theme_void()

ggsave("output/map/california.png", california, 
       width = 3, height = 4, units = "in", dpi = 600)
