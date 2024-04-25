# Plot PORE Beach Camera Map #################################################
# Author: Frankie Gerraty ####################################################
##############################################################################


# Part 1: Load Packages, Set Working Directory, Import Datasets ---------------

# Load packages
packages<- c("tidyverse", "readxl", "janitor", "sf", "ggspatial")

pacman::p_load(packages, character.only = TRUE)


# Import Point Reyes National Seashore from NPS administrative boundaries
# Originally downloaded from https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True

PORE <-  st_read("data/raw/Administrative_Boundaries of_National Park_System_Units/nps_boundary.shp") %>% 
  subset(UNIT_CODE == "PORE")%>% 
  st_transform(crs= "WGS84")


#Import california counties shapefile, originally downloaded from https://purl.stanford.edu/jm667wq2232
counties <- st_read("data/raw/stanford-jm667wq2232-shapefile/jm667wq2232.shp") %>% 
  st_make_valid() %>% 
  st_union() 


#Create new shapefile with overlap of counties and PORE shapefiles
PORE_land <-st_intersection (counties, PORE) 



#Import cam data

cam_sites <- read_excel("data/raw/camera_sites.xlsx") %>% 
  clean_names() %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))


#Plot

plot <- ggplot() +
  geom_sf(data = counties, fill="#FAEED9")+
  geom_sf(data=PORE_land, fill = "#D1E3B3")+
  geom_point(data = cam_sites, 
             mapping = aes(longitude, latitude, color = site_type))+
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.8),
           ylim = c(37.98, 38.1),
           expand = FALSE)+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#BDE8FE"))+
  scale_x_continuous(breaks=c(-123, -122.9), name = "")+ # Sets the x (longitude) labels 
  scale_y_continuous(breaks = c(38.0, 38.05), name = "")+
  scale_color_manual(values = c("darkgreen", "#3a86ff", "#F27F0C"), 
                     labels = c("Non-Rookery", "Non-Rookery\n(Wilderness)", "Rookery"))+
  labs(color = c("Camera Trap\nSite Type"))+
  theme(legend.position = c(.865, .75),
        legend.box.background = element_rect(color = "black", linewidth = 1))+
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2)+
  # Add north arrow
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())


plot

#Export Map


pdf("output/cam_sites.pdf", 
    width = 7, height = 5)

plot(plot)

dev.off()
