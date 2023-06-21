library(sf)
library(tidyverse)
library(marmap)
library(magick)
library(sp)
library(adehabitatHR)
library(ggspatial)
library(ggnewscale)
library(ggpubr)
library(RColorBrewer)
library(cowplot)

###### import tracking data and map layers #####
RFB_trips <- read_csv("Data/RFB_2016-2023.csv")
head(RFB_trips)
str(RFB_trips)


chagos = st_read("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/Chagos_v6.shp")
head(chagos)
chagos$DEPTHLABEL = fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")

MPA <- read.csv("/Users/at687/Documents/BIOT/Non-breeding/Data/Map Indian Ocean/MPA.csv", as.is = T)
MPA_shp = st_read("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/ChagosEEZ.shp")

  
colonies <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Colonies.csv", as.is = T)
CAcols <- read_csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Breeding numbers.csv")
RFBcols <- subset(CAcols, Sula.sula > 0)
str(RFBcols)
RFBcols_group <- plyr::ddply(RFBcols, ~Group, summarise, 
                             no.RFB =sum(Sula.sula),
                             lat = mean(lat),
                             lon = mean(lon))

#booby_img = readJPG("/Users/at687/Documents/BIOT/Seabird graphics/white morph red footed booby adult.png", native = TRUE)

chagos_bathy <- getNOAA.bathy(lon1 = 66, lon2 = 78,
                              lat1 = -1.5, lat2 = -11.6, resolution = 1)
chagos_bathy_fort = fortify(chagos_bathy)
head(chagos_bathy_fort)


### map tracks #####

trackmap <- ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.9) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  #geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  geom_sf(data = MPA_shp, inherit.aes = FALSE, col = "grey90", lwd = 0.6, fill = NA)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_color() +
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1)+
  new_scale_color() +
  # geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # geom_path(data = RFB_trips[RFB_trips$ColYear == "NI_2018",], aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia\n(ALL)", "Danger Island\n(2019)", "Nelson's Island\n(2018)", "Nelson's Island\n(2019)")) + 
  geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony), alpha = 0.6) + 
  scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"))+
  labs(x = " ", y = "Latitude (°N)")
trackmap


####### foraging ranges #######

tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  group_by(Colony, BirdID)%>%
  summarise(ind_max = max(Max_distance)) %>%
  ungroup() %>%
  group_by(Colony)%>%
  summarise(mean_range = mean(ind_max),
            max_range = max(ind_max))

colonies_sf <- colonies %>%
  left_join(., tripmetrics, by = "Colony") %>%
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326)
  

colonies_sf_max <- colonies_sf %>%
  st_transform(., CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) %>%
  group_by(Colony)%>%
  st_buffer(., dist = c(.$max_range)*1000) %>%
  st_transform(., 4326) %>%
  mutate(Range = "max")

colonies_sf_mean <- colonies_sf %>%
  st_transform(., CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) %>%
  group_by(Colony)%>%
  st_buffer(., dist = c(.$mean_range)*1000) %>%
  st_transform(., 4326)  %>%
  mutate(Range = "mean")

colony_ranges <- rbind(colonies_sf_max, colonies_sf_mean)

ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_fill() +
  geom_sf(data = colony_ranges, aes(col = Colony, lty = Range), fill = NA, lwd = 0.5)+
  # scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                    guide="none") +
  # scale_fill_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                  labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                  guide="none") +
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_linetype_manual(values = c("dotdash", "dashed"), guide = "none")

####### kernel areas #######

for (j in unique(RFB_trips$Colony)){
  RFBsp <- RFB_trips[RFB_trips$Colony == j,c("Longitude", "Latitude", "Colony")]
  colnames(RFBsp)[3] <- "id"
  coordinates(RFBsp) <- c("Longitude", "Latitude")
  RFBsf <- st_as_sfc(RFBsp)
  st_crs(RFBsf) <- 4326
  RFBlaea <- st_transform(RFBsf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 
  
  bbox <- st_bbox(RFBlaea)
  
  x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
  y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
  xy <- expand.grid(x=x,y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  
  RFBsp <- as(RFBlaea, "Spatial")
  
  RFB_UD <- kernelUD(RFBsp, kern = "bivnorm", grid = xy, same4all = F)
  
  RFB_kernel_25 <- getverticeshr(RFB_UD, percent = c(25)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_25"), percent = 25)
  RFB_kernel_50 <- getverticeshr(RFB_UD, percent = c(50)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_50"), percent = 50)
  RFB_kernel_75 <- getverticeshr(RFB_UD, percent = c(75)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_75"), percent = 75)
  RFB_kernel_95 <- getverticeshr(RFB_UD, percent = c(95)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_95"), percent = 95)
  
  kernel_df <- do.call(rbind, lapply(ls(pattern = "RFB_kernel_"), get))
  kernel_df$Colony <- j
  
  write_csv(kernel_df, paste0("Data/kernels/kernel_",j, ".csv" ))
  
}

kernel_df_DG <- read_csv("Data/kernels/kernel_DG.csv")
kernel_df_NI <- read_csv("Data/kernels/kernel_NI.csv")
kernel_df_DI <- read_csv("Data/kernels/kernel_DI.csv")

kernel_df <- rbind(kernel_df_DG, kernel_df_NI, kernel_df_DI)
head(kernel_df)

sf_col <- sf::st_as_sf(kernel_df, coords = c("long", "lat"))
st_crs(sf_col) <- CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")
sf_col_ll <- st_transform(sf_col, 4326) 

#st_write(sf_col_ll, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Col kernels/kernels_ALL.shp")

sf_col_ll_poly = st_sf(
  aggregate(
    sf_col_ll$geometry,
    list(sf_col_ll$piece, sf_col_ll$percent, sf_col_ll$Colony),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

sf_col_ll_poly.o <- sf_col_ll_poly[order(-sf_col_ll_poly$Group.2),]
sf_col_ll_poly.o$Group.2 <- as.factor(sf_col_ll_poly.o$Group.2)

map_kernel <- ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_fill() +
  geom_sf(data = sf_col_ll_poly.o, aes(col = Group.3, fill = Group.3, alpha = Group.2), lwd = 0.4)+
  # scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                    guide="none") +
  # scale_fill_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                  labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                  guide="none") +
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o$Group.2)),"% UD",range = c(0.1, 1))+
  geom_sf(data = colony_ranges, aes(col = Colony, lty = Range), fill = NA, lwd = 0.75)+
  scale_linetype_manual(values = c("dotted", "dashed"), guide = "none")+
  #guides(fill = guide_legend(override.aes = list(alpha = 1)))+
  #geom_point(data = RFBcols, aes(x = lon, y = lat), inherit.aes = FALSE, cex = 0.5, col = "gray40")+
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())
map_kernel


# inset scale #####
world_map <- map_data("world")

gg_inset_map <- ggplot() +
  coord_cartesian(xlim = c(25, 120), ylim = c(-40, 25), expand = F)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="gray60", colour = "gray60")+
  theme_void()+
  #geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "gray50", lwd = 0.6)+
  annotate("rect", xmin = 67, xmax = 77, ymin = -11.2, ymax = -2, fill = "NA", col = "blue4")+
  theme(panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "gray20", fill=NA, linewidth =0.8))
gg_inset_map


# adjust track plot to combine ####
trackmap2 <- ggplot() + 
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_sf(data = sf_col_ll_poly.o, aes(col = Group.3, fill = Group.3, alpha = Group.2), lwd = 0.4)+
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                    guide="none") +
  geom_sf(data = colony_ranges, aes(col = Colony, lty = Range), fill = NA, lwd = 0.75)+
  scale_linetype_manual(values = c("dotted", "dashed"), name = "Range")+
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y), fill = "white") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o$Group.2)),"% UD",range = c(0.1, 1))+
  new_scale_fill() +
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  #geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  geom_sf(data = MPA_shp, inherit.aes = FALSE, col = "grey90", lwd = 0.6, fill = NA)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_color() +
  # geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # geom_path(data = RFB_trips[RFB_trips$ColYear == "NI_2018",], aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia\n(ALL)", "Danger Island\n(2019)", "Nelson's Island\n(2018)", "Nelson's Island\n(2019)")) + 
  geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony), alpha = 0.75) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1),order = 1),
         alpha = guide_legend(order = 2),
         linetype = guide_legend(order = 3),
         size = guide_legend(order = 4))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())
  
trackmap2

# Angles #####

angles.plot <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/DepartureAngles.csv", as.is = T)
angles.plot$ColonyLong <- as.factor(angles.plot$Colony)
levels(angles.plot$ColonyLong)
levels(angles.plot$ColonyLong) <- c("Diego Garcia", "Danger Island", "Nelson's Island")

arrowdf <- data.frame(
  ColonyLong = as.factor(c("Diego Garcia", "Diego Garcia","Danger Island", "Nelson's Island","Nelson's Island")),
  Colony = c("DG", "DG","DI", "NI", "NI"),
  x = c(135,315,135,315,135),
  xend= c(135,315,135,315,135)
)

angle.plot <- ggplot(subset(angles.plot, distance == "max dist"), aes(x = theta, group = Colony, fill = Colony)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks=seq(0, 330, by=30), expand=c(0,0), lim=c(0, 360),
                     labels = c("90", " ", " ", "0/360", " ", " ", "270", " ", " ", "180", " ", " ")) +
  coord_polar(start = -1.57, direction=-1) +
  geom_segment(data = arrowdf, 
               aes(x = x, xend = xend, y = Inf, yend = 13), 
               colour = "grey30", size = 0.5, arrow = arrow(length = unit(.2,"cm")))+
  theme_minimal() + 
  ylim(0, 18) +
  xlab(NULL)+ylab(NULL) + 
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"), guide = "none") + 
  facet_grid(ColonyLong~.) +
  theme(panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())
angle.plot

# Combine plots #####

trackplots <- ggarrange(
  trackmap2, map_kernel, labels = c("a", "b"), ncol = 1, common.legend = T, legend = "right", align = "v"
)

track.combo<-cowplot::plot_grid(trackplots, angle.plot, labels = c('', 'c'), ncol = 2, rel_widths = c(2.2,1))
RFBimg <- "/Users/at687/Documents/BIOT/Seabird graphics/white morph red footed booby adult.png"

t <- ggdraw() +
  draw_plot(track.combo) +
  draw_plot(gg_inset_map, x = 0.1, y = 0.83, width = 0.18, height = 0.14)+
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0,
    width = 0.22
  )



png("Figures/Tracks.png",width = 21, height = 20, units = "cm", res = 300)
print(t)
dev.off()







##### kernel overlap #####

##### re run kernels as one object for BA analyses

RFBsp <- RFB_trips[,c("Longitude", "Latitude", "Colony")]
colnames(RFBsp)[3] <- "id"
coordinates(RFBsp) <- c("Longitude", "Latitude")
RFBsf <- st_as_sfc(RFBsp)
st_crs(RFBsf) <- 4326
RFBlaea <- st_transform(RFBsf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(RFBlaea)

x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

RFBsp <- as(RFBlaea, "Spatial")
RFBsp$id <- RFB_trips[,c("Colony")]

RFB_UD <- kernelUD(RFBsp, kern = "bivnorm", grid = xy, same4all = F)

BA <- kerneloverlaphr(RFB_UD, method = c("BA"))
write.csv(BA, "Data/kernels/BAvalues.csv", row.names = T)

##### Repeat above for 2019: tracking data from all colonies ####
RFB_2019 <- RFB_trips %>%
  filter(Year == "2019")
n_distinct(RFB_2019$BirdID)
RFB_2019 %>%
  group_by(Colony) %>%
  summarise(n = n_distinct(BirdID))

### adjust track plot to combine 
trackmap2019 <- ggplot() + 
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_sf(data = sf_col_ll_poly.o, aes(col = Group.3, fill = Group.3, alpha = Group.2), lwd = 0.4)+
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                    guide="none") +
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y), fill = "white") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o$Group.2)),"% UD",range = c(0.1, 1))+
  new_scale_fill() +
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  #geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  geom_sf(data = MPA_shp, inherit.aes = FALSE, col = "grey90", lwd = 0.6, fill = NA)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_color() +
  # geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # geom_path(data = RFB_trips[RFB_trips$ColYear == "NI_2018",], aes(x = Longitude, y = Latitude, group = TripID, colour = ColYear), alpha = 0.6) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia\n(ALL)", "Danger Island\n(2019)", "Nelson's Island\n(2018)", "Nelson's Island\n(2019)")) + 
  geom_path(data = RFB_2019, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony), alpha = 0.75) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1),order = 1),
         alpha = guide_legend(order = 2),
         linetype = guide_legend(order = 3),
         size = guide_legend(order = 4))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())

trackmap2019

### kernel areas 

for (j in unique(RFB_2019$Colony)){
  RFBsp <- RFB_2019[RFB_2019$Colony == j,c("Longitude", "Latitude", "Colony")]
  colnames(RFBsp)[3] <- "id"
  coordinates(RFBsp) <- c("Longitude", "Latitude")
  RFBsf <- st_as_sfc(RFBsp)
  st_crs(RFBsf) <- 4326
  RFBlaea <- st_transform(RFBsf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 
  
  bbox <- st_bbox(RFBlaea)
  
  x <- seq(as.numeric(bbox["xmin"])-50000,as.numeric(bbox["xmax"])+50000,by=1000) # by = 1km grid
  y <- seq(as.numeric(bbox["ymin"])-50000,as.numeric(bbox["ymax"])+50000,by=1000)
  xy <- expand.grid(x=x,y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  
  RFBsp <- as(RFBlaea, "Spatial")
  
  RFB_UD <- kernelUD(RFBsp, kern = "bivnorm", grid = xy, same4all = F)
  
  RFB_kernel_25 <- getverticeshr(RFB_UD, percent = c(25)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_25"), percent = 25)
  RFB_kernel_50 <- getverticeshr(RFB_UD, percent = c(50)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_50"), percent = 50)
  RFB_kernel_75 <- getverticeshr(RFB_UD, percent = c(75)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_75"), percent = 75)
  RFB_kernel_95 <- getverticeshr(RFB_UD, percent = c(95)) %>% fortify() %>% mutate(id = j, group = paste0(j,"_95"), percent = 95)
  
  kernel_df <- do.call(rbind, lapply(ls(pattern = "RFB_kernel_"), get))
  kernel_df$Colony <- j
  
  write_csv(kernel_df, paste0("Data/kernels/kernel_",j, "_2019.csv" ))
  
}

kernel_df_DG_2019 <- read_csv("Data/kernels/kernel_DG_2019.csv")
kernel_df_NI_2019 <- read_csv("Data/kernels/kernel_NI_2019.csv")
kernel_df_DI_2019 <- read_csv("Data/kernels/kernel_DI_2019.csv")

kernel_df_2019 <- rbind(kernel_df_DG_2019, kernel_df_NI_2019, kernel_df_DI_2019)
head(kernel_df_2019)

sf_col_2019 <- sf::st_as_sf(kernel_df_2019, coords = c("long", "lat"))
st_crs(sf_col_2019) <- CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")
sf_col_ll_2019 <- st_transform(sf_col_2019, 4326) 

#st_write(sf_col_ll, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Col kernels/kernels_ALL.shp")

sf_col_ll_poly_2019 = st_sf(
  aggregate(
    sf_col_ll_2019$geometry,
    list(sf_col_ll_2019$piece, sf_col_ll_2019$percent, sf_col_ll_2019$Colony),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

sf_col_ll_poly.o_2019 <- sf_col_ll_poly_2019[order(-sf_col_ll_poly$Group.2),]
sf_col_ll_poly.o_2019$Group.2 <- as.factor(sf_col_ll_poly.o_2019$Group.2)

map_kernel_2019 <- ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_fill() +
  geom_sf(data = sf_col_ll_poly.o_2019, aes(col = Group.3, fill = Group.3, alpha = Group.2), lwd = 0.4)+
  # scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                    guide="none") +
  # scale_fill_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                  labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
  #                  guide="none") +
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                    guide="none") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o$Group.2)),"% UD",range = c(0.1, 1))+
  #guides(fill = guide_legend(override.aes = list(alpha = 1)))+
  #geom_point(data = RFBcols, aes(x = lon, y = lat), inherit.aes = FALSE, cex = 0.5, col = "gray40")+
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())
map_kernel_2019


trackplots_2019 <- ggarrange(
  trackmap2019, map_kernel_2019, labels = c("a", "b"), ncol = 1, common.legend = T, legend = "right", align = "h"
)


png("Figures/Tracks_2019.png",width = 15, height = 20, units = "cm", res = 300)
print(trackplots_2019)
dev.off()


#### BA Overlap 2019 

RFB2019sp <- RFB_trips %>%
  filter(Year == 2019) %>%
  dplyr::select(Longitude, Latitude, Colony)
colnames(RFB2019sp)[3] <- "id"
coordinates(RFB2019sp) <- c("Longitude", "Latitude")
RFB2019sf <- st_as_sfc(RFB2019sp)
st_crs(RFB2019sf) <- 4326
RFB2019laea <- st_transform(RFB2019sf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(RFB2019laea)

x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

RFB2019sp <- as(RFB2019laea, "Spatial")
RFB2019sp$id <- RFB_trips %>%
  filter(Year == 2019) %>%
  dplyr::select(Colony)

RFB2019_UD <- kernelUD(RFB2019sp, kern = "bivnorm", grid = xy, same4all = F)

BA2019 <- kerneloverlaphr(RFB2019_UD, method = c("BA"))
write.csv(BA2019, "Data/kernels/BAvalues_2019.csv", row.names = T)


#### angles at different track segments ####

angles$distance <- as.factor(angles$distance)
levels(angles$distance)
angles$distance <- fct_relevel(angles$distance, "1km", "5km", "10km", "25km", "max dist")


supp.angle.plot <- ggplot(angles, aes(x = theta, group = Colony, fill = Colony)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks=seq(0, 330, by=30), expand=c(0,0), lim=c(0, 360),
                     labels = c("90", " ", " ", "0/360", " ", " ", "270", " ", " ", "180", " ", " ")) +
  coord_polar(start = -1.57, direction=-1) +
  #scale_size() +
  theme_minimal() + 
  ylim(0, 18) +
  xlab(NULL)+
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  facet_grid(ColonyLong~distance) +
  theme(panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank(),
        legend.position = "bottom", axis.text.x = element_text(size = 7))
supp.angle.plot

png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/Distance_depAngle.png",width = 22, height = 16, units = "cm", res = 300)
print(supp.angle.plot)
dev.off()

