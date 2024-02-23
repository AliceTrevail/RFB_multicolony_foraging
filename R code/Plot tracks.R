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
library(patchwork)

###### import tracking data and map layers #####
RFB_trips <- read_csv("Data/RFB_2016-2023.csv") %>%
  mutate(colony_sub = case_when(Colony == "DG" & Year != 2022 ~ "BP",
                                Colony == "DG" & Year == 2022 ~ "EI", 
                                .default = Colony),
         Colony_f = factor(colony_sub, levels = c("BP", "EI", "DI", "NI")))
head(RFB_trips)
str(RFB_trips)

# read in map layers 
chagos = st_read("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/Chagos_v6.shp")
head(chagos)
chagos$DEPTHLABEL = fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")

MPA <- read.csv("/Users/at687/Documents/BIOT/Non-breeding/Data/Map Indian Ocean/MPA.csv", as.is = T)
MPA_shp = st_read("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/ChagosEEZ.shp")

  
colonies <- read.csv("Data/Colonies.csv", as.is = T) %>% filter(!Colony == "DG")
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

col_values <- brewer.pal(9, "RdPu")[c(8,6,5,3)]
col_labs <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island")

### map tracks #####

trackmap <- ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
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
  geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony_f), alpha = 0.6) + 
  scale_colour_manual(values = col_values, "Colony",
                     labels = col_labs) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"))+
  labs(x = " ", y = "Latitude (°N)")
#trackmap


####### foraging ranges #######

tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  mutate(trip_length_days = case_when(Trip_duration > 24 ~ "multi", .default = "single"),
         colony_sub = case_when(Colony == "DG" & Year != 2022 ~ "BP",
                                Colony == "DG" & Year == 2022 ~ "EI", 
                                .default = Colony),
         Colony_f = factor(colony_sub, levels = c("BP", "EI", "DI", "NI"))) %>%
  group_by(Colony_f, BirdID)%>%
  summarise(ind_max = max(Max_distance)) %>%
  ungroup() %>%
  group_by(Colony_f)%>%
  summarise(mean_range = mean(ind_max),
            max_range = max(ind_max))

colonies_sf <- colonies %>%
  rename(Colony_f = Colony) %>%
  right_join(., tripmetrics) %>%
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326)
  

colonies_sf_max <- colonies_sf %>%
  st_transform(., CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) %>%
  group_by(Colony_f)%>%
  st_buffer(., dist = c(.$max_range)*1000) %>%
  st_transform(., 4326) %>%
  mutate(Range = "max")

colonies_sf_mean <- colonies_sf %>%
  st_transform(., CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) %>%
  group_by(Colony_f)%>%
  st_buffer(., dist = c(.$mean_range)*1000) %>%
  st_transform(., 4326)  %>%
  mutate(Range = "mean")

# colony_ranges <- rbind(colonies_sf_max, colonies_sf_mean)
# 
# ggplot() + 
#   geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
#   scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
#   scale_y_continuous(expand = c(-0.05,-0.05))+
#   geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
#   scale_fill_viridis_c(option="mako", "Depth (m)") + 
#   new_scale_fill() +
#   geom_sf(data = colony_ranges, aes(col = Colony_f, lty = Range), fill = NA, lwd = 0.5)+
#   scale_color_manual(values = col_values, "Colony",
#                      #labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
#                      guide="none") +
#   scale_linetype_manual(values = c("dotdash", "dashed"), guide = "none")


colony_ranges <- colonies_sf_mean

ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.8) +
  scale_x_continuous(limits = c(67.5, 76.4), expand = c(0,0))+
  scale_y_continuous(expand = c(-0.05,-0.05))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "grey90", lwd = 0.6)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_fill() +
  geom_sf(data = colony_ranges, aes(col = Colony_f), linetype = "dashed", fill = NA, lwd = 0.5)+
  scale_color_manual(values = col_values, "Colony",
                     #labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none")

####### kernel areas #######

for (j in unique(RFB_trips$Colony_f)){
  RFBsp <- RFB_trips[RFB_trips$Colony_f == j,c("Longitude", "Latitude", "Colony")]
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
  kernel_df$Colony_f <- j
  
  write_csv(kernel_df, paste0("Data/kernels/kernel_",j, ".csv" ))
  
}

kernel_df_BP <- read_csv("Data/kernels/kernel_BP.csv")
kernel_df_EI <- read_csv("Data/kernels/kernel_EI.csv")
kernel_df_NI <- read_csv("Data/kernels/kernel_NI.csv")
kernel_df_DI <- read_csv("Data/kernels/kernel_DI.csv")

kernel_df <- rbind(kernel_df_BP, kernel_df_EI, kernel_df_NI, kernel_df_DI)
head(kernel_df)

sf_col <- sf::st_as_sf(kernel_df, coords = c("long", "lat"))
st_crs(sf_col) <- CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")
sf_col_ll <- st_transform(sf_col, 4326) 

#st_write(sf_col_ll, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Col kernels/kernels_ALL.shp")

sf_col_ll_poly = st_sf(
  aggregate(
    sf_col_ll$geometry,
    list(sf_col_ll$piece, sf_col_ll$percent, sf_col_ll$Colony_f),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

sf_col_ll_poly.o <- sf_col_ll_poly[order(-sf_col_ll_poly$Group.2),]
sf_col_ll_poly.o$Group.2 <- as.factor(sf_col_ll_poly.o$Group.2)
sf_col_ll_poly.o$Group.3 = factor(sf_col_ll_poly.o$Group.3, levels = c("BP", "EI", "DI", "NI"))

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
  scale_color_manual(values = col_values, "Colony",
                     #labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = col_values, "Colony",
                     #labels = c("Diego Garcia", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o$Group.2)),"% UD",range = c(0.1, 1))+
  geom_sf(data = colony_ranges, aes(col = Colony_f), fill = NA, lwd = 0.75, linetype = "dashed")+
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
  scale_color_manual(values = col_values, "Colony", labels = col_labs, guide="none") +
  scale_fill_manual(values = col_values, "Colony", labels = col_labs, guide="none") +
  geom_sf(data = colony_ranges, aes(col = Colony_f), fill = NA, lwd = 0.75, linetype = "dashed")+
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
  geom_path(data = RFB_trips, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony_f), alpha = 0.75) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  scale_color_manual(values = col_values, "Colony", labels = col_labs) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1),order = 1),
         alpha = guide_legend(order = 2),
         linetype = guide_legend(order = 3),
         size = guide_legend(order = 4))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())
  
trackmap2

# Angle plot #####

angles.plot <- read.csv("Data/DepartureAngles.csv", as.is = T)
angles.plot$Colony_f <- factor(angles.plot$Colony_f, levels = c("BP", "EI", "DI", "NI"))
levels(angles.plot$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")
unique(angles.plot$Colony_f)

arrowdf <- data.frame(
  Colony_f = c("DG - BP", "DG - BP", "DG - EI", "DI", "NI", "NI"),
  x = c(135,315,135,135,315,135),
  xend= c(135,315,135,135,315,135)
)

angle.plot <- ggplot(subset(angles.plot, distance == "max dist"), aes(x = theta, group = Colony_f, fill = Colony_f)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks=seq(0, 330, by=30), expand=c(0,0), lim=c(0, 360),
                     labels = c("90", " ", " ", "0/360", " ", " ", "270", " ", " ", "180", " ", " ")) +
  coord_polar(start = -1.57, direction=-1, clip = "off") +
  geom_segment(data = arrowdf, 
               aes(x = x, xend = xend, y = Inf, yend = 13), 
               colour = "grey30", size = 0.5, arrow = arrow(length = unit(.2,"cm")))+
  theme_minimal() + 
  ylim(0, 18) +
  xlab(NULL)+ylab(NULL) + 
  scale_fill_manual(values = col_values, "Colony",
                     labels = col_labs, guide = "none") + 
  facet_grid(Colony_f~.) +
  #scale_y_log10()+
  theme(panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank(), axis.text.y = element_text(hjust = 0.5))
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


p1 <- trackmap2
p2 <- map_kernel
p3 <- angle.plot+theme(plot.margin = margin(0,0,0,0))
pinset <- gg_inset_map


design <- "
 134
 234
"

plotcombo <- p1 + inset_element(pinset, left = 0.06, bottom = 0.65, right = 0.45, top = 0.95) + p2 + p3 + guide_area() + 
  plot_layout(design = design, guides = "collect", widths = c(1.5,1,0.6)) + 
  plot_annotation(tag_levels = list(c("a", "", "b", "c"))) & 
  theme(plot.tag.position = c(0, 0.95), plot.tag = element_text(size = 14, hjust = 0, vjust = 0, face = "bold"))

t <- ggdraw() +
  draw_plot(plotcombo) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0,
    width = 0.23
  )


png("Figures/Tracks.png",width = 24, height = 22, units = "cm", res = 300)
print(t)
dev.off()


# plot tracks by intra-col levels #####
sex_values <- brewer.pal(7, "Greens")[c(6,3)]
triplength_values <- brewer.pal(8, "Blues")[c(7,4)]
monsoon_values <- brewer.pal(8, "YlOrBr")[c(7,4)]
brstage_values <- brewer.pal(8, "Purples")[c(7,4)]
FE_levels <- c("Monsoon", "Sex", "Breeding Stage", "Trip length")

colonies$Colony_f <- factor(colonies$Colony, levels = c("BP", "EI", "DI", "NI"))
levels(colonies$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")

# tracks ~ sex ####
RFB_trips_knownsex <- RFB_trips %>%
  filter(Sex %in% c("F", "M")) %>%
  dplyr::select(Colony_f, Longitude, Latitude, TripID, Sex) %>%
  mutate(FE = factor("Sex", levels = FE_levels),
         Colony_f = factor(Colony_f, levels = c("BP", "EI", "DI", "NI")))

levels(RFB_trips_knownsex$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")




# tracks ~ triplength ####
RFB_tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  mutate(trip_length_days = case_when(Trip_duration > 24 ~ "Multi-day", .default = "Single day"),
         colony_sub = case_when(Colony == "DG" & Year != 2022 ~ "BP",
                                Colony == "DG" & Year == 2022 ~ "EI", 
                                .default = Colony))

RFB_trips_tripmetrics <- RFB_trips %>%
  left_join(., RFB_tripmetrics) %>%
  mutate(FE = factor("Trip length", levels = FE_levels),
         Colony_f = factor(colony_sub, levels = c("BP", "EI", "DI", "NI"))) %>%
  dplyr::select(Colony_f, Longitude, Latitude, TripID, trip_length_days, FE)
  
levels(RFB_trips_tripmetrics$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")


# tracks ~ monsoon ####
RFB_trips_monsoon <- RFB_trips %>%
  dplyr::select(Colony_f, Longitude, Latitude, TripID, Monsoon) %>%
  mutate(FE = factor("Monsoon", levels = FE_levels),
         Colony_f = factor(Colony_f, levels = c("BP", "EI", "DI", "NI")))

levels(RFB_trips_monsoon$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")


# tracks ~ brstage ####
RFB_trips_brstage <- RFB_trips %>%
  filter(Breed_Stage %in% c("chick-rearing", "incubation")) %>%
  dplyr::select(Colony_f, Longitude, Latitude, TripID, Breed_Stage) %>%
  mutate(FE = factor("Breeding Stage", levels = FE_levels),
         Colony_f = factor(Colony_f, levels = c("BP", "EI", "DI", "NI")))

levels(RFB_trips_brstage$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")

# tracks ~ intracol plot ####
BA_IC <- read_csv("Data/kernels/BAvalues_Intracol.csv") %>%
  mutate(Colony_f = factor(Colony_f, levels = c("BP", "EI", "DI", "NI")),
         BA_text = paste0("BA = ", formatC(BA, digits = 2, format = "f")),
         FE = factor(FE, levels = FE_levels))
levels(BA_IC$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")
BA_IC

trackmap_intracol <- ggplot() + 
  scale_x_continuous(expand = c(0.05,0.05))+
  scale_y_continuous(expand = c(0.05,0.05))+
  geom_sf(data = filter(chagos, DEPTHLABEL == "land"), inherit.aes = FALSE, fill = NA, col = "grey30", lwd = 0.6)+
  geom_path(data = RFB_trips_knownsex, aes(x = Longitude, y = Latitude, group = TripID, colour = Sex), alpha = 0.6) + 
  scale_colour_manual(values = sex_values, labels = c("Female","Male")) + 
  new_scale_color() +
  geom_path(data = RFB_trips_tripmetrics, aes(x = Longitude, y = Latitude, group = TripID, colour = trip_length_days), alpha = 0.6) + 
  scale_colour_manual(values = triplength_values, name = "Trip length") + 
  new_scale_color() +
  geom_path(data = RFB_trips_monsoon, aes(x = Longitude, y = Latitude, group = TripID, colour = Monsoon), alpha = 0.6) + 
  scale_colour_manual(values = monsoon_values, name = "Monsoon season") + 
  new_scale_color() +
  geom_path(data = RFB_trips_brstage, aes(x = Longitude, y = Latitude, group = TripID, colour = Breed_Stage), alpha = 0.6) + 
  scale_colour_manual(values = brstage_values, name = "Breeding stage", labels = c("Chick-rearing", "Incubation")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1)+
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "gray"), 
        panel.grid = element_blank(),
        legend.key.size = unit(0.8, "cm"), axis.text.x=element_text(angle = 90, hjust = 0))+
  facet_grid(Colony_f~FE, order = FE_levels)+
  geom_text(data = BA_IC, mapping = aes(x = -Inf, y = -Inf, label = BA_text), fontface=2, hjust = -0.1, vjust = -0.5, cex = 2.8)


png("Figures/Tracks_intracol.png",width = 21, height = 15, units = "cm", res = 300)
print(trackmap_intracol)
dev.off()


RFB_trips_year <- RFB_trips %>%
  dplyr::select(Colony_f, Longitude, Latitude, TripID, Monsoon, Year) %>%
  mutate(FE = factor("Monsoon", levels = FE_levels),
         Colony_f = factor(Colony_f, levels = c("BP", "EI", "DI", "NI")))

levels(RFB_trips_year$Colony_f) <- c("DG - BP", "DG - EI", "DI", "NI")

trackmap_year <- ggplot() + 
  scale_x_continuous(expand = c(0.05,0.05))+
  scale_y_continuous(expand = c(0.05,0.05))+
  geom_sf(data = filter(chagos, DEPTHLABEL == "land"), inherit.aes = FALSE, fill = NA, col = "grey30", lwd = 0.6)+
  geom_path(data = RFB_trips_year, aes(x = Longitude, y = Latitude, group = TripID, colour = Monsoon), alpha = 0.6) + 
  scale_colour_manual(values = monsoon_values, name = "Monsoon season") + 
  new_scale_color() +
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1)+
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "gray"), 
        panel.grid = element_blank(),
        legend.key.size = unit(0.8, "cm"), axis.text.x=element_text(angle = 90, hjust = 0))+
  facet_grid(Colony_f~Year)


png("Figures/Tracks_year.png",width = 21, height = 15, units = "cm", res = 300)
print(trackmap_year)
dev.off()


# DG map ####
BP <- colonies %>%
  filter(Colony == "BP") %>%
  dplyr::select(Long, Lat)

EI <- colonies %>%
  filter(Colony == "EI") %>%
  dplyr::select(Long, Lat)

library(ggspatial)

CA_plot <- ggplot() + 
  geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("variable")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("shallow")), inherit.aes = FALSE, fill = "gray50", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("land")), inherit.aes = FALSE, fill = "grey30", col = "grey20", lwd = 0.6)+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 2, col = "magenta")+
  geom_text(aes(x = 72, y=-6.32), label = "Great Chagos\nBank", cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 72.32, y=-7.35), label = "Diego\nGarcia\natoll", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.2, y=-6.45), label = "Danger\nIsland", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 72.38, y=-5.67), label = "Nelson's\nIsland", hjust = 0, cex = 2.9, lineheight = 1)+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))+
  annotation_scale(style = "ticks")+
  ylab(NULL)+
  xlab(NULL)
CA_plot

DG_plot <- ggplot() + 
  scale_x_continuous(limits = c(BP$Long-0.1, BP$Long+0.1), expand = c(0,0))+
  scale_y_continuous(limits = c(-7.47, -7.21), expand = c(0,0))+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("variable")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("shallow")), inherit.aes = FALSE, fill = "gray50", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("land")), inherit.aes = FALSE, fill = "grey30", col = "grey20", lwd = 0.6)+
  geom_rect(aes(xmin = (EI$Long - 0.005), xmax = (EI$Long + 0.006), ymin = (EI$Lat - 0.004), ymax = (EI$Lat + 0.003)), inherit.aes = F, col = col_values[2], fill = NA)+
  annotate("text", label = "East Island", x = (EI$Long - 0.02), y = (EI$Lat + 0.01), col = col_values[2])+
  geom_rect(aes(xmin = (BP$Long - 0.005), xmax = (BP$Long + 0.03), ymin = (BP$Lat - 0.03), ymax = (BP$Lat + 0.003)), inherit.aes = F, col = col_values[1], fill = NA)+
  annotate("text", label = "Barton\nPoint", x = (BP$Long + 0.032), y = (BP$Lat-0.01), col = col_values[1], hjust = 0)+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))+
  annotation_scale(style = "ticks")+
  ylab(NULL)+
  xlab(NULL)

png("Figures/Supp_DGmap.png",width = 16, height = 10, units = "cm", res = 300)
print(CA_plot + DG_plot + plot_annotation(tag_levels = "A"))
dev.off()

##### kernel overlap #####

##### re run kernels as one object for BA analyses
unique(RFB_trips$Colony_f)
RFBsp <- RFB_trips[,c("Longitude", "Latitude", "Colony_f")]
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
RFBsp$id <- RFB_trips[,c("Colony_f")]

RFB_UD <- kernelUD(RFBsp, kern = "bivnorm", grid = xy, same4all = F)

BA <- kerneloverlaphr(RFB_UD, method = c("BA"))
write.csv(BA, "Data/kernels/BAvalues.csv", row.names = T)

##### Repeat above for 2019: tracking data from all colonies ####
RFB_2019 <- RFB_trips %>%
  filter(Year == "2019")
n_distinct(RFB_2019$BirdID)
RFB_2019 %>%
  group_by(Colony_f) %>%
  summarise(n = n_distinct(BirdID))

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

sf_col_ll_poly.o_2019 <- sf_col_ll_poly_2019[order(-sf_col_ll_poly_2019$Group.2),]
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
                     labels = c("Diego Garcia - Barton Point", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia - Barton Point", "Danger Island", "Nelson's Island"),
                    guide="none") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o_2019$Group.2)),"% UD",range = c(0.1, 1))+
  #guides(fill = guide_legend(override.aes = list(alpha = 1)))+
  #geom_point(data = RFBcols, aes(x = lon, y = lat), inherit.aes = FALSE, cex = 0.5, col = "gray40")+
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())
map_kernel_2019


### adjust track plot to combine 
trackmap2019 <- ggplot() + 
  geom_point(data = RFBcols_group, aes(x = lon, y = lat, cex = no.RFB), inherit.aes = FALSE, col = "gray40")+
  scale_size_continuous("Colony size")+
  geom_sf(data = sf_col_ll_poly.o_2019, aes(col = Group.3, fill = Group.3, alpha = Group.2), lwd = 0.4)+
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia - Barton Point", "Danger Island", "Nelson's Island"),
                     guide="none") +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                    labels = c("Diego Garcia - Barton Point", "Danger Island", "Nelson's Island"),
                    guide="none") +
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y), fill = "white") +
  scale_alpha_discrete(limits = rev(levels(sf_col_ll_poly.o_2019$Group.2)),"% UD",range = c(0.1, 1))+
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
  geom_path(data = RFB_2019, aes(x = Longitude, y = Latitude, group = TripID, colour = Colony_f), alpha = 0.75) + 
  # scale_color_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony",
                     labels = c("Diego Garcia - Barton Point", "Danger Island", "Nelson's Island")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1),order = 1),
         alpha = guide_legend(order = 2),
         linetype = guide_legend(order = 3),
         size = guide_legend(order = 4))+
  geom_point(data = colonies, aes(x = Long, y = Lat), inherit.aes = FALSE, cex = 1.1)+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"),
        axis.title = element_blank())

trackmap2019

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

angles.plot$distance <- as.factor(angles.plot$distance)
levels(angles.plot$distance)
angles.plot$distance <- fct_relevel(angles.plot$distance, "1km", "5km", "10km", "25km", "max dist")


supp.angle.plot <- ggplot(angles.plot, aes(x = theta, group = Colony_f, fill = Colony_f)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks=seq(0, 330, by=30), expand=c(0,0), lim=c(0, 360),
                     labels = c("90", " ", " ", "0/360", " ", " ", "270", " ", " ", "180", " ", " ")) +
  coord_polar(start = -1.57, direction=-1) +
  #scale_size() +
  theme_minimal() + 
  ylim(0, 18) +
  xlab(NULL)+
  scale_fill_manual(values = col_values, "Colony", labels = col_labs) + 
  facet_grid(Colony_f~distance) +
  theme(panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank(),
        legend.position = "bottom", axis.text.x = element_text(size = 7))
supp.angle.plot

png("Figures/Supp_Distance_depAngle.png",width = 22, height = 18, units = "cm", res = 300)
print(supp.angle.plot)
dev.off()


# BA Overlap : intra-colony effects #####

# 1. Monsoon ####

Intracol_Monsoon <- RFB_trips %>%
  dplyr::select(Longitude, Latitude, Colony_f, Monsoon) %>%
  mutate(col_monsoon = paste0(Colony_f, "_", Monsoon))

IC_Monsoon_sp <- Intracol_Monsoon %>%
  dplyr::select(Longitude, Latitude, col_monsoon)
colnames(IC_Monsoon_sp)[3] <- "id"
coordinates(IC_Monsoon_sp) <- c("Longitude", "Latitude")
IC_Monsoon_sf <- st_as_sfc(IC_Monsoon_sp)
st_crs(IC_Monsoon_sf) <- 4326
IC_Monsoon_laea <- st_transform(IC_Monsoon_sf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(IC_Monsoon_laea)
x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

IC_Monsoon_sp <- as(IC_Monsoon_laea, "Spatial")
IC_Monsoon_sp$id <- Intracol_Monsoon$col_monsoon

IC_Monsoon_UD <- kernelUD(IC_Monsoon_sp, kern = "bivnorm", grid = xy, same4all = F)

BA_IC_Monsoon <- kerneloverlaphr(IC_Monsoon_UD, method = c("BA")) 
BA_IC_Monsoon_long <- BA_IC_Monsoon %>%
  as_tibble(rownames = "col_Monsoon1") %>%
  gather(., key="col_Monsoon2", value = "BA", -col_Monsoon1) %>%
  # remove rows where comparison between matching UDs 
  filter(!col_Monsoon1 == col_Monsoon2) %>%
  separate(., col_Monsoon1, into = c("col1", "Monsoon1")) %>%
  separate(., col_Monsoon2, into = c("col2", "Monsoon2")) %>%
  # keep pairwise comparisons only
  filter(col1 == col2) %>%
  # keep one of pairwise comparisons
  distinct(., col1, .keep_all = T) %>%
  rename(Colony_f = col1) %>%
  mutate(FE = "Monsoon") %>%
  dplyr::select(Colony_f, FE, BA)
BA_IC_Monsoon_long


# 2. Sex ####

Intracol_Sex <- RFB_trips %>%
  filter(Sex %in% c("F", "M")) %>%
  dplyr::select(Longitude, Latitude, Colony_f, Sex) %>%
  mutate(col_Sex = paste0(Colony_f, "_", Sex))

IC_Sex_sp <- Intracol_Sex %>%
  dplyr::select(Longitude, Latitude, col_Sex)
colnames(IC_Sex_sp)[3] <- "id"
coordinates(IC_Sex_sp) <- c("Longitude", "Latitude")
IC_Sex_sf <- st_as_sfc(IC_Sex_sp)
st_crs(IC_Sex_sf) <- 4326
IC_Sex_laea <- st_transform(IC_Sex_sf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(IC_Sex_laea)
x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

IC_Sex_sp <- as(IC_Sex_laea, "Spatial")
IC_Sex_sp$id <- Intracol_Sex$col_Sex

IC_Sex_UD <- kernelUD(IC_Sex_sp, kern = "bivnorm", grid = xy, same4all = F)

BA_IC_Sex <- kerneloverlaphr(IC_Sex_UD, method = c("BA")) 
BA_IC_Sex_long <- BA_IC_Sex %>%
  as_tibble(rownames = "col_Sex1") %>%
  gather(., key="col_Sex2", value = "BA", -col_Sex1) %>%
  # remove rows where comparison between matching UDs 
  filter(!col_Sex1 == col_Sex2) %>%
  separate(., col_Sex1, into = c("col1", "Sex1")) %>%
  separate(., col_Sex2, into = c("col2", "Sex2")) %>%
  # keep pairwise comparisons only
  filter(col1 == col2) %>%
  # keep one of pairwise comparisons
  distinct(., col1, .keep_all = T) %>%
  rename(Colony_f = col1) %>%
  mutate(FE = "Sex") %>%
  dplyr::select(Colony_f, FE, BA)
BA_IC_Sex_long


# 3. Breed_Stage ####

Intracol_Breed_Stage <- RFB_trips %>%
  filter(Breed_Stage %in% c("chick-rearing", "incubation")) %>%
  dplyr::select(Longitude, Latitude, Colony_f, Breed_Stage) %>%
  mutate(col_Breed_Stage = paste0(Colony_f, "_", Breed_Stage))

IC_Breed_Stage_sp <- Intracol_Breed_Stage %>%
  dplyr::select(Longitude, Latitude, col_Breed_Stage)
colnames(IC_Breed_Stage_sp)[3] <- "id"
coordinates(IC_Breed_Stage_sp) <- c("Longitude", "Latitude")
IC_Breed_Stage_sf <- st_as_sfc(IC_Breed_Stage_sp)
st_crs(IC_Breed_Stage_sf) <- 4326
IC_Breed_Stage_laea <- st_transform(IC_Breed_Stage_sf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(IC_Breed_Stage_laea)
x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

IC_Breed_Stage_sp <- as(IC_Breed_Stage_laea, "Spatial")
IC_Breed_Stage_sp$id <- Intracol_Breed_Stage$col_Breed_Stage

IC_Breed_Stage_UD <- kernelUD(IC_Breed_Stage_sp, kern = "bivnorm", grid = xy, same4all = F)

BA_IC_Breed_Stage <- kerneloverlaphr(IC_Breed_Stage_UD, method = c("BA")) 
BA_IC_Breed_Stage_long <- BA_IC_Breed_Stage %>%
  as_tibble(rownames = "col_Breed_Stage1") %>%
  gather(., key="col_Breed_Stage2", value = "BA", -col_Breed_Stage1) %>%
  # remove rows where comparison between matching UDs 
  filter(!col_Breed_Stage1 == col_Breed_Stage2) %>%
  separate(., col_Breed_Stage1, into = c("col1", "Breed_Stage1"), sep = "_") %>%
  separate(., col_Breed_Stage2, into = c("col2", "Breed_Stage2"), sep = "_") %>%
  # keep pairwise comparisons only
  filter(col1 == col2) %>%
  # keep one of pairwise comparisons
  distinct(., col1, .keep_all = T) %>%
  rename(Colony_f = col1) %>%
  mutate(FE = "Breeding Stage") %>%
  dplyr::select(Colony_f, FE, BA)
BA_IC_Breed_Stage_long




# 4. trip_length_days ####

Intracol_trip_length_days <- RFB_trips_tripmetrics %>%
  dplyr::select(Longitude, Latitude, Colony_f, trip_length_days) %>%
  mutate(col_trip_length_days = paste0(Colony_f, "_", trip_length_days))

IC_trip_length_days_sp <- Intracol_trip_length_days %>%
  dplyr::select(Longitude, Latitude, col_trip_length_days)
colnames(IC_trip_length_days_sp)[3] <- "id"
coordinates(IC_trip_length_days_sp) <- c("Longitude", "Latitude")
IC_trip_length_days_sf <- st_as_sfc(IC_trip_length_days_sp)
st_crs(IC_trip_length_days_sf) <- 4326
IC_trip_length_days_laea <- st_transform(IC_trip_length_days_sf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(IC_trip_length_days_laea)
x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

IC_trip_length_days_sp <- as(IC_trip_length_days_laea, "Spatial")
IC_trip_length_days_sp$id <- Intracol_trip_length_days$col_trip_length_days

IC_trip_length_days_UD <- kernelUD(IC_trip_length_days_sp, kern = "bivnorm", grid = xy, same4all = F)

BA_IC_trip_length_days <- kerneloverlaphr(IC_trip_length_days_UD, method = c("BA")) 
BA_IC_trip_length_days_long <- BA_IC_trip_length_days %>%
  as_tibble(rownames = "col_trip_length_days1") %>%
  gather(., key="col_trip_length_days2", value = "BA", -col_trip_length_days1) %>%
  # remove rows where comparison between matching UDs 
  filter(!col_trip_length_days1 == col_trip_length_days2) %>%
  separate(., col_trip_length_days1, into = c("col1", "trip_length_days1"), sep = "_") %>%
  separate(., col_trip_length_days2, into = c("col2", "trip_length_days2"), sep = "_") %>%
  # keep pairwise comparisons only
  filter(col1 == col2) %>%
  # keep one of pairwise comparisons
  distinct(., col1, .keep_all = T) %>%
  rename(Colony_f = col1) %>%
  mutate(FE = "Trip length") %>%
  dplyr::select(Colony_f, FE, BA) %>%
  mutate(Colony_f = case_when(Colony_f== "DG - BP" ~ "BP", Colony_f== "DG - EI" ~ "EI", .default = Colony_f))
BA_IC_trip_length_days_long

BA_IC <- bind_rows(BA_IC_Monsoon_long, BA_IC_Sex_long, BA_IC_Breed_Stage_long, BA_IC_trip_length_days_long)

write_csv(BA_IC, "Data/kernels/BAvalues_Intracol.csv")



###### test hexagon grid ####
library(raster)

min(RFB_trips$Longitude)
max(RFB_trips$Longitude)
min(RFB_trips$Latitude)
max(RFB_trips$Latitude)


RFBsf <- st_as_sf(RFB_trips, coords=c("Longitude","Latitude"), crs=4326)
RFBlaea <- st_transform(RFBsf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 
hgrid <- st_make_grid(RFBlaea, cellsize = 20000, square = FALSE) %>% # cellsize in m, i.e., 10000 = 10km
  st_transform(crs=4326) %>%
  st_as_sf() %>%
  mutate(hID = c(1:NROW(.)))


ggplot()+
  geom_sf(data=hgrid)+
  geom_sf(data=RFBsf, col = "blue")
  


chagos_bathy <- getNOAA.bathy(lon1 = 66, lon2 = 78,
                              lat1 = -1.5, lat2 = -11.6, resolution = 1)
chagos_bathy_sf <- fortify(chagos_bathy) %>%
  st_as_sf(., coords=c("x","y"), crs=4326)
head(chagos_bathy_sf)

hgrid_bathy <- st_join(hgrid, chagos_bathy_sf) %>% 
  group_by(hID) %>% 
  summarise(mean_bathy = mean(z))

chagos_bathy_raster <- marmap::as.raster(chagos_bathy)
slope <- terrain(chagos_bathy_raster, opt="slope", unit="degrees", neighbours="8") %>%
  raster::as.data.frame(., xy=TRUE) %>%
  st_as_sf(., coords=c("x","y"), crs=4326)

hgrid_slope <- st_join(hgrid, slope) %>% 
  group_by(hID) %>% 
  summarise(mean_slope = mean(slope))

head(hgrid_bathy_slope)

