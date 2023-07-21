library(sjPlot)
library(patchwork)

## Potential spatial difference between Diego Garcia sub-colonies

DG <- read_csv("Data/RFB_2016-2023.csv") %>%
  filter(Colony == "DG") %>%
  mutate(Sub_colony = case_when(Year == "2022" ~ "East Island", .default = "Barton Point"))
head(DG)
str(DG)
unique(DG$Sub_colony)

# read in map layers 
chagos_bathy <- getNOAA.bathy(lon1 = 66, lon2 = 78,
                              lat1 = -1.5, lat2 = -11.6, resolution = 1)
chagos_bathy_fort = fortify(chagos_bathy)
head(chagos_bathy_fort)

MPA <- read.csv("/Users/at687/Documents/BIOT/Non-breeding/Data/Map Indian Ocean/MPA.csv", as.is = T)
MPA_shp = st_read("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/ChagosEEZ.shp")

subcolonies <- tibble(
  "Sub_colony" = c("Barton Point", "East Island"),
  "Lon" = c(72.4373, 72.4197),
  "Lat" = c(-7.2344, -7.2268)
)

# map tracks ####
DG_tracks_plot <- ggplot() + 
  geom_raster(data = chagos_bathy_fort, aes(x=x, y=y, fill=z), alpha = 0.9) +
  scale_x_continuous(limits = c(70, 76.4), expand = c(0,0))+
  scale_y_continuous(limits = c(-10.5, -4.1), expand = c(0,0))+
  geom_sf(data = MPA_shp, inherit.aes = FALSE, col = "grey90", lwd = 0.6, fill = NA)+
  scale_fill_viridis_c(option="mako", "Depth (m)") + 
  new_scale_color() +
  geom_path(data = DG, aes(x = Longitude, y = Latitude, group = TripID, colour = Sub_colony), alpha = 0.6) + 
  scale_colour_viridis_d(option = "plasma", begin = 0.95, end = 0.6)+
  geom_point(data = subcolonies, aes(x = Lon, y = Lat), inherit.aes = FALSE, cex = 1)+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"))+
  labs(x = " ", y = "Latitude (°N)")

# kernel overlap ####
DGsp <- DG[,c("Longitude", "Latitude", "Sub_colony")]
colnames(DGsp)[3] <- "id"
coordinates(DGsp) <- c("Longitude", "Latitude")
DGsf <- st_as_sfc(DGsp)
st_crs(DGsf) <- 4326
DGlaea <- st_transform(DGsf, CRS("+proj=laea +lat_0=-7 +lon_0=72 +units=m")) 

bbox <- st_bbox(DGlaea)

x <- seq(as.numeric(bbox["xmin"])-35000,as.numeric(bbox["xmax"])+35000,by=1000) # by = 1km grid
y <- seq(as.numeric(bbox["ymin"])-35000,as.numeric(bbox["ymax"])+35000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

DGsp <- as(DGlaea, "Spatial")
DGsp$id <- DG[,c("Sub_colony")]

DG_UD <- kernelUD(DGsp, kern = "bivnorm", grid = xy, same4all = F)

BA <- kerneloverlaphr(DG_UD, method = c("BA"))

# Trip metrics ####


DG_tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  filter(Colony == "DG") %>%
  mutate(Sub_colony = case_when(Year == "2022" ~ "East Island", .default = "Barton Point"))


# subset to complete trips 
DG_complete <- DG_tripmetrics %>%
  filter(Completetrip == "Complete") %>%
  mutate(year_f = as.factor(Year),
         log_duration = log10(Trip_duration),
         log_totdist = log10(Total_distance),
         log_maxdist = log10(Max_distance))


m.DG.logduration <- lmer(log_duration ~ Sub_colony + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logduration))
qqline(resid(m.DG.logduration))

summary(m.DG.logduration)
d.DG.logduration <- dredge(m.DG.logduration)
d.DG.logduration


param.duration <- as.data.frame(sjPlot::get_model_data(m.DG.logduration, type = "pred", plot = F)$Sub_colony)
param.duration$trip.metric <- "Trip Duration"
param.duration$Sub_colony <- c("Barton Point", "East Island")


m.DG.logtotdist <- lmer(log_totdist ~ Sub_colony + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logtotdist))
qqline(resid(m.DG.logtotdist))

summary(m.DG.logtotdist)
d.DG.logtotdist <- dredge(m.DG.logtotdist)
d.DG.logtotdist

param.totdist <- as.data.frame(sjPlot::get_model_data(m.DG.logtotdist, type = "pred", plot = F)$Sub_colony)
param.totdist$trip.metric <- "Total Distance"
param.totdist$Sub_colony <- c("Barton Point", "East Island")


m.DG.logmaxdist <- lmer(log_maxdist ~ Sub_colony + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logmaxdist))
qqline(resid(m.DG.logmaxdist))

summary(m.DG.logmaxdist)
d.DG.logmaxdist <- dredge(m.DG.logmaxdist)
d.DG.logmaxdist


param.maxdist <- as.data.frame(sjPlot::get_model_data(m.DG.logmaxdist, type = "pred", plot = F)$Sub_colony)
param.maxdist$trip.metric <- "Max Distance"
param.maxdist$Sub_colony <- c("Barton Point", "East Island")


model.estimates <- rbind(param.duration, param.maxdist, param.totdist)
model.estimates$trip.metricLong <- as.factor(model.estimates$trip.metric)

trips.long <- gather(DG_tripmetrics, key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance))
head(trips.long)

trips.long$trip.metricLong <- as.factor(trips.long$trip.metric)
levels(trips.long$trip.metricLong)
levels(trips.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

DG_trips_plot <- ggplot(trips.long, aes(x = Sub_colony, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = Sub_colony), alpha = 0.4, position = position_jitter())+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony") + 
  scale_colour_viridis_d(option = "plasma", begin = 0.95, end = 0.6, guide = "none")+
  scale_y_log10(n.breaks = 7,)+
  #geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  geom_errorbar(data = model.estimates, aes(x = Sub_colony, ymin = 10^(conf.low), ymax = 10^(conf.high)), inherit.aes = F, width = 0.3)+
  geom_point(data = model.estimates, aes(x = Sub_colony, y = 10^(predicted)), inherit.aes = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Sub Colony", y = "Value (Distance in km; Duration in hours)")



DG_subcol <- DG_tracks_plot + DG_trips_plot + 
  plot_annotation(tag_levels = 'a') +
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

png("Figures/Supp_DG_subcol.png",width = 22, height = 14, units = "cm", res = 300)
print(DG_subcol)
dev.off()

DG_unknownsex <- DG %>% 
  group_by(BirdID) %>%
  filter(Sex == "unknown") %>%
  distinct(BirdID, Year)
write_csv(DG_unknownsex, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/DG_unknownsex.csv")
