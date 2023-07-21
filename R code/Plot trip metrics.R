library(gridExtra)
library(RColorBrewer)
library(png)
library(tidyverse)
library(cowplot)
library(lubridate)
library(patchwork)

RFBimg <- "/Users/at687/Documents/BIOT/Seabird graphics/white morph red footed booby adult.png"
col_values <- brewer.pal(9, "RdPu")[c(8,6,4,2)]
sex_values <- brewer.pal(7, "Greens")[c(6,3)]
triplength_values <- brewer.pal(8, "Blues")[c(7,4)]


### read in trip metrics datasets ####

RFB_tripmetrics_sun_complete <- read_csv("Data/RFB_2016-2023_tripmetrics_sun_complete.csv") %>%
  mutate(Colony_f = factor(colony_sub, levels = c("BP", "EI", "DI", "NI")))

RFB_tripmetrics_sun_complete$ColonyLong <- as.factor(RFB_tripmetrics_sun_complete$Colony_f)
levels(RFB_tripmetrics_sun_complete$ColonyLong)
levels(RFB_tripmetrics_sun_complete$ColonyLong) <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island")


## plot trip duration ####
tripdur <- ggplot(RFB_tripmetrics_sun_complete, aes(x = Trip_duration, fill = ColonyLong))+
  geom_histogram(bins = 40)+
  # scale_fill_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony", guide = "none",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) +
  scale_fill_manual(values = col_values, guide = "none")+
  geom_vline(xintercept = c(24,48,72,96,120), lty = "dashed")+
  scale_x_continuous(limits = c(0,120), breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120))+
  labs(x = "Trip Duration (hours)", y = "Frequency")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
tripdur

# plot time of departures/arrivals ####

mean.dawn <- mean(hour(ymd_hms(RFB_tripmetrics_sun_complete$Dep_nauticalDawn)) + minute(ymd_hms(RFB_tripmetrics_sun_complete$Dep_nauticalDawn))/60)
mean.dusk <- mean(hour(ymd_hms(RFB_tripmetrics_sun_complete$Arr_nauticalDusk)) + minute(ymd_hms(RFB_tripmetrics_sun_complete$Arr_nauticalDusk))/60)

# read in model estimates data

mod.ests.times <- read.csv("Data/mod_estimates_triptimes.csv", as.is = T)

mod.ests.times$group <- factor(mod.ests.times$x, levels = c("BP", "EI", "DI", "NI", "F", "M", "unknown", "Single day", "Multi-day"))
levels(mod.ests.times$group) <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island", "Female", "Male", "Unknown", "Single day", "Multi-day")
mod.ests.times$TripSegment <- "Dep"
mod.ests.times$pred_rel <- mod.ests.times$predicted + mean.dawn
mod.ests.times$conf.low_rel <- mod.ests.times$conf.low + mean.dawn
mod.ests.times$conf.high_rel <- mod.ests.times$conf.high + mean.dawn
mod.ests.times$TripSegment <- as.factor(mod.ests.times$TripSegment)
levels(mod.ests.times$TripSegment)
levels(mod.ests.times$TripSegment) <- c("Departure")

# time of day ~ col ####

trips_plottimes_col <- RFB_tripmetrics_sun_complete %>%
  select(Colony_f, TripID, Dep_Timelocal, Arr_Timelocal) %>%
  arrange(factor(Colony_f, levels = c("BP", "EI", "DI", "NI")), Dep_Timelocal) %>%
  mutate(triporder = 1:n()) %>%
  pivot_longer(Dep_Timelocal:Arr_Timelocal, names_to = c("TripSegment", ".value"), names_sep = "_")

trips_plottimes_col$ColonyLong <- as.factor(trips_plottimes_col$Colony_f)
levels(trips_plottimes_col$ColonyLong)
levels(trips_plottimes_col$ColonyLong) <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island")
trips_plottimes_col$TripSegment <- as.factor(trips_plottimes_col$TripSegment)
levels(trips_plottimes_col$TripSegment)
levels(trips_plottimes_col$TripSegment) <- c("Arrival", "Departure")
trips_plottimes_col$TripSegment <- fct_relevel(trips_plottimes_col$TripSegment, "Departure", "Arrival")

mean.y <- trips_plottimes_col %>% dplyr::group_by(Colony_f) %>% dplyr::summarise(av.y = mean(triporder))
mod.ests.times_col <- mod.ests.times %>%
  filter(FE == "Colony") %>%
  mutate(ColonyLong = c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island")) %>%
  left_join(., mean.y, by = c("x" = "Colony_f")) %>%
  rename(Colony_f = x)

head(mod.ests.times_col)
head(trips_plottimes_col)

plot_time_col <- ggplot(trips_plottimes_col, aes(x = Timelocal, y = triporder, col =  ColonyLong))+
  facet_grid(factor(Colony_f, levels = c("BP", "EI", "DI", "NI")) ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(y = "Trip")+
  xlab(NULL)+
  theme_minimal()+
  scale_color_manual(values = col_values, name = "Colony")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_y_continuous(expand = c(0.1,0.1))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.times_col, aes(y = av.y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.times_col, aes(y = av.y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time_col


# time of day ~ sex ####


trips_plottimes_sex <- RFB_tripmetrics_sun_complete %>%
  select(Sex, TripID, Dep_Timelocal) %>%
  filter(Sex %in% c("F", "M")) %>%
  arrange(Sex, Dep_Timelocal) %>%
  mutate(triporder = 1:n())

trips_plottimes_sex$Sex <- as.factor(trips_plottimes_sex$Sex)
levels(trips_plottimes_sex$Sex)
levels(trips_plottimes_sex$Sex) <- c("Female", "Male")
trips_plottimes_sex$TripSegment <- "Departure"

mean.y <- trips_plottimes_sex %>% dplyr::group_by(Sex) %>% dplyr::summarise(av.y = mean(triporder))
mod.ests.times_sex <- mod.ests.times %>%
  filter(FE == "Sex" & x %in% c("F", "M")) %>%
  mutate(Sex = case_when(x == "F" ~ "Female", x == "M" ~ "Male")) %>%
  left_join(., mean.y, by = c("Sex" = "Sex"))

head(mod.ests.times_sex)
head(trips_plottimes_sex)

plot_time_sex <- ggplot(trips_plottimes_sex, aes(x = Dep_Timelocal, y = triporder, col =  Sex))+
  facet_grid(Sex ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(y = "Trip")+
  xlab(NULL)+
  theme_minimal()+
  scale_color_manual(values = sex_values)+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_y_continuous(expand = c(0.05,0.05))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.times_sex, aes(y = av.y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.times_sex, aes(y = av.y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time_sex





# time of day ~ trip length days ####


trips_plottimes_triplength <- RFB_tripmetrics_sun_complete %>%
  select(Trip_length_days, TripID, Dep_Timelocal) %>%
  arrange(Trip_length_days, Dep_Timelocal) %>%
  mutate(triporder = 1:n(),
         trip_short = case_when(Trip_length_days == "Single day" ~ "Single", .default = "Multi"))

trips_plottimes_triplength$Trip_length_days <- factor(trips_plottimes_triplength$Trip_length_days, levels = c("Single day", "Multi-day"))
trips_plottimes_triplength$trip_short <- factor(trips_plottimes_triplength$trip_short, levels = c("Single", "Multi"))
trips_plottimes_triplength$TripSegment <- "Departure"

mean.y <- trips_plottimes_triplength %>% dplyr::group_by(Trip_length_days) %>% dplyr::summarise(av.y = mean(triporder))
mod.ests.times_triplength <- mod.ests.times %>%
  filter(FE == "Trip_length") %>%
  left_join(., mean.y, by = c("x" = "Trip_length_days")) %>%
  rename(Trip_length_days = group) %>%
  mutate(trip_short = case_when(x == "Single day" ~ "Single", .default = "Multi"))
mod.ests.times_triplength$trip_short <- factor(mod.ests.times_triplength$trip_short, levels = c("Single", "Multi"))

head(mod.ests.times_triplength)
head(trips_plottimes_triplength)


plot_time_triplength <- ggplot(trips_plottimes_triplength, aes(x = Dep_Timelocal, y = triporder, col =  Trip_length_days))+
  facet_grid(trip_short ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(y = "Trip")+
  xlab(NULL)+
  theme_minimal()+
  scale_color_manual(values = triplength_values, name = "Trip length")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_y_continuous(expand = c(0.05,0.05))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.times_triplength, aes(y = av.y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.times_triplength, aes(y = av.y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time_triplength




### plot trip metrics #####


head(RFB_tripmetrics_sun_complete)

trips.long_col <- RFB_tripmetrics_sun_complete %>%
  select(Colony_f, BirdID, Trip_duration, Total_distance, Max_distance) %>%
  gather(key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance)) %>%
  rename(group = Colony_f) %>%
  mutate(FE = "Colony")
head(trips.long_col)

trips.long_sex <- RFB_tripmetrics_sun_complete %>%
  select(Sex, BirdID, Trip_duration, Total_distance, Max_distance) %>%
  filter(Sex %in% c("F", "M")) %>%
  gather(key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance)) %>%
  rename(group = Sex) %>%
  mutate(FE = "Sex")
head(trips.long_sex)
unique(trips.long_sex$group)

trips.long <- bind_rows(trips.long_col, trips.long_sex)
trips.long$group <- factor(trips.long$group, levels = c("BP", "EI", "DI", "NI", "F", "M"))
levels(trips.long$group) <- c("DG - BP", "DG - EI", "DI", "NI", "F", "M")

trips.long$trip.metricLong <- as.factor(trips.long$trip.metric)
levels(trips.long$trip.metricLong)
levels(trips.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

model.estimates <- read_csv("Data/mod_estimates_tripmetrics.csv") %>%
  filter(x %in% c("BP", "EI", "DI", "NI", "F", "M"))
model.estimates$trip.metricLong <- as.factor(model.estimates$TripMetric)
levels(model.estimates$trip.metricLong)
levels(model.estimates$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )
model.estimates$group <- factor(model.estimates$x, levels = c("BP", "EI", "DI", "NI", "F", "M"))
levels(model.estimates$group) <- c("DG - BP", "DG - EI", "DI", "NI", "F", "M")

cols <- c(col_values, sex_values)

trip_metrics <- ggplot(trips.long, aes(x = group, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = group), alpha = 0.4, position = position_jitter(width = 0.2))+
  scale_color_manual(values = cols)+
  scale_y_log10()+
  guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  #geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  geom_errorbar(data = model.estimates, aes(x = group, ymin = conf.low_invlog, ymax = conf.high_invlog), inherit.aes = F, width = 0.3)+
  geom_point(data = model.estimates, aes(x = group, y = predicted_invlog), inherit.aes = F)+
  geom_vline(xintercept = c(4.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Colony, Sex", y = "Value (Distance in km; Duration in hours)")
trip_metrics

### combine plots ####




legend.Col <- cowplot::get_legend(
  plot_time_col +theme(legend.margin=margin(30,0,0,20))
)

legend.Sex <- cowplot::get_legend(
  plot_time_sex +theme(legend.margin=margin(60,0,0,50))
)


legend.triplength <- cowplot::get_legend(
  plot_time_triplength +theme(legend.margin=margin(60,0,0,10))
)


legends <- cowplot::plot_grid(legend.Col, legend.Sex, legend.triplength, nrow =1, align = "h", axis = "b")


p1 <- tripdur
p2 <- trip_metrics+theme(legend.position = "none")+xlab(NULL)
p3 <- plot_time_col+theme(legend.position = "none")
p4 <- plot_time_sex+theme(legend.position = "none") | plot_time_triplength+theme(legend.position = "none")+ylab(NULL)
p5 <- legends

design <- "
 11133
 11133
 11133
 11133
 11133
 22233
 22244
 22244
 22244
 22244
 222##
 22255
"

plotcombo <- p1 + p2 + p3 + p4 + p5 + 
  plot_layout(design = design) + 
  plot_annotation(tag_levels = list(c("a", "b", "c", "d", "e", " "))) & 
  theme(plot.tag.position = c(0, 0.95), plot.tag = element_text(size = 14, hjust = 0, vjust = 0, face = "bold"))
 

trip.plot.2 <- ggdraw() + 
  draw_plot(plotcombo) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0.12,
    width = 0.12
  )



png("Figures/TripMetricsComposite.png",width = 25, height = 20, units = "cm", res = 300)
print(trip.plot.2)
dev.off()




# Plot single day trip metrics #####

complete.singleday <- subset(ind.trips, complete == TRUE & Trip_length_days == "Single day")
complete.singleday$ColYear <- ifelse(complete.singleday$Colony == "DG", 
                                 paste0(complete.singleday$Colony, "_ALL"),
                                 paste0(complete.singleday$Colony, "_", complete.singleday$Year))



head(RFB_tripmetrics_sun_complete)

trips_single.long_col <- RFB_tripmetrics_sun_complete %>%
  filter(Trip_duration <24) %>%
  select(ColonyLong, BirdID, Trip_duration, Total_distance, Max_distance) %>%
  gather(key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance)) %>%
  rename(group = ColonyLong) %>%
  mutate(FE = "Colony")
head(trips_single.long_col)

trips_single.long_sex <- RFB_tripmetrics_sun_complete %>%
  filter(Trip_duration <24)  %>%
  gather(key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance)) %>%
  rename(group = Sex) %>%
  mutate(FE = "Sex")
head(trips_single.long_sex)
unique(trips_single.long_sex$group)

trips_single.long <- bind_rows(trips_single.long_col, trips_single.long_sex)
trips_single.long$group <- factor(trips_single.long$group, levels = c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island", "F", "M", "unknown"))
levels(trips_single.long$group) <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island", "Female", "Male", "Unknown")

trips_single.long$trip.metricLong <- as.factor(trips_single.long$trip.metric)
levels(trips_single.long$trip.metricLong)
levels(trips_single.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

model_single.estimates <- read_csv("Data/mod_estimates_tripmetrics_singleday.csv")
model_single.estimates$trip.metricLong <- as.factor(model_single.estimates$TripMetric)
levels(model_single.estimates$trip.metricLong)
levels(model_single.estimates$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )
model_single.estimates$group <- factor(model_single.estimates$x, levels = c("BP", "EI", "DI", "NI", "F", "M", "unknown"))
levels(model_single.estimates$group) <- c("Diego Garcia - Barton Point", "Diego Garcia - East Island", "Danger Island", "Nelson's Island", "Female", "Male", "Unknown")

cols <- c(col_values, brewer.pal(7, "Greens")[c(7,5,3)])

trip_metrics_single <- ggplot(trips_single.long, aes(x = group, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = group), alpha = 0.4, position = position_jitter(width = 0.2))+
  scale_color_manual(values = cols, name = "Colony, Sex")+
  scale_y_log10()+
  guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  #geom_errorbar(data = model_single.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  geom_errorbar(data = model_single.estimates, aes(x = group, ymin = conf.low_invlog, ymax = conf.high_invlog), inherit.aes = F, width = 0.3)+
  geom_point(data = model_single.estimates, aes(x = group, y = predicted_invlog), inherit.aes = F)+
  geom_vline(xintercept = c(4.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Colony, Sex", y = "Value (Distance in km; Duration in hours)")
trip_metrics_single

png("Figures/Supp_TripMetrics_Singleday.png",width = 18, height = 12, units = "cm", res = 300)
print(trip_metrics_single)
dev.off()


