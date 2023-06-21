library(gridExtra)
library(RColorBrewer)
library(png)
library(tidyverse)
library(cowplot)

RFBimg <- "/Users/at687/Documents/BIOT/Seabird graphics/white morph red footed booby adult.png"

### read in trip metrics datasets ####
ind.trips <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/RFB_tripMetrics_30mintrips.csv", as.is = T)
ind.trips$Year_fac = as.factor(ind.trips$Year)
ind.trips$Trip_length_days <- ifelse(ind.trips$TripDuration > 24, "Multi-day", "Single day")
head(ind.trips)

complete.trips <- subset(ind.trips, complete == TRUE)
complete.trips$ColYear <- ifelse(complete.trips$Colony == "DG", 
                                 paste0(complete.trips$Colony, "_ALL"),
                                 paste0(complete.trips$Colony, "_", complete.trips$Year))


ind.trips.timeh <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/RFB_tripMetrics_DepArrtimeh.csv")
ind.trips.timeh$ColonyLong <- as.factor(ind.trips.timeh$Colony)
levels(ind.trips.timeh$ColonyLong)
levels(ind.trips.timeh$ColonyLong) <- c("Diego Garcia", "Danger Island", "Nelson's Island")
ind.trips.timeh$TripSegment <- as.factor(ind.trips.timeh$TripSegment)
levels(ind.trips.timeh$TripSegment)
levels(ind.trips.timeh$TripSegment) <- c("Arrival", "Departure")
ind.trips.timeh$TripSegment <- fct_relevel(ind.trips.timeh$TripSegment, "Departure", "Arrival")
ind.trips.timeh$ColYrLong <- as.factor(ind.trips.timeh$ColYear)
levels(ind.trips.timeh$ColYrLong)
levels(ind.trips.timeh$ColYrLong) <- c("DG\nAll", "DI\n2019", "NI\n2018", "NI\n2019")

head(ind.trips.timeh)

## plot trip duration ####
tripdur <- ggplot(complete.trips, aes(x = TripDuration, fill = Colony))+
  geom_histogram(bins = 40)+
  # scale_fill_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony", guide = "none",
  #                    labels = c("Diego Garcia", "Danger Island", "Nelson's Island")) +
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], guide = "none")+
  geom_vline(xintercept = c(24,48,72,96,120), lty = "dashed")+
  scale_x_continuous(limits = c(0,120), breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120))+
  labs(x = "Trip Duration (hours)", y = "Frequency")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())


### plot time of day ~ Colony Year ####

mean.dawn <- 6.48745993589744
mean.dusk <- 20.0584134615385

# read in model estimates data

mod.ests.times <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/TripTimes_AllModelEstimates_Col.csv", as.is = T)
mod.ests.times$ColYrLong <- as.factor(mod.ests.times$ColYear)
levels(mod.ests.times$ColYrLong)
levels(mod.ests.times$ColYrLong) <- c("DG\nAll", "DI\n2019", "NI\n2018", "NI\n2019")
mean.y <- ind.trips.timeh %>% dplyr::group_by(ColYrLong) %>% dplyr::summarise(av.y = mean(triporder))
mod.ests.times$ColonyLong <- c("Diego Garcia", "Danger Island", "Nelson's Island", "Nelson's Island")
mod.ests.times$TripSegment <- as.factor(mod.ests.times$TripSegment)
levels(mod.ests.times$TripSegment)
levels(mod.ests.times$TripSegment) <- c("Arrival", "Departure")
mod.ests.times$TripSegment <- fct_relevel(mod.ests.times$TripSegment, "Departure", "Arrival")
mod.ests.times$y <- mean.y$av.y
mod.ests.times$pred_rel <- ifelse(mod.ests.times$TripSegment == "Arrival", 
                                  mod.ests.times$predicted + mean.dusk,
                                  mod.ests.times$predicted + mean.dawn)
mod.ests.times$conf.low_rel <- ifelse(mod.ests.times$TripSegment == "Arrival", 
                                      mod.ests.times$conf.low + mean.dusk,
                                      mod.ests.times$conf.low + mean.dawn)
mod.ests.times$conf.high_rel <- ifelse(mod.ests.times$TripSegment == "Arrival", 
                                       mod.ests.times$conf.high + mean.dusk,
                                       mod.ests.times$conf.high + mean.dawn)

head(mod.ests.times)



plot_time <- ggplot(ind.trips.timeh, aes(x = Time.h, y = triporder, col =  ColonyLong))+
  facet_grid(ColYrLong ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(x = "Time of Day", y = "Trip")+
  theme_minimal()+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony", guide = "none") + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], name = "Colony")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.times, aes(y = y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.times, aes(y = y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time

plot_time.circ <- ggplot(ind.trips.timeh, aes(x = Time.h, fill =  ColonyLong))+
  facet_grid(. ~ TripSegment)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_histogram(bins = 45)+
  labs(x = "Time of Day")+ylab(NULL)+
  theme_minimal()+
  scale_fill_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], guide = "none")+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0,0),  lim=c(0, 24),
                     labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00", "24:00"))+
  coord_polar()+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())



plot_time_col <- ggplot(ind.trips.timeh, aes(x = Time.h, y = triporder, col =  ColonyLong))+
  facet_grid(ColYrLong ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(y = "Trip")+
  xlab(NULL)+
  theme_minimal()+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony", guide = "none") + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)])+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.times, aes(y = y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.times, aes(y = y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time_col

### add single/multi-day to dep time plot ###

mod.ests.multi <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/TripTimes_AllModelEstimates_Multiday.csv")
colnames(mod.ests.multi)[6] <- "ColYear"

mod.ests.multi$ColYrLong <- as.factor(mod.ests.multi$ColYear)
levels(mod.ests.multi$ColYrLong)
levels(mod.ests.multi$ColYrLong) <- c("DG\nAll", "DI\n2019", "NI\n2018", "NI\n2019")

mean.y <- ind.trips.timeh %>% dplyr::group_by(ColYrLong) %>% dplyr::summarise(av.y = mean(triporder))

mod.ests.multi$ColonyLong <- c("Diego Garcia", "Danger Island", "Nelson's Island", "Nelson's Island")
mod.ests.multi$TripSegment <- as.factor(mod.ests.multi$TripSegment)
mod.ests.multi$y <- ifelse(mod.ests.multi$Trip_length_days == "Multi-day", 
                           mean.y$av.y - 10,
                           mean.y$av.y + 10)
mod.ests.multi$pred_rel <- ifelse(mod.ests.multi$TripSegment == "Arrival", 
                                  mod.ests.multi$predicted + mean.dusk,
                                  mod.ests.multi$predicted + mean.dawn)
mod.ests.multi$conf.low_rel <- ifelse(mod.ests.multi$TripSegment == "Arrival", 
                                      mod.ests.multi$conf.low + mean.dusk,
                                      mod.ests.multi$conf.low + mean.dawn)
mod.ests.multi$conf.high_rel <- ifelse(mod.ests.multi$TripSegment == "Arrival", 
                                       mod.ests.multi$conf.high + mean.dusk,
                                       mod.ests.multi$conf.high + mean.dawn)

head(mod.ests.multi)
head(mod.ests.times)

mod.ests.times$Trip_length_days <- "All"

mod.ests.comb <- bind_rows(subset(mod.ests.times, TripSegment == "Arrival"), mod.ests.multi)
head(mod.ests.comb)

head(ind.trips.timeh)
ind.trips.timeh$Trip_length_days <- ifelse(ind.trips.timeh$TripDuration > 24, "Multi-day", "Single day")

plot_time_col_multi <- ggplot(ind.trips.timeh, aes(x = Time.h, y = triporder, col =  ColonyLong))+
  facet_grid(ColYrLong ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(x = "Time of Day", y = "Trip")+
  theme_minimal()+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony", guide = "none") + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], guide = "none")+
  #guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.comb, aes(y = y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.comb, aes(y = y, x = pred_rel, pch = Trip_length_days), inherit.aes = F, cex = 2)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  scale_shape_manual(values=c(16, 17, 18), name = "Trip duration")+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"))
plot_time_col_multi 

### plot time of day ~ Monsoon ####
# read in model estimates data


mod.ests.monsoon <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/TripTimes_AllModelEstimates_Monsoon.csv", as.is = T)
mean.y.m <- ind.trips.timeh %>% dplyr::group_by(Monsoon) %>% dplyr::summarise(av.y = mean(triporder_M))
mod.ests.monsoon$y <- mean.y.m$av.y
mod.ests.monsoon$TripSegment <- as.factor(mod.ests.monsoon$TripSegment)
levels(mod.ests.monsoon$TripSegment)
levels(mod.ests.monsoon$TripSegment) <- c("Arrival", "Departure")
mod.ests.monsoon$TripSegment <- fct_relevel(mod.ests.monsoon$TripSegment, "Departure", "Arrival")
mod.ests.monsoon$pred_rel <- ifelse(mod.ests.monsoon$TripSegment == "Arrival", 
                                  mod.ests.monsoon$predicted + mean.dusk,
                                  mod.ests.monsoon$predicted + mean.dawn)
mod.ests.monsoon$conf.low_rel <- ifelse(mod.ests.monsoon$TripSegment == "Arrival", 
                                      mod.ests.monsoon$conf.low + mean.dusk,
                                      mod.ests.monsoon$conf.low + mean.dawn)
mod.ests.monsoon$conf.high_rel <- ifelse(mod.ests.monsoon$TripSegment == "Arrival", 
                                       mod.ests.monsoon$conf.high + mean.dusk,
                                       mod.ests.monsoon$conf.high + mean.dawn)

head(mod.ests.monsoon)


plot_time_monsoon <- ggplot(ind.trips.timeh, aes(x = Time.h, y = triporder_M, col =  Monsoon))+
  facet_grid(Monsoon ~ TripSegment,scales = "free_y", space = "free_y")+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = -Inf, xmax = mean.dawn, alpha = 0.2)+
  annotate("rect", ymin=-Inf,ymax=Inf,xmin = mean.dusk, xmax = Inf, alpha = 0.2)+
  geom_point(alpha = 0.4)+
  labs(x = "Time of Day", y = "Trip")+
  theme_minimal()+
  #scale_colour_brewer(palette = "Greens", type = "seq", direction=-1, "Colony", guide = "none") + 
  scale_color_manual(values = brewer.pal(8, "Greens")[c(7,4)])+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), expand = c(0.02,0.01), 
                     labels = c("00:00", "", "06:00", "", "12:00", "", "18:00", "", "24:00"))+
  geom_errorbar(data = mod.ests.monsoon, aes(y = y, xmin = conf.low_rel, xmax = conf.high_rel), inherit.aes = F, width = 10)+
  geom_point(data = mod.ests.monsoon, aes(y = y, x = pred_rel), inherit.aes = F)+
  #guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.spacing.y=unit(0.1, "lines"), strip.text.x = element_blank())
plot_time_monsoon


### plot trip metrics #####


head(complete.trips)

trips.long <- gather(complete.trips, key = "trip.metric", value = "value", c(TripDuration, TotalDistance, MaxDistance))
head(trips.long)
trips.long$ColonyLong <- as.factor(trips.long$Colony)
levels(trips.long$ColonyLong)
levels(trips.long$ColonyLong) <- c("Diego Garcia", "Danger Island", "Nelson's Island")

trips.long$ColYrLong <- as.factor(trips.long$ColYear)
levels(trips.long$ColYrLong)
levels(trips.long$ColYrLong) <- c("DG, All", "DI, 2019", "NI, 2018", "NI, 2019")

trips.long$trip.metricLong <- as.factor(trips.long$trip.metric)
levels(trips.long$trip.metricLong)
levels(trips.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

model.estimates <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/AllModelEstimates.csv", as.is = T)
model.estimates$trip.metricLong <- as.factor(model.estimates$trip.metric)
levels(model.estimates$trip.metricLong)
levels(model.estimates$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )
model.estimates$ColYrLong <- as.factor(model.estimates$ColYear)
levels(model.estimates$ColYrLong)
levels(model.estimates$ColYrLong) <- c("DG, All", "DI, 2019", "NI, 2018", "NI, 2019")

trip_metrics <- ggplot(trips.long, aes(x = ColYrLong, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = ColonyLong), alpha = 0.4)+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony") + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony")+
  scale_y_log10(n.breaks = 7,)+
  guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  #geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(conf.low), ymax = 10^(conf.high)), inherit.aes = F, width = 0.3)+
  geom_point(data = model.estimates, aes(x = ColYrLong, y = 10^(predicted)), inherit.aes = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Colony, Year", y = "Value (Distance in km; Duration in hours)")

### combine plots ####


toprow <- cowplot::plot_grid(tripdur, plot_time, align = "h", axis = "bt", labels = c('a', 'b'), label_size = 11)

p <- cowplot::plot_grid(toprow, trip_metrics, labels = c('', 'c'), label_size = 11, ncol = 1, rel_heights = c(1,1))


trip.plot <- ggdraw() + 
  draw_plot(p) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0.05,
    width = 0.15
  )



png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/TripMetricsComposite.png",width = 22, height = 20, units = "cm", res = 300)
print(trip.plot)
dev.off()



toprow.circ <- cowplot::plot_grid(tripdur, plot_time.circ, align = "h", axis = "bt", labels = c('a', 'b'), label_size = 11)

p.circ <- cowplot::plot_grid(toprow.circ, trip_metrics, labels = c('', 'c'), label_size = 11, ncol = 1, rel_heights = c(1,1.05))


trip.plot.circ <- ggdraw() + 
  draw_plot(p.circ) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0.05,
    width = 0.15
  )



png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/TripMetricsComposite_CircTime.png",width = 20, height = 14, units = "cm", res = 300)
print(trip.plot.circ)
dev.off()



legend.Col <- cowplot::get_legend(
  trip_metrics +theme(legend.margin=margin(0,0,0,0))
)

legend.Monsoon <- cowplot::get_legend(
  plot_time_monsoon +theme(legend.margin=margin(0,0,0,0))
)


legend.Multi <- cowplot::get_legend(
  plot_time_col_multi +theme(legend.margin=margin(0,0,0,0))
)


leftcol <- cowplot::plot_grid(tripdur, trip_metrics+theme(legend.position = "none"), 
                              align = "v", axis = "lr", labels = c('a', 'b'), label_size = 12, ncol = 1)

legends <- cowplot::plot_grid(legend.Col, legend.Multi, legend.Monsoon, nrow =1)


rightcol <- cowplot::plot_grid(plot_time_col_multi+theme(legend.position = "none"), plot_time_monsoon+theme(legend.position = "none"),
                               legends, rel_heights = c(1.2,0.9,0.4),
                               align = "v", axis = "lr", labels = c('c', 'd', ''), label_size = 12, ncol = 1)

p.2 <- cowplot::plot_grid(leftcol, rightcol, rel_widths = c(1.4,1),
                         align = "v", axis = "lr", ncol = 2)


trip.plot.2 <- ggdraw() + 
  draw_plot(p.2) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 0.12,
    width = 0.12
  )



#png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/TripMetricsComposite_ColMonsoon.png",width = 25, height = 20, units = "cm", res = 300)
png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/TripMetricsComposite_ColMonsoon_Multi.png",width = 25, height = 20, units = "cm", res = 300)
print(trip.plot.2)
dev.off()



### model coefficients for inclusion in paper #####

head(model.estimates)
model.estimates$pred.t <- 10^model.estimates$predicted
model.estimates$std.error.t <- 10^(model.estimates$predicted + model.estimates$std.error) - model.estimates$pred.t
head(model.estimates)
write.csv(model.estimates[,c("trip.metric", "ColYear","pred.t", "std.error.t")], 
          "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/AllModelEstimates_t_forManuscript.csv",
          row.names = F)


###### Plot single day trip metrics #####

complete.singleday <- subset(ind.trips, complete == TRUE & Trip_length_days == "Single day")
complete.singleday$ColYear <- ifelse(complete.singleday$Colony == "DG", 
                                 paste0(complete.singleday$Colony, "_ALL"),
                                 paste0(complete.singleday$Colony, "_", complete.singleday$Year))


daytrips.long <- gather(complete.singleday, key = "trip.metric", value = "value", c(TripDuration, TotalDistance, MaxDistance))
head(daytrips.long)
daytrips.long$ColonyLong <- as.factor(daytrips.long$Colony)
levels(daytrips.long$ColonyLong)
levels(daytrips.long$ColonyLong) <- c("Diego Garcia", "Danger Island", "Nelson's Island")

daytrips.long$ColYrLong <- as.factor(daytrips.long$ColYear)
levels(daytrips.long$ColYrLong)
levels(daytrips.long$ColYrLong) <- c("DG, All", "DI, 2019", "NI, 2018", "NI, 2019")

daytrips.long$trip.metricLong <- as.factor(daytrips.long$trip.metric)
levels(daytrips.long$trip.metricLong)
levels(daytrips.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

day.model.estimates <- read.csv("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/SingleDayModelEstimates.csv", as.is = T)
day.model.estimates$trip.metricLong <- as.factor(day.model.estimates$trip.metric)
levels(day.model.estimates$trip.metricLong)
levels(day.model.estimates$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )
day.model.estimates$ColYrLong <- as.factor(day.model.estimates$ColYear)
levels(day.model.estimates$ColYrLong)
levels(day.model.estimates$ColYrLong) <- c("DG, All", "DI, 2019", "NI, 2018", "NI, 2019")

daytrip_metrics <- ggplot(daytrips.long, aes(x = ColYrLong, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = ColonyLong), alpha = 0.4)+
  #scale_colour_brewer(palette = "RdPu", type = "seq", direction=-1, "Colony") + 
  scale_color_manual(values = brewer.pal(4, "RdPu")[c(4,3,2)], "Colony")+
  scale_y_log10(n.breaks = 7,)+
  guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  #geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  geom_errorbar(data = day.model.estimates, aes(x = ColYrLong, ymin = 10^(conf.low), ymax = 10^(conf.high)), inherit.aes = F, width = 0.3)+
  geom_point(data = day.model.estimates, aes(x = ColYrLong, y = 10^(predicted)), inherit.aes = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Colony, Year", y = "Value (km/hours)")

png("/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Plots/DayTripMetrics.png",width = 18, height = 8, units = "cm", res = 300)
print(daytrip_metrics)
dev.off()


