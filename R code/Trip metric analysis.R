
library(lme4)
library(lubridate)
library(MuMIn)
library(tidyverse)
options(na.action = na.fail)

#--------------------------------#
# Read in trip metrics data   ####
#--------------------------------#

# categorise single vs multi-day trips
RFB_tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  mutate(trip_length_days = case_when(Trip_duration > 24 ~ "multi", .default = "single"))
names(RFB_tripmetrics)

hist(log(RFB_tripmetrics$Trip_duration))
hist(log(RFB_tripmetrics$Total_distance))
hist(log(RFB_tripmetrics$Max_distance))


# subset to complete trips 
RFB_complete <- RFB_tripmetrics %>%
  filter(Completetrip == "Complete") %>%
  mutate(year_f = as.factor(Year),
         log_duration = log10(Trip_duration),
         log_totdist = log10(Total_distance),
         log_maxdist = log10(Max_distance))


#------------------------------------#
# within-colony differences at DG ####
#------------------------------------#
DG_complete <- RFB_complete %>%
  filter(Colony == "DG")

# trip duration ####

m.DG.logduration <- lmer(log_duration ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logduration))
qqline(resid(m.DG.logduration))

summary(m.DG.logduration)
d.DG.logduration <- dredge(m.DG.logduration)

d.DG.logduration.out <- d.DG.logduration  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "DG", Trip_metric = "Trip duration", .before = Breed_Stage)


# total distance ####

m.DG.logtotdist <- lmer(log_totdist ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logtotdist))
qqline(resid(m.DG.logtotdist))

summary(m.DG.logtotdist)
d.DG.logtotdist <- dredge(m.DG.logtotdist)

d.DG.logtotdist.out <- d.DG.logtotdist  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "DG", Trip_metric = "Total distance", .before = Breed_Stage)


# max distance ####

m.DG.logmaxdist <- lmer(log_maxdist ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = DG_complete)
qqnorm(resid(m.DG.logmaxdist))
qqline(resid(m.DG.logmaxdist))

summary(m.DG.logmaxdist)
d.DG.logmaxdist <- dredge(m.DG.logmaxdist)

d.DG.logmaxdist.out <- d.DG.logmaxdist  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "DG", Trip_metric = "Max distance", .before = Breed_Stage)

#------------------------------------#
# within-colony differences at NI ####
#------------------------------------#

NI_complete <- RFB_complete %>%
  filter(Colony == "NI")

# trip duration ####

m.NI.logduration <- lmer(log_duration ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = NI_complete)
qqnorm(resid(m.NI.logduration))
qqline(resid(m.NI.logduration))

summary(m.NI.logduration)
d.NI.logduration <- dredge(m.NI.logduration)

d.NI.logduration.out <- d.NI.logduration  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "NI", Trip_metric = "Trip duration", .before = Breed_Stage)


# total distance ####

m.NI.logtotdist <- lmer(log_totdist ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = NI_complete)
qqnorm(resid(m.NI.logtotdist))
qqline(resid(m.NI.logtotdist))

summary(m.NI.logtotdist)
d.NI.logtotdist <- dredge(m.NI.logtotdist)

d.NI.logtotdist.out <- d.NI.logtotdist  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "NI", Trip_metric = "Total distance", .before = Breed_Stage)


# max distance ####

m.NI.logmaxdist <- lmer(log_maxdist ~ year_f + Monsoon + Sex + Breed_Stage + (1|BirdID), data = NI_complete)
qqnorm(resid(m.NI.logmaxdist))
qqline(resid(m.NI.logmaxdist))

summary(m.NI.logmaxdist)
d.NI.logmaxdist <- dredge(m.NI.logmaxdist)

d.NI.logmaxdist.out <- d.NI.logmaxdist  %>%
  tibble() %>%
  filter(delta < 2) %>%
  mutate(Colony = "NI", Trip_metric = "Max distance", .before = Breed_Stage)


# save inter-colony model selection ####

d.intracol.out <- bind_rows(d.DG.logduration.out, d.DG.logtotdist.out, d.DG.logmaxdist.out,
                            d.NI.logduration.out, d.NI.logtotdist.out, d.NI.logmaxdist.out) %>%
  dplyr::select(-c(`(Intercept)`, weight)) %>%
  flextable() %>%
  colformat_double(j = c(8:10), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Colony", "Trip metric", "Explanatory variables", "Model selection metrics"))), colwidths = c(1,1,4,4), top = TRUE) %>% # add header row
  set_header_labels(Trip_metric = 'Trip metric',
                    Breed_Stage = 'Breeding stage',
                    year_f = 'Year') %>%
  merge_v(j = c(1,2), part = "header") %>%
  merge_v(~Colony) %>%
  merge_v(~Trip_metric) %>%
  fix_border_issues() %>%
  valign(valign = 'top', j = c(1,2), part = 'body') %>%
  hline(i = c(4,6,8,12,16), border = fp_border_default()) %>%
  fontsize(size = 10, part = 'all') # set font size for the table
d.intracol.out
save_as_docx(d.intracol.out, path = here("Tables", "Supplementary_mod_selection_intracol.docx"), align = 'center')

##### ******************* #######
#-------------------------------#
# between colony differences ####
#-------------------------------#

m.all.logduration <- lmer(log_duration ~ Colony + year_f + (1|BirdID), data = RFB_complete)
qqnorm(resid(m.all.logduration))
qqline(resid(m.all.logduration))

summary(m.all.logduration)
d.all.logduration <- dredge(m.all.logduration)

d.all.logduration.out <- d.all.logduration  %>%
  tibble() %>%
  filter(delta < 2)
d.all.logduration.out

plot_model(m.all.logduration)
plot_model(m.all.logduration, type = "pred", show.data = T, jitter = T)
param.logdur <- as.data.frame(get_model_data(m.all.logduration, type = "pred", plot = F)$ColYear)
param.logdur$trip.metric <- "TripDuration"
param.logdur$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")

# total distance ####

complete.trips$log_TotDistance = log10(complete.trips$TotalDistance)


m.all.logTotDistance <- lmer(log_TotDistance ~ ColYear + (1|BirdID), data = complete.trips)
qqnorm(resid(m.all.logTotDistance))
qqline(resid(m.all.logTotDistance))

summary(m.all.logTotDistance)
dredge.logTotDistance <- dredge(m.all.logTotDistance)
write.csv(dredge.logTotDistance, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/dredge.logTotDistance_allcols.csv", row.names = F)

plot_model(m.all.logTotDistance)
plot_model(m.all.logTotDistance, type = "pred", show.data = T, jitter = T)
param.totdist <- as.data.frame(get_model_data(m.all.logTotDistance, type = "pred", plot = F)$ColYear)
param.totdist$trip.metric <- "TotalDistance"
param.totdist$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")



# max distance ####

complete.trips$log_MaxDistance = log10(complete.trips$MaxDistance)
head(complete.trips)

m.all.logMaxDistance <- lmer(log_MaxDistance ~ ColYear + (1|BirdID), data = complete.trips)
qqnorm(resid(m.all.logMaxDistance))
qqline(resid(m.all.logMaxDistance))

summary(m.all.logMaxDistance)
dredge.logMaxDistance <- dredge(m.all.logMaxDistance)
write.csv(dredge.logMaxDistance, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/dredge.logMaxDistance_allcols.csv", row.names = F)

plot_model(m.all.logMaxDistance)
plot_model(m.all.logMaxDistance, type = "pred", show.data = T, jitter = T)
param.maxdist <- as.data.frame(get_model_data(m.all.logMaxDistance, type = "pred", plot = F)$ColYear)
param.maxdist$trip.metric <- "MaxDistance"
param.maxdist$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")

model.estimates <- rbind(param.logdur, param.maxdist, param.totdist)
write.csv(model.estimates, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/AllModelEstimates.csv", row.names = F)


# single day trips ####
#### trip duration ####
### run log normal models
complete.singleday$log_duration = log10(complete.singleday$TripDuration)
complete.singleday$ColYear <- ifelse(complete.singleday$Colony == "DG", 
                                 paste0(complete.singleday$Colony, "_ALL"),
                                 paste0(complete.singleday$Colony, "_", complete.singleday$Year))

m.single.logduration <- lmer(log_duration ~ ColYear + (1|BirdID), data = complete.singleday)
qqnorm(resid(m.single.logduration))
qqline(resid(m.single.logduration))

summary(m.single.logduration)
dredge.logduration <- dredge(m.single.logduration)
#write.csv(dredge.logduration, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/dredge.logduration_allcols.csv", row.names = F)

plot_model(m.single.logduration)
plot_model(m.single.logduration, type = "pred", show.data = T, jitter = T)
param.sing.logdur <- as.data.frame(get_model_data(m.single.logduration, type = "pred", plot = F)$ColYear)
param.sing.logdur$trip.metric <- "TripDuration"
param.sing.logdur$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")


#### total distance ####

complete.singleday$log_TotDistance = log10(complete.singleday$TotalDistance)


m.single.logTotDistance <- lmer(log_TotDistance ~ ColYear + (1|BirdID), data = complete.singleday)
qqnorm(resid(m.single.logTotDistance))
qqline(resid(m.single.logTotDistance))

summary(m.single.logTotDistance)
dredge.logTotDistance <- dredge(m.single.logTotDistance)
#write.csv(dredge.logTotDistance, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/dredge.logTotDistance_allcols.csv", row.names = F)

plot_model(m.single.logTotDistance)
plot_model(m.single.logTotDistance, type = "pred", show.data = T, jitter = T)
param.sing.totdist <- as.data.frame(get_model_data(m.single.logTotDistance, type = "pred", plot = F)$ColYear)
param.sing.totdist$trip.metric <- "TotalDistance"
param.sing.totdist$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")



# max distance ####

complete.singleday$log_MaxDistance = log10(complete.singleday$MaxDistance)
head(complete.singleday)

m.single.logMaxDistance <- lmer(log_MaxDistance ~ ColYear + (1|BirdID), data = complete.singleday)
qqnorm(resid(m.single.logMaxDistance))
qqline(resid(m.single.logMaxDistance))

summary(m.single.logMaxDistance)
dredge.logMaxDistance <- dredge(m.single.logMaxDistance)
#write.csv(dredge.logMaxDistance, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/dredge.logMaxDistance_allcols.csv", row.names = F)

plot_model(m.single.logMaxDistance)
plot_model(m.single.logMaxDistance, type = "pred", show.data = T, jitter = T)
param.sing.maxdist <- as.data.frame(get_model_data(m.single.logMaxDistance, type = "pred", plot = F)$ColYear)
param.sing.maxdist$trip.metric <- "MaxDistance"
param.sing.maxdist$ColYear <- c("DG_ALL", "DI_2019", "NI_2018", "NI_2019")

single.model.estimates <- rbind(param.sing.logdur, param.sing.maxdist, param.sing.totdist)
write.csv(single.model.estimates, "/Users/at687/Documents/BIOT/Breeding data/Project1_RFB Chagos foraging ecology/Data/Model selection/SingleDayModelEstimates.csv", row.names = F)

###****************####

# Sample sizes ######

StageYr <- complete.trips %>% 
  filter(Colony == "NI") %>%
  group_by(Year, Breed_Stage) %>%
  dplyr::summarise(n = length(unique(TripID)))

Sum <- ind.trips %>% 
  dplyr::summarise(n.bird = length(unique(BirdID)),
                   n.trip = length(unique(TripID)))

Col <- ind.trips %>% 
  group_by(Colony) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)),
                   n.trip = length(unique(TripID)))

ColYrMonsoon <- ind.trips %>% 
  group_by(Colony, Year, Monsoon) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)),
                   n.trip = length(unique(TripID)))


ColYrMonsoonSex <- ind.trips %>% 
  group_by(Colony, Year, Monsoon, Sex) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))

ColSex <- ind.trips %>% 
  group_by(Colony, Sex) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))

SummSex <- ind.trips %>% 
  group_by(Sex) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))



ColYrMonsoonStage <- ind.trips %>% 
  group_by(Colony, Year, Monsoon, Breed_Stage) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))


ColStage <- ind.trips %>% 
  group_by(Colony, Breed_Stage) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))

SummStage <- ind.trips %>% 
  group_by(Breed_Stage) %>%
  dplyr::summarise(n.bird = length(unique(BirdID)))


###### Plot difference in trip metrics between colonies #####

head(ind.trips)

ggplot(complete.trips, aes(x = TripDuration))+
  geom_histogram(bins = 120)+
  theme_bw()+
  geom_vline(xintercept = c(24,48,72,96,120), lty = "dashed")+
  scale_x_continuous(limits = c(0,120), breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120))+
  theme(panel.grid.minor = element_blank())+
  labs(x = "Trip Duration (hours)", y = "Frequency")
  

ggplot(complete.trips, aes(x = TripDuration))+
  geom_histogram(bins = 50)#+
  #facet_wrap(ColYear ~., ncol = 1, scales = "free_y")

ggplot(complete.trips, aes(x = TotalDistance/2, y = MaxDistance))+
  geom_point()+
  geom_abline(slope=1, lty = "dashed")+
  geom_smooth(data = subset(complete.trips, Trip_length_days == "Single day"), method = "lm", col = "red")+
  geom_smooth(data = subset(complete.trips, Trip_length_days == "Multi-day"), method = "lm", col = "blue")
