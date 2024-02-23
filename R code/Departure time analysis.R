####### RFB GPS Analysis #########
library(lme4)
library(lubridate)
library(MuMIn)
library(suncalc)
library(tidyverse)
library(sjPlot)
library(RColorBrewer)
library(flextable)
library(here)
library(ggeffects)


# read in data and add local arrival/departure times ####
ind.trips <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  mutate(colony_sub = case_when(Colony == "DG" & Year != 2022 ~ "BP",
                                Colony == "DG" & Year == 2022 ~ "EI", 
                                .default = Colony),
         year_f = as.factor(Year),
         Breed_Stage = factor(Breed_Stage, 
                              levels = c("chick-rearing", "incubation", "pre-egg", "breeding")),
         Colony_f = factor(colony_sub, 
                           levels = c("BP", "EI", "DI", "NI")),
         Dep_DateTimelocal = with_tz(DepartureTime, tz = "Indian/Chagos"),
         Dep_Timelocal = hour(ymd_hms(Dep_DateTimelocal)) + minute(ymd_hms(Dep_DateTimelocal))/60,
         Arr_DateTimelocal = with_tz(ArrivalTime, tz = "Indian/Chagos"),
         Arr_Timelocal = hour(ymd_hms(Arr_DateTimelocal)) + minute(ymd_hms(Arr_DateTimelocal))/60) %>%
  filter(Completetrip == "Complete")
head(ind.trips)

# add colony positions for dawn/dusk times ####
colony_positions <- read_csv("Data/Colonies.csv")
ind.trips_pos <- dplyr::left_join(ind.trips, colony_positions, by = "Colony")

Dep_sunpos <- ind.trips_pos %>%
  mutate(date = as_date(Dep_DateTimelocal)) %>%
  select(date, lat = Lat, lon = Long)

Dep_sundata = getSunlightTimes(data = Dep_sunpos, 
                               keep = c("nauticalDawn"), tz = "Indian/Chagos")

Arr_sunpos <- ind.trips_pos %>%
  mutate(date = as_date(Arr_DateTimelocal)) %>%
  select(date, lat = Lat, lon = Long)

Arr_sundata = getSunlightTimes(data = Arr_sunpos, 
                               keep = c("nauticalDusk"), tz = "Indian/Chagos")

ind.trips_sun <- ind.trips_pos %>%
  mutate(Dep_nauticalDawn = Dep_sundata$nauticalDawn,
         Arr_nauticalDusk = Arr_sundata$nauticalDusk,
         Dep_time_after_dawn = as.numeric(interval(Dep_nauticalDawn, Dep_DateTimelocal), "hours"),
         Arr_time_after_dusk = as.numeric(interval(Arr_nauticalDusk, Arr_DateTimelocal), "hours"))


# subset to complete trips ####
trips_sun_complete <- ind.trips_sun %>%
  filter(Completetrip == "Complete") %>%
  mutate(year_f = as.factor(Year),
         Trip_length_days = ifelse(Trip_duration > 24, "Multi-day", "Single day"),
         Breed_Stage = factor(Breed_Stage, 
                              levels = c("chick-rearing", "incubation", "pre-egg", "breeding")),
         Colony_f = factor(colony_sub, 
                           levels = c("BP", "EI", "DI", "NI")))

hist(trips_sun_complete$Dep_time_after_dawn, 100)
hist(trips_sun_complete$Arr_time_after_dusk, 100)

write.csv(trips_sun_complete, "Data/RFB_2016-2023_tripmetrics_sun_complete.csv")

#------------------------------------#
# model ####
#------------------------------------#
# departure time ####

m.deptime <- lmerTest::lmer(Dep_time_after_dawn ~ Colony_f + Monsoon + Sex + Breed_Stage + Trip_length_days + (1|year_f) + (1|BirdID), 
                                data = trips_sun_complete, REML = T)
summary(m.deptime)

# Save model output
t.deptime <- as_flextable(m.deptime) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,13), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t.deptime
save_as_docx(t.deptime, path = here("Tables", "Supplementary_mod_output_deptime.docx"), align = 'center')

# Save model estimates
p.deptime_col <- as.data.frame(ggemmeans(m.deptime, terms = c("Colony_f")))
p.deptime_sex <- as.data.frame(ggemmeans(m.deptime, terms = c("Sex")))
p.deptime_multi <- as.data.frame(ggemmeans(m.deptime, terms = c("Trip_length_days")))
p.deptime <- bind_rows(p.deptime_col, p.deptime_sex, p.deptime_multi) %>%
  mutate(TripSegment = "DepTime")



# arrival time ####

m.arrtime <- lmerTest::lmer(Arr_time_after_dusk ~ Colony_f + Monsoon + Sex + Breed_Stage + Trip_length_days + (1|year_f) + (1|BirdID), 
                            data = trips_sun_complete, REML = T)
summary(m.arrtime)

# Save model output
t.arrtime <- as_flextable(m.arrtime) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,13), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t.arrtime
save_as_docx(t.arrtime, path = here("Tables", "Supplementary_mod_output_arrtime.docx"), align = 'center')



# save model estimates
p.triptimes <- p.deptime %>%
  mutate(FE = case_when(x %in% c("BP", "EI", "DI", "NI") ~ "Colony",
                        x %in% c("Multi-day", "Single day") ~ "Trip_length",
                        .default = "Sex"))

p.triptimes
write_csv(p.triptimes, "Data/mod_estimates_triptimes.csv")


