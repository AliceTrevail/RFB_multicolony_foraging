
library(lme4)
library(lubridate)
library(MuMIn)
library(tidyverse)
library(here)
library(sjPlot)
library(broom.mixed)
library(ggeffects)
library(flextable)
options(na.action = na.fail)


#--------------------------------#
# Read in trip metrics data   ####
#--------------------------------#

# categorise single vs multi-day trips
RFB_tripmetrics <- read_csv("Data/RFB_2016-2023_tripmetrics.csv") %>%
  mutate(trip_length_days = case_when(Trip_duration > 24 ~ "multi", .default = "single"),
         colony_sub = case_when(Colony == "DG" & Year != 2022 ~ "BP",
                                Colony == "DG" & Year == 2022 ~ "EI", 
                                .default = Colony))
names(RFB_tripmetrics)

hist(log(RFB_tripmetrics$Trip_duration))
hist(log(RFB_tripmetrics$Total_distance))
hist(log(RFB_tripmetrics$Max_distance))


# subset to complete trips 
RFB_complete <- RFB_tripmetrics %>%
  filter(Completetrip == "Complete") %>%
  mutate(year_f = as.factor(Year),
         log_duration = log(Trip_duration),
         log_totdist = log(Total_distance),
         log_maxdist = log(Max_distance),
         Breed_Stage = factor(Breed_Stage, 
                              levels = c("chick-rearing", "incubation", "pre-egg", "breeding")),
         Colony_f = factor(colony_sub, 
                              levels = c("BP", "EI", "DI", "NI")))



#------------------------------------#
# model ####
#------------------------------------#
# trip duration ####

m.logduration <- lmerTest::lmer(log_duration ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                                data = RFB_complete, REML = T)
summary(m.logduration)

# Save model output
t.logduration <- as_flextable(m.logduration) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t.logduration
save_as_docx(t.logduration, path = here("Tables", "Supplementary_mod_output_duration.docx"), align = 'center')

# Save model estimates
p.logduration_col <- as.data.frame(ggemmeans(m.logduration, terms = c("Colony_f")))
p.logduration_sex <- as.data.frame(ggemmeans(m.logduration, terms = c("Sex")))
p.logduration <- bind_rows(p.logduration_col, p.logduration_sex) %>%
  mutate(TripMetric = "TripDuration")



# total distance ####

m.logtotdist <- lmerTest::lmer(log_totdist ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                                data = RFB_complete, REML = T)

summary(m.logtotdist)

# Save model output
t.logtotdist <- as_flextable(m.logtotdist) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t.logtotdist
save_as_docx(t.logtotdist, path = here("Tables", "Supplementary_mod_output_totdist.docx"), align = 'center')

# Save model estimates
p.logtotdist_col <- as.data.frame(ggemmeans(m.logtotdist, terms = c("Colony_f")))
p.logtotdist_sex <- as.data.frame(ggemmeans(m.logtotdist, terms = c("Sex")))
p.logtotdist <- bind_rows(p.logtotdist_col, p.logtotdist_sex) %>%
  mutate(TripMetric = "TotalDistance")



# max distance ####

m.logmaxdist <- lmerTest::lmer(log_maxdist ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                               data = RFB_complete, REML = T)

summary(m.logmaxdist)

# Save model output
t.logmaxdist <- as_flextable(m.logmaxdist) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t.logmaxdist
save_as_docx(t.logmaxdist, path = here("Tables", "Supplementary_mod_output_maxdist.docx"), align = 'center')

# Save model estimates
p.logmaxdist_col <- as.data.frame(ggemmeans(m.logmaxdist, terms = c("Colony_f")))
p.logmaxdist_sex <- as.data.frame(ggemmeans(m.logmaxdist, terms = c("Sex")))
p.logmaxdist <- bind_rows(p.logmaxdist_col, p.logmaxdist_sex) %>%
  mutate(TripMetric = "MaxDistance")


# Combine model estimates ####


p.tripmetrics <- bind_rows(p.logduration, p.logtotdist, p.logmaxdist) %>%
  mutate(FE = case_when(x %in% c("BP", "EI", "DI", "NI") ~ "Colony",
                        .default = "Sex"),
         predicted_invlog = exp(predicted),
         conf.low_invlog = exp(conf.low),
         conf.high_invlog = exp(conf.high)) %>%
  arrange(FE, TripMetric, x)

p.tripmetrics
write_csv(p.tripmetrics, "Data/mod_estimates_tripmetrics.csv")



#------------------------------------###
# model single days ####
#------------------------------------###

# subset to complete trips 
RFB_single <- RFB_complete %>%
  filter(Trip_duration <24) 

# trip duration ####

m_single.logduration <- lmerTest::lmer(log_duration ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                                data = RFB_single, REML = T)
summary(m_single.logduration)

# Save model output
t_single.logduration <- as_flextable(m_single.logduration) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t_single.logduration

# Save model estimates
p_single.logduration_col <- as.data.frame(ggemmeans(m_single.logduration, terms = c("Colony_f")))
p_single.logduration_sex <- as.data.frame(ggemmeans(m_single.logduration, terms = c("Sex")))
p_single.logduration <- bind_rows(p_single.logduration_col, p_single.logduration_sex) %>%
  mutate(TripMetric = "TripDuration")



# total distance ####

m_single.logtotdist <- lmerTest::lmer(log_totdist ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                               data = RFB_single, REML = T)

summary(m_single.logtotdist)

# Save model output
t_single.logtotdist <- as_flextable(m_single.logtotdist) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t_single.logtotdist

# Save model estimates
p_single.logtotdist_col <- as.data.frame(ggemmeans(m_single.logtotdist, terms = c("Colony_f")))
p_single.logtotdist_sex <- as.data.frame(ggemmeans(m_single.logtotdist, terms = c("Sex")))
p_single.logtotdist <- bind_rows(p_single.logtotdist_col, p_single.logtotdist_sex) %>%
  mutate(TripMetric = "TotalDistance")



# max distance ####

m_single.logmaxdist <- lmerTest::lmer(log_maxdist ~ Colony_f + Monsoon + Sex + Breed_Stage + (1|year_f) + (1|BirdID), 
                               data = RFB_single, REML = T)

summary(m_single.logmaxdist)

# Save model output
t_single.logmaxdist <- as_flextable(m_single.logmaxdist) %>%
  align(align = 'left', j = c(1), part = 'body') %>%
  bg(bg = "grey90", i = c(1,12), part = "body") %>%
  fontsize(size = 9, part = 'all')%>%
  autofit()
t_single.logmaxdist

# Save model estimates
p_single.logmaxdist_col <- as.data.frame(ggemmeans(m_single.logmaxdist, terms = c("Colony_f")))
p_single.logmaxdist_sex <- as.data.frame(ggemmeans(m_single.logmaxdist, terms = c("Sex")))
p_single.logmaxdist <- bind_rows(p_single.logmaxdist_col, p_single.logmaxdist_sex) %>%
  mutate(TripMetric = "MaxDistance")


# Combine model estimates ####


p_single.tripmetrics <- bind_rows(p_single.logduration, p_single.logtotdist, p_single.logmaxdist) %>%
  mutate(FE = case_when(x %in% c("BP", "EI", "DI", "NI") ~ "Colony",
                        .default = "Sex"),
         predicted_invlog = exp(predicted),
         conf.low_invlog = exp(conf.low),
         conf.high_invlog = exp(conf.high)) %>%
  arrange(FE, TripMetric, x)

p_single.tripmetrics
write_csv(p_single.tripmetrics, "Data/mod_estimates_tripmetrics_singleday.csv")


