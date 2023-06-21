library(tidyverse)
library(flextable)
library(officer)
library(sf)

#----------------------------#
# Read in tracking data   ####
#----------------------------#

RFB_trips <- read_csv("Data/RFB_2016-2023.csv")
names(RFB_trips)
str(RFB_trips)


#----------------------------#
# Calculate trip metrics  ####
#----------------------------#


## function to calculate standard error
se <- function(x) sqrt(sd(x, na.rm = T) / length(x[!is.na(x)]))

# Using ExMove code

#--------------------#
## USER INPUT START ##
#--------------------#

sampleRateUnits <- "hours" # units to display sample rate in table

## define levels of grouping factors

## Firstly, down to population level
## here, we are working on data from one population & year, and so use 'Species' and 'Population'
## add any other relevant grouping factors here, e.g., Country / Year / Season / Age

grouping_factors_poplevel <- c("Colony", "Year", "Monsoon", "Breed_Stage", "Sex") 

## Secondly, down to individual level
## add e.g., DeployID if relevant

grouping_factors_indlevel <- c("BirdID")

#------------------#
## USER INPUT END ##
#------------------#

# add distance travelled between each location and distance to colony
RFB_trips <- RFB_trips %>%
  mutate(geometry = st_transform( # assign geometry and transform to WGS for distance calculations
    st_as_sf(., coords=c("Longitude","Latitude"), crs=4326), crs = 3857)$geometry,
    geometry_CP = st_transform( 
      st_as_sf(., coords=c("CPLon","CPLat"), crs=4326), crs = 3857)$geometry) %>%
  group_by(TripID) %>% # calculate per trip
  mutate(dist = as.numeric((st_distance(geometry, lag(geometry), by_element = T)))/1000,
         ColonyDist = as.numeric(st_distance(geometry, geometry_CP, by_element = T))/1000) %>% 
  ungroup() %>% dplyr::select(-c(geometry, geometry_CP))

## trip metrics calculated for each trip = 1 row per trip
length(unique(RFB_trips$TripID))

df_tripmetrics <- RFB_trips %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel, "TripID")))) %>% 
  summarise(DepartureTime = min(datetime),
            ArrivalTime = max(datetime),
            Completetrip = ifelse(complete == TRUE, "Complete", "Incomplete"), # Complete if  last point is back at the CP
            Trip_duration = difftime(max(datetime), min(datetime), units="mins"),
            Total_distance = sum(dist, na.rm = T), #km
            Max_distance = max(ColonyDist)) %>%
  distinct()
df_tripmetrics

# save trip metrics ####
write_csv(df_tripmetrics, 'Data/RFB_2016-2023_tripmetrics.csv')


#------------------------------------------#
# Summarise trip metrics for manuscript ####
#------------------------------------------#

## intermediate = individual level summary = 1 row per individual
## each individual will therefore have equal weighting in the population summary

df_tripmetrics_ind <- df_tripmetrics %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel)))) %>% 
  summarise(Trip_duration_ind_mean = mean(as.numeric(Trip_duration, units = sampleRateUnits), na.rm = T),
            Trip_duration_ind_se = se(as.numeric(Trip_duration, units = sampleRateUnits)),
            Trip_duration_ind_min = min(as.numeric(Trip_duration, units = sampleRateUnits), na.rm = T),
            Trip_duration_ind_max = max(as.numeric(Trip_duration, units = sampleRateUnits), na.rm = T),
            Total_distance_ind_mean = mean(Total_distance),
            Total_distance_ind_se = se(Total_distance),
            Total_distance_ind_min = min(Total_distance),
            Total_distance_ind_max = max(Total_distance),
            Max_distance_ind_mean = mean(Max_distance),
            Max_distance_ind_se = se(Max_distance),
            Max_distance_ind_min = min(Max_distance),
            Max_distance_ind_max = max(Max_distance),
            N_Trips = length(unique(TripID))) 
df_tripmetrics_ind

## population level summary = 1 row per population

df_tripmetrics_summary_pop <- df_tripmetrics_ind %>%
  group_by(Colony, Year, Monsoon) %>%
  summarise(Trip_duration_mean = mean(Trip_duration_ind_mean),
            Trip_duration_se = se(Trip_duration_ind_mean),
            Trip_duration_min = min(Trip_duration_ind_min),
            Trip_duration_max = max(Trip_duration_ind_max),
            Total_distance_mean = mean(Total_distance_ind_mean),
            Total_distance_se = se(Total_distance_ind_mean),
            Total_distance_min = min(Total_distance_ind_min),
            Total_distance_max = max(Total_distance_ind_max),
            Max_distance_mean = mean(Max_distance_ind_mean),
            Max_distance_se = se(Max_distance_ind_mean),
            Max_distance_min = min(Max_distance_ind_min),
            Max_distance_max = max(Max_distance_ind_max),
            N_Trips_total = sum(N_Trips)) %>%
  mutate(Year = as.character(Year))
df_tripmetrics_summary_pop


## colony level summary = 1 row per colony
df_tripmetrics_summary_colony <- df_tripmetrics_summary_pop %>%
  group_by(Colony) %>%
  summarise(Trip_duration_mean_col = mean(Trip_duration_mean),
            Trip_duration_se = se(Trip_duration_mean),
            Trip_duration_min = min(Trip_duration_min),
            Trip_duration_max = max(Trip_duration_max),
            Total_distance_mean_col = mean(Total_distance_mean),
            Total_distance_se = se(Total_distance_mean),
            Total_distance_min = min(Total_distance_min),
            Total_distance_max = max(Total_distance_max),
            Max_distance_mean_col = mean(Max_distance_mean),
            Max_distance_se = se(Max_distance_mean),
            Max_distance_min = min(Max_distance_min),
            Max_distance_max = max(Max_distance_max),
            N_Trips_total = sum(N_Trips_total)) %>%
  mutate(Year = "ALL", Monsoon = "ALL") %>%
  rename(Trip_duration_mean = Trip_duration_mean_col,
         Total_distance_mean = Total_distance_mean_col,
         Max_distance_mean = Max_distance_mean_col)
df_tripmetrics_summary_colony

## full data summary = 1 row
df_tripmetrics_summary_all <- df_tripmetrics_summary_colony %>%
  summarise(Trip_duration_mean_all = mean(Trip_duration_mean),
            Trip_duration_se = se(Trip_duration_mean),
            Trip_duration_min = min(Trip_duration_min),
            Trip_duration_max = max(Trip_duration_max),
            Total_distance_mean_all = mean(Total_distance_mean),
            Total_distance_se = se(Total_distance_mean),
            Total_distance_min = min(Total_distance_min),
            Total_distance_max = max(Total_distance_max),
            Max_distance_mean_all = mean(Max_distance_mean),
            Max_distance_se = se(Max_distance_mean),
            Max_distance_min = min(Max_distance_min),
            Max_distance_max = max(Max_distance_max),
            N_Trips_total = sum(N_Trips_total)) %>%
  mutate(Colony = "ALL", Year = "ALL", Monsoon = "ALL") %>%
  rename(Trip_duration_mean = Trip_duration_mean_all,
         Total_distance_mean = Total_distance_mean_all,
         Max_distance_mean = Max_distance_mean_all)
df_tripmetrics_summary_all

# make flextable ####
#new header labels
pars <- as_paragraph(as_chunk(c("Colony", "Year", "Monsoon", "Trip duration (hours)", "Total distance (km)", "Max distance (km)")))

df_tripmetrics_table <- bind_rows(df_tripmetrics_summary_pop, df_tripmetrics_summary_colony, df_tripmetrics_summary_all)%>%
  arrange(Colony, Year, Monsoon) %>%
  select(-N_Trips_total)%>%
  flextable() %>%
  set_header_labels(Trip_duration_mean = 'mean',
                    Trip_duration_se = 'se',
                    Trip_duration_min = 'min',
                    Trip_duration_max = 'max',
                    Total_distance_mean = 'mean',
                    Total_distance_se = 'se',
                    Total_distance_min = 'min',
                    Total_distance_max = 'max',
                    Max_distance_mean = 'mean',
                    Max_distance_se = 'se',
                    Max_distance_min = 'min',
                    Max_distance_max = 'max') %>%
  add_header_row(values = pars, colwidths = c(1,1,1,4,4,4), top = TRUE) %>%
  merge_v(j = c(1,2,3), part = "header") %>%
  merge_v(~Colony) %>%
  merge_v(~Year) %>%
  fix_border_issues() %>%
  valign(valign = 'top', j = c(1,2), part = 'body') %>%
  colformat_double(j = c(4:15), digits = 1) %>%
  align(align = 'right', j = c(4:15), part = 'header') %>%
  align(align = 'right', j = c(4:15),  part = 'body') %>%
  hline(i = c(1,8,10), border = fp_border_default()) %>%
  fontsize(size = 9, part = 'all') %>% 
  bg(bg = "grey90", i = c(1,8,10,14), part = "body") %>%
  autofit
  
df_tripmetrics_table

# save trip metrics summary as word doc ####
save_as_docx(df_tripmetrics_table, path ='Tables/trip_metrics.docx', pr_section = prop_section(page_size(orient = 'landscape')))


#------------------------------------------#
# Calculate sample sizes for manuscript ####
#------------------------------------------#

unique(RFB_trips$Sex)
unique(RFB_trips$Breed_Stage)

df_sample_sizes_pop <- df_tripmetrics %>%
  group_by(Colony, Year, Monsoon) %>%
  summarise(no.inds = n_distinct(BirdID),
            no.trips = n_distinct(TripID),
            Female = n_distinct(BirdID[Sex == "F"]),
            Male = n_distinct(BirdID[Sex == "M"]),
            Unkown = n_distinct(BirdID[Sex == "unknown"]),
            Pre.egg = n_distinct(BirdID[Breed_Stage == "pre-egg"]),
            Incubation = n_distinct(BirdID[Breed_Stage == "incubation"]),
            Chick.rearing = n_distinct(BirdID[Breed_Stage == "chick-rearing"]),
            Breeding = n_distinct(BirdID[Breed_Stage == "breeding"])) %>%
  mutate(Year = as.factor(Year))
df_sample_sizes_pop


df_sample_sizes_col <- df_tripmetrics %>%
  group_by(Colony) %>%
  summarise(no.inds = n_distinct(BirdID),
            no.trips = n_distinct(TripID),
            Female = n_distinct(BirdID[Sex == "F"]),
            Male = n_distinct(BirdID[Sex == "M"]),
            Unkown = n_distinct(BirdID[Sex == "unknown"]),
            Pre.egg = n_distinct(BirdID[Breed_Stage == "pre-egg"]),
            Incubation = n_distinct(BirdID[Breed_Stage == "incubation"]),
            Chick.rearing = n_distinct(BirdID[Breed_Stage == "chick-rearing"]),
            Breeding = n_distinct(BirdID[Breed_Stage == "breeding"])) %>%
  mutate(Year = "ALL", Monsoon = "ALL")
df_sample_sizes_col


df_sample_sizes_all <- df_tripmetrics %>%
  group_by() %>%
  summarise(no.inds = n_distinct(BirdID),
            no.trips = n_distinct(TripID),
            Female = n_distinct(BirdID[Sex == "F"]),
            Male = n_distinct(BirdID[Sex == "M"]),
            Unkown = n_distinct(BirdID[Sex == "unknown"]),
            Pre.egg = n_distinct(BirdID[Breed_Stage == "pre-egg"]),
            Incubation = n_distinct(BirdID[Breed_Stage == "incubation"]),
            Chick.rearing = n_distinct(BirdID[Breed_Stage == "chick-rearing"]),
            Breeding = n_distinct(BirdID[Breed_Stage == "breeding"])) %>%
  mutate(Colony = "ALL", Year = "ALL", Monsoon = "ALL")
df_sample_sizes_all

# make flextable ####

pars2 <- as_paragraph(as_chunk(c("Colony", "Colony size\n(br. pairs)", "Latitude\n(°N)",  "Longitude\n(°E)", 
                                 "Year", "Monsoon", "No.\nindividuals", "No.\ntrips", "No. individuals by Sex", "No. individuals by Breeding Stage")))

df_sample_sizes_table <- bind_rows(df_sample_sizes_pop, df_sample_sizes_col, df_sample_sizes_all)%>%
  arrange(Colony, Year, Monsoon) %>%
  mutate(Col.size = case_when(Colony == "DG" ~ 8068, Colony == "DI" ~ 3500, Colony == "NI" ~ 3300, .default = NA),
         Lat = case_when(Colony == "DG" ~ -7.23, Colony == "DI" ~ -6.39, Colony == "NI" ~ -5.68, .default = NA),
         Lon = case_when(Colony == "DG" ~ 72.43, Colony == "DI" ~ 71.24, Colony == "NI" ~ 72.32, .default = NA),
         .after = Colony) %>%
  flextable() %>%
  set_header_labels(Col.size = 'Colony size\n(br. pairs)',
                    Lat = 'Latitude\n(°N)',
                    Lon = 'Longitude\n(°E)',
                    no.inds = 'No.\nindividuals',
                    no.trips = 'No.\ntrips',
                    Pre.egg = 'Pre-egg\nlaying',
                    Chick.rearing = 'Chick\nrearing',
                    Breeding = 'Unkown') %>%
  add_header_row(values = pars2, colwidths = c(1,1,1,1,1,1,1,1,3,4), top = TRUE) %>%
  merge_v(j = c(1:8), part = "header") %>%
  merge_v(~Colony) %>%
  merge_v(~Col.size) %>%
  merge_v(~Lat) %>%
  merge_v(~Lon) %>%
  merge_v(~Year) %>%
  fix_border_issues() %>%
  valign(valign = 'top', j = c(1:5), part = 'body') %>%
  hline(i = c(1,8,10), border = fp_border_default()) %>%
  fontsize(size = 9, part = 'all') %>% 
  bg(bg = "grey90", i = c(1,8,10,14), part = "body") %>%
  autofit(add_w = 0, add_h = 0)

df_sample_sizes_table


# save sample sizes as word doc ####
save_as_docx(df_sample_sizes_table, path ='Tables/sample_sizes.docx', pr_section = prop_section(page_size(orient = 'landscape')))
