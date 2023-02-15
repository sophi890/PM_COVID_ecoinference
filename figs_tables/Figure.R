library(tidyverse)
library(sp)
library("dplyr")
library("sf")
library("stringr")
library("ggplot2")
library(grid) 
library(pBrackets) 
library(gridExtra)
library("lme4")

library('maps')

library('rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Figure 1 the US PM2.5 and COVID-19 death maps
us <- map_data('state')

# Plot prevalence of COVID-19
states <- st_read("./data/cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

# Create aggregate_pm_census_cdc_tests_beds from Preprocessing.R before running this
aggregate_pm_census_cdc_test_beds$fips <- str_pad(aggregate_pm_census_cdc_test_beds$fips, 5, pad = "0")
covid_us <- mutate(aggregate_pm_census_cdc_test_beds,
                   STATEFP = str_sub(fips, 1, 2),
                   COUNTYFP = str_sub(fips, 3, 5))
str(covid_us)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
statesCOVID <- left_join(states, covid_us, by = c("STATEFP", "COUNTYFP"))
statesCOVID$mortality <- statesCOVID$Deaths/statesCOVID$population * 10^5 #change
statesCOVID$logmortality <- log10(statesCOVID$Deaths / statesCOVID$population * 10^5) #change
statesCOVID$logmortality[statesCOVID$logmortality < 0] <- (-1)
statesCOVID$logmortality[is.na(statesCOVID$logmortality)] <- (-1)

g1 <- ggplot(statesCOVID) +
  xlim(-125, -65) +
  ylim(25, 50) +
  #  geom_sf(aes(fill = PD_p),color=NA,size=0.025)+
  geom_sf(aes(fill = logmortality), color = NA, size = 0.005) + # or grey
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 deaths per 100,000")), 
                       low = "#FFFFFF",
                       mid = "#ffcccb",
                       high = "#8b0000",
                       midpoint = 1,
                       breaks = c(-1, 0, 1, 2, 3), 
                       labels = c("0", "1", "10", "100", "1000"),
                       limits = c(-1, 3.5),
                       na.value = "white") +
  # labs(title = expression(paste("Cumulative Deaths Related to COVID-19 until March 30, 2020"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20 * 2),
        legend.text.align = 0.65,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(125 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_covid.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g1
dev.off()

county_pm_aggregated <- county_pm %>%
  group_by(fips) %>% 
  summarise(mean_pm25 = mean(pm25))

county_pm_aggregated$fips <- str_pad(county_pm_aggregated$fips, 5, pad = "0")
county_pm_aggregated <- mutate(county_pm_aggregated,
                               STATEFP = str_sub(fips, 1, 2),
                               COUNTYFP = str_sub(fips, 3, 5))
str(county_pm_aggregated)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
statesPM <- left_join(states, 
                      county_pm_aggregated, 
                      by = c("STATEFP", "COUNTYFP"))

g2 <- ggplot(statesPM) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  geom_sf(aes(fill = mean_pm25), color=NA, size = 0.005) + # or grey
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("PM"[2.5])), 
                       low = "#FFFFFF", #1e90ff
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = 6, # 9
                       breaks = c(0, 3, 6, 9, 12),
                       labels = c("0", "3", "6", "9", "12"),
                       limits = c(0, 15),
                       na.value = "white") +
  # labs(title = expression(paste("Annual Average of PM"[2.5]," per ",mu,g/m^3," in 2000-2016"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24 * 2,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 24 * 2),
        legend.key.width = unit(150 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_pm.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g2
dev.off()

# Create weightevarlist.pm25 from Preprocessing.R before running this
pm25_var = cbind.data.frame(fips = as.numeric(names(weightedvarlist.pm25)), weightedvarlist.pm25)
pm25_var$fips <- str_pad(pm25_var$fips, 5, pad = "0")
pm25_var <- mutate(pm25_var,
                   STATEFP = str_sub(fips, 1, 2),
                   COUNTYFP = str_sub(fips, 3, 5))
pm25_var$log10weightedvarlist.pm25 = log10(pm25_var$weightedvarlist.pm25)
str(pm25_var)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
statesPM25_var <- left_join(states, pm25_var, by = c("STATEFP", "COUNTYFP"))

g3 <- ggplot(statesPM25_var) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  geom_sf(aes(fill = log10weightedvarlist.pm25), color=NA, size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("PM"[2.5], "variance")), 
                       low = "#FFFFFF",#"#1e90ff", 
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = -1.4, # 9
                       breaks = c(-3, -2, -1, 0, 1),
                       labels = c("0.001", "0.01", "0.1", "1", "10"),
                       limits = c(-3, 1),
                       na.value = "white") +
  # labs(title = expression(paste("Annual Average of PM"[2.5]," per ",mu,g/m^3," in 2000-2016"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text.align = 0.65,
        legend.title = element_text(size = 24*2),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_pm_variance.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g3
dev.off()

# take two of the above with different census tract PM

pm25_var = cbind.data.frame(fips = as.numeric(names(weightedvarlist.pm25_2)), weightedvarlist.pm25_2)
pm25_var$fips <- str_pad(pm25_var$fips, 5, pad = "0")
pm25_var <- mutate(pm25_var,
                   STATEFP = str_sub(fips, 1, 2),
                   COUNTYFP = str_sub(fips, 3, 5))
pm25_var$log10weightedvarlist.pm25 = log10(pm25_var$weightedvarlist.pm25)
str(pm25_var)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
statesPM25_var <- left_join(states, pm25_var, by = c("STATEFP", "COUNTYFP"))

g3 <- ggplot(statesPM25_var) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  geom_sf(aes(fill = log10weightedvarlist.pm25), color='grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("PM"[2.5], "variance")), 
                       low = "#1e90ff", #FFFFFF
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = -1.4, # 9
                       breaks = c(-3, -2, -1, 0, 1),
                       labels = c("0.001", "0.01", "0.1", "1", "10"),
                       limits = c(-3, 1),
                       na.value = "white") +
  # labs(title = expression(paste("Annual Average of PM"[2.5]," per ",mu,g/m^3," in 2000-2016"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text.align = 0.65,
        legend.title = element_text(size = 24*2),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_pm_variance.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g3
dev.off()

