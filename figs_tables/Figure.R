library(tidyverse)
library(sp)
#library("raster")
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

#Plot prevalence of COVID-19
states <- st_read("./data/cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

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
  geom_sf(aes(fill = logmortality), color = 'grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 deaths per 100,000")), 
                       low = "#FFFFFF",
                       mid = "#ffcccb",
                       high = "#8b0000",
                       midpoint = 1,
                       breaks = c(-1, 0, 1, 2, 3), 
                       labels = c("0", "1", "10", "100", "1000+"),
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

png("county_covid_trial.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
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
  geom_sf(aes(fill = mean_pm25), color='grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("PM"[2.5])), 
                       low = "#FFFFFF", #1e90ff
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = 6, # 9
                       breaks = c(0, 3, 6, 9, 12),
                       labels = c("0", "3", "6", "9", "12+"),
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
  geom_sf(aes(fill = log10weightedvarlist.pm25), color='grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("PM"[2.5], "variance")), 
                       low = "#FFFFFF", #1e90ff
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = -1.4, # 9
                       breaks = c(-3, -2, -1, 0, 1),
                       labels = c("0.001", "0.01", "0.1", "1", "10+"),
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

loghouseholdincome.var = unlist(lapply(covlist, function(x){x[1,1]}))
loghousevalue.var = unlist(lapply(covlist, function(x){x[2,2]}))
county_finances = cbind.data.frame(fips = covid_data$fips, loghouseholdincome.var, loghousevalue.var)
county_finances$fips <- str_pad(county_finances$fips, 5, pad = "0")
county_finances <- mutate(county_finances,
                   STATEFP = str_sub(fips, 1, 2),
                   COUNTYFP = str_sub(fips, 3, 5))
str(county_finances)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
statescounty_finances <- left_join(states, county_finances, by = c("STATEFP", "COUNTYFP"))

g4 <- ggplot(statescounty_finances) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  geom_sf(aes(fill = loghouseholdincome.var), color='grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("Variance of log household income")), 
                       low = "#FFFFFF", #1e90ff
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = 0.7, # try 1?
                       breaks = c(0.5, 0.75, 1, 1.25, 1.5),
                       labels = c("0.5", "0.75", "1", "1.25", "1.5+"),
                       limits = c(0.5, 1.5),
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
        legend.text = element_text(angle = 60,  size = 30),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(112 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_loghouseholdincome_var.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g4
dev.off()

g5 <- ggplot(statescounty_finances) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  geom_sf(aes(fill = loghousevalue.var), color='grey', size = 0.005) +
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  # scale_fill_viridis_c(option="magma",begin=0,direction = -1, breaks = c(0,4,8,12,16,20))+
  scale_fill_gradient2(expression(paste("Variance of log house value")), 
                       low = "#FFFFFF", #1e90ff
                       mid = "#ffcccb", #ffffba
                       high = "#8b0000", #8b0000
                       midpoint = 0.88,
                       breaks = c(0.2, 0.5, 0.8, 1.1, 1.4),
                       labels = c("0.2", "0.5", "0.8", "1.1", "1.4+"),
                       limits = c(0.2, 1.7),
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
        legend.text = element_text(angle = 60,  size = 30),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(112 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png("county_loghousevalue_var.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2)
g5
dev.off()

# Figure 2 Odds Ratios and Sensitivity Analyses

## Figure 2a
load('./results/fit6.Rdata')
vec1 = exp((extract(fit6)$pars[,24]))
load('./results/fit1_x.random.dec.RData')
vec2 = exp((extract(fit1_x.random)$pars[,2]))
load('./results/fit12.Rdata')
vec3 = exp((extract(fit12)$pars[,24]))
load('./results/fit1_x.random.RData')
vec4 = exp((extract(fit1_x.random)$pars[,2]))

df = data.frame(x = as.factor(c('PUMS, 12/1/2020 (Main Analysis)', 'no PUMS, 12/1/2020', 'PUMS, 6/18/2020', 'no PUMS, 6/18/2020' )), 
                y = c(mean(vec1), mean(vec2), mean(vec3), mean(vec4)), 
                lower = c(quantile(vec1, 0.025), quantile(vec2, 0.025), quantile(vec3, 0.025), quantile(vec4, 0.025)), 
                upper = c(quantile(vec1, 0.975), quantile(vec2, 0.975), quantile(vec3, 0.975), quantile(vec4, 0.975)))

g3 = ggplot(df, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "red") + 
  scale_x_discrete(limits = c('PUMS, 12/1/2020 (Main Analysis)', 'no PUMS, 12/1/2020', 'PUMS, 6/18/2020', 'no PUMS, 6/18/2020' )) + 
  ylab('Odds Ratios for PM 2.5') + xlab(element_blank()) + ylim(0.99, 1.12)

png("pm25_or.jpeg", height = 512, width = 850)
g3
dev.off()

## Figure 2b

# Main Analysis
load('./results/fit6.Rdata')
vec0 = exp((extract(fit6)$pars[,24]))

# Sensitivities
load('./results/fit5.Rdata')
vec1 = exp((extract(fit5)$pars[,17]))
load('./results/fit8.Rdata')
vec2 = exp((extract(fit8)$pars[,26]))
load('./results/fit7.Rdata')
vec3 = exp((extract(fit7)$pars[,17]))
load('./results/fit2.Rdata')
vec4 = exp((extract(fit2)$pars[,2]))
load('./results/fit1.Rdata')
vec5 = exp((extract(fit1)$pars[,2]))
load('./results/fit4.Rdata')
vec6 = exp((extract(fit4)$pars[,2]))
load('./results/fit3.Rdata')
vec7 = exp((extract(fit3)$pars[,2]))

df = data.frame(x = as.factor(c('Main Analysis', 'Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5', 'Model 6', 'Model 7')), 
                y = c(mean(vec0), mean(vec1), mean(vec2), mean(vec3), mean(vec4), mean(vec5), mean(vec6), mean(vec7)), 
                lower = c(quantile(vec0, 0.025), quantile(vec1, 0.025), quantile(vec2, 0.025), quantile(vec3, 0.025), quantile(vec4, 0.025), quantile(vec5, 0.025), quantile(vec6, 0.025), quantile(vec7, 0.025)), 
                upper = c(quantile(vec0, 0.975), quantile(vec1, 0.975), quantile(vec2, 0.975), quantile(vec3, 0.975), quantile(vec4, 0.975), quantile(vec5, 0.975), quantile(vec6, 0.975), quantile(vec7, 0.975)))

g4 = ggplot(df, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "red") + 
  scale_x_discrete(limits = c('Main Analysis', 'Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5', 'Model 6', 'Model 7')) + 
  ylab('Odds Ratios for PM 2.5') + xlab(element_blank()) + ylim(0.99, 1.06) + theme(axis.text=element_text(size=14),
                                                                                    axis.title=element_text(size=14))

png("pm25_or_sensitivity.jpeg", height = 512, width = 850)
g4
dev.off()
