library(tidyverse)
library(dplyr)
library(stringr)
library(RCurl)
library(httr)
library(gdata)
library(Hmisc)
library(xml2)
library(rvest)

# This file compiles all datasets from raw data for Main Text: 
# main analysis: dat_main.RData 
# sensitivity analyses: dat_1.RData, ..., dat_7.RData
# additional analyses: dat_add1.RData, ..., dat_add3.RData

# Read in household PUMS
options(timeout=5000)
temp = tempfile()
download.file('https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/csv_hus.zip', temp)
husa = read.csv(unz(temp, 'psam_husa.csv'))
husb = read.csv(unz(temp, 'psam_husb.csv'))
husc = read.csv(unz(temp, 'psam_husc.csv'))
husd = read.csv(unz(temp, 'psam_husd.csv'))
unlink(temp)
hus = rbind(husa, husb, husc, husd)

# Read in person PUMS
temp = tempfile()
download.file('https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/csv_pus.zip', temp)
pusa = read.csv(unz(temp, 'psam_pusa.csv'))
pusb = read.csv(unz(temp, 'psam_pusb.csv'))
pusc = read.csv(unz(temp, 'psam_pusc.csv'))
pusd = read.csv(unz(temp, 'psam_pusd.csv'))
unlink(temp)
pus = rbind(pusa, pusb, pusc, pusd)

# Merge
pus.hus.merged = merge(pus[,c('PUMA', 'ST', 'SERIALNO', 'AGEP', 'SEX', 'RAC1P', 'POVPIP', 'SCHL', 'PWGTP')], 
                       hus[,c('PUMA', 'ST', 'SERIALNO', 'TEN', 'WGTP')], by = c('PUMA', 'ST', 'SERIALNO'), all.x = T)

# Age
pus.hus.merged$age.factor[pus.hus.merged$AGEP <= 39] = '0.39'
pus.hus.merged$age.factor[pus.hus.merged$AGEP >=40] = '40.'
pus.hus.merged$age.factor = factor(pus.hus.merged$age.factor)

# Sex
pus.hus.merged$sex.factor = factor(pus.hus.merged$SEX)
levels(pus.hus.merged$sex.factor) = c('Male', 'Female')

# Race
pus.hus.merged$race.factor = factor(pus.hus.merged$RAC1P)
levels(pus.hus.merged$race.factor) <- list("White"=c("1"), 
                                           "Black.AfricanAmerican"=c("2"), 
                                           "OtherRace"=c("3","4","5","6","7","8","9"))

# Poverty
pus.hus.merged$poverty.factor = as.factor(ifelse(pus.hus.merged$POVPIP<100, 'Pov', 'NoPov'))

# Education
pus.hus.merged$education.factor = as.factor(ifelse(pus.hus.merged$SCHL < 16, 'NoGrad', 'Grad'))

# Owner Occupied
pus.hus.merged$owner_occupied.factor = as.factor(ifelse(pus.hus.merged$TEN <= 2, 'Owner_occupied', 'NoOwner_occupied'))

# For sensitivity analyses:
pus.hus.merged.192 = pus.hus.merged
# Overwrite Age: 4 categories, creates 192 strata
pus.hus.merged.192$age.factor[pus.hus.merged.192$AGEP <= 17] = '0.17'
pus.hus.merged.192$age.factor[pus.hus.merged.192$AGEP >=18 & pus.hus.merged.192$AGEP <= 39] = '18.39'
pus.hus.merged.192$age.factor[pus.hus.merged.192$AGEP >=40 & pus.hus.merged.192$AGEP <= 64] = '40.64'
pus.hus.merged.192$age.factor[pus.hus.merged.192$AGEP >=65] = '65.'
pus.hus.merged.192$age.factor = factor(pus.hus.merged.192$age.factor)
  
# PUMA Equivalency files
equivalency_url = 'https://www2.census.gov/geo/docs/reference/puma/'
pg <- read_html(equivalency_url)
equivfilePaths = paste(equivalency_url, html_attr(html_nodes(pg, "a"), "href")[8:59], sep = '')
equiv <- do.call("rbind", lapply(equivfilePaths, function(x){read.delim(x, stringsAsFactor = FALSE, header = F)}))

# example: 796 11 017023820 0101 001 01702382
# format:  796 StateFIPS StateNationalStandardCode PUMAcode CountyFIPS CountyNationalStandardCode
# See https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Format_Layout.pdf for more info

# Extract 796 strings to get the fips and puma equivalencies
codes = unlist(lapply(strsplit(equiv[,1][which(startsWith(equiv[,1], '796') == T)], "\\s+"), function(x){x[1]}))
# Extract PUMAs
pumas = as.numeric(substring(codes, 14, 18))
# Extract FIPS
fips = paste(substring(codes, 4, 5), substring(codes, 19,21), sep = "")
# Dataframe matching PUMAs to FIPS
puma.fips.match = data.frame(pumas,fips)

# Function that links PUMAs to FIPS 
# Note that some PUMAs are contained within FIPS, some FIPS within PUMAs. 
# Currently link by going from FIPS to all pumas listed under that FIPS, 
# so there may be overlaps of individuals/households across different FIPS

fips.to.puma = function(fips, df){
  state = as.numeric(substring(fips, 1,2))
  puma = puma.fips.match$pumas[puma.fips.match$fips == fips]
  return(df[df$PUMA %in% puma & df$ST == state,])
}

## The following code (up to line 238) is borrowed from the 2020 analysis 
## by Xiao Wu (https://github.com/wxwx1993/PM_COVID/tree/master)

date_of_study <- "12-01-2020"
# Additional analyses 2 and 3 use date_of_study <- "06-18-2020"

# Historical data
covid_hist <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist <- subset(covid_hist, Country_Region == "US" & is.na(FIPS) == F)

# Import outcome data from JHU CSSE
covid <- read.csv(text = getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date_of_study, ".csv")))
covid_us <- subset(covid, Country_Region == "US")[, 1:12]
covid_us <- rbind(covid_us, subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS)) & Confirmed == 0 & Deaths == 0 & is.na(FIPS) == F))
covid_us$FIPS <- str_pad(covid_us$FIPS, 5, pad = "0")

# Import exposure PM2.5 data
county_pm <- read.csv(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))

county_temp <- read.csv(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv"))
# Import census, brfss, testing, mortality, hospital beds data as potential confounders
county_census <- read.csv(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
county_brfss = read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv", skip = 1)
county_brfss <- county_brfss[, c('fipscode', 'v011_rawvalue', 'v009_rawvalue')]
names(county_brfss) <- c('fips', 'obese', 'smoke')
county_brfss$fips <- str_pad(county_brfss$fips, 5, pad = "0")

hospitals <- read.csv(text = getURL("https://opendata.arcgis.com/api/v3/datasets/75079bdea94743bcaca7b6e833692639_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"))
hospitals$BEDS[hospitals$BEDS < 0] <- NA

# pm2.5 average over 17 years
county_pm_aggregated <- county_pm %>% 
  group_by(fips) %>% 
  summarise(mean_pm25 = mean(pm25))

# temperature and relative humidity average over 17 years
county_temp_aggregated <- county_temp %>% 
  group_by(fips) %>% 
  summarise(mean_winter_temp = mean(winter_tmmx),
            mean_summer_temp = mean(summer_tmmx),
            mean_winter_rm = mean(winter_rmax),
            mean_summer_rm = mean(summer_rmax))

county_pm_aggregated <- merge(county_pm_aggregated,
                              county_temp_aggregated,
                              by = "fips",
                              all.x = TRUE)

county_hospitals_aggregated <- hospitals %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS, na.rm = TRUE))
county_hospitals_aggregated$COUNTYFIPS <- str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")

county_census_aggregated2 <- subset(county_census, year == 2016)

county_census_aggregated2$q_popdensity <- 1
quantile_popdensity <- quantile(county_census_aggregated2$popdensity, c(0.2, 0.4, 0.6, 0.8))
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity <= quantile_popdensity[1]] <- 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity > quantile_popdensity[1] &
                                         county_census_aggregated2$popdensity <= quantile_popdensity[2]] <- 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity > quantile_popdensity[2] &
                                         county_census_aggregated2$popdensity <= quantile_popdensity[3]] <- 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity > quantile_popdensity[3] &
                                         county_census_aggregated2$popdensity <= quantile_popdensity[4]] <- 4
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity > quantile_popdensity[4]] <- 5

county_census_aggregated2$fips <- str_pad(county_census_aggregated2$fips, 5, pad = "0")
county_census_aggregated2 <- merge(county_census_aggregated2,county_brfss,
                                   by = "fips",
                                   all.x = TRUE)

county_pm_aggregated$fips <- str_pad(county_pm_aggregated$fips, 5, pad = "0")
aggregate_pm <- merge(county_pm_aggregated,covid_us,
                      by.x = "fips",
                      by.y = "FIPS")

aggregate_pm_census <- merge(aggregate_pm,
                             county_census_aggregated2,
                             by.x = "fips",
                             by.y = "fips")


aggregate_pm_census_cdc <- aggregate_pm_census[is.na(aggregate_pm_census$fips) == F, ]

aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc,county_hospitals_aggregated,
                                           by.x = "fips",
                                           by.y = "COUNTYFIPS",
                                           all.x = TRUE)
aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] <- 0

# Combine five boroughs of NYC
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City", ]$population <-
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City", ]$beds <-
  subset(aggregate_pm_census_cdc_test_beds,Admin2 == "New York City" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$beds

vars <- c("mean_pm25", "obese", "smoke","mean_summer_temp", "mean_summer_rm", "mean_winter_temp", "mean_winter_rm")
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City",][, vars] <-
  sapply(vars, function(var) {
    (subset(aggregate_pm_census_cdc_test_beds, Admin2=="New York City" & Province_State=="New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")[, var] * 
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")[, var] * 
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population) / (
         subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population + 
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population + 
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population)
  }
  )
aggregate_pm_census_cdc_test_beds <- subset(aggregate_pm_census_cdc_test_beds,
                                            !(Admin2 == "Bronx" & Province_State == "New York") &
                                              !(Admin2 == "Kings" & Province_State == "New York") &
                                              !(Admin2 == "Queens" & Province_State == "New York") &
                                              !(Admin2 == "Richmond" & Province_State == "New York"))

# Merge the previous dataframe with historical averages of O3 and NO2
data<-read.csv('data/county_no2_ozone.csv')
covid_data = merge(cbind.data.frame(fips = data$fips, 
                                    state = data$state, 
                                    mean_no2 = data$mean_no2, 
                                    mean_ozone = data$mean_ozone),
                   cbind(fips = as.numeric(aggregate_pm_census_cdc_test_beds$fips), 
                         Deaths = aggregate_pm_census_cdc_test_beds$Deaths, 
                         population = aggregate_pm_census_cdc_test_beds$population, 
                         mean_pm25 = aggregate_pm_census_cdc_test_beds$mean_pm25, 
                         q_popdensity = aggregate_pm_census_cdc_test_beds$q_popdensity, 
                          beds = aggregate_pm_census_cdc_test_beds$beds, 
                          obese = aggregate_pm_census_cdc_test_beds$obese, 
                         smoke = aggregate_pm_census_cdc_test_beds$smoke, 
                         mean_summer_temp = aggregate_pm_census_cdc_test_beds$mean_summer_temp, 
                         mean_winter_temp= aggregate_pm_census_cdc_test_beds$mean_winter_temp,
                         mean_summer_rm = aggregate_pm_census_cdc_test_beds$mean_summer_rm, 
                         mean_winter_rm = aggregate_pm_census_cdc_test_beds$mean_winter_rm), by = 'fips')

# Read in census-tract level PM2.5. Source: Randall Martin.
censustract.pm25 = read.csv('data/census_tract_pm25_2018.csv')
censustract.pm25$geoidstr = str_pad(censustract.pm25$geoid, 11, pad = '0')
censustract.pm25$fips = paste(substring(censustract.pm25$geoidstr, 1, 2), 
                              substring(censustract.pm25$geoidstr, 3, 5), sep = "")

# read in census 2018 data which has population by census tract 
census2018 = read.csv('data/census2018.csv')

# merge census-tract level PM2.5 with population data from census2018
censustract.pm25 = merge(censustract.pm25, 
                         cbind(geoid = census2018$geoid, population = census2018$population), 
                         by = 'geoid', all.x = T)

# weighted variance calculations of PM2.5 in each county
weightedvarlist.pm25 = sapply(split(censustract.pm25, censustract.pm25$fips), 
                              function(x){wtd.var(x$pm25, x$population)})

# Merge weighted pm25 variance list with previous dataset to ensure same order of FIPS
covid_data = merge(as.data.frame(cbind(fips = as.numeric(names(weightedvarlist.pm25)), weightedvarlist.pm25)), 
                   covid_data, by = 'fips')

# FINANCIAL VARIABLES : extract variance and means

hus$HINCP[which(hus$HINCP <= 0)] = NA 
# remove negative incomes for simplicity. These are only 1 percent of households

# Create list of 2x2 covariance matrices for log household income and log house value
covlist = vector(mode = "list", length = length(covid_data$fips))
## Create list of matrices, length of fips (each list item is the 2 x 2 covariance matrix for that fips)
for (i in 1:length(covid_data$fips)){
  fip = str_pad(covid_data$fips[i], 5, pad = '0')
  fipdat = fips.to.puma(fip, hus) # extract housing PUMS data for that fips
  data = na.omit(as.data.frame(cbind(HINCP = fipdat$HINCP, # household income
                                     ADJINC = fipdat$ADJINC, # income inflation factor
                                     VALP = fipdat$VALP, # house value
                                     WGTP = fipdat$WGTP))) # household weight
  # weighted mean and covariance using weights provided in housing PUMS files
  covmat = cov.wt(x = cbind(log((data$ADJINC/1000000)*(data$HINCP)), log(data$VALP)), wt = data$WGTP) 
  covlist[[i]] = covmat$cov
  covid_data$loghouseholdincome[i] = covmat$center[1]
  covid_data$loghousevalue[i] = covmat$center[2]
}

# Create list of 3 x 3 covariance matrices for log household income, log house value, PM2.5 
# (assume PM2.5 is uncorrelated with the previous two)
covlist.pm25 = vector(mode = "list", length = length(covid_data$fips))
for (i in 1:length(covlist)){
  covlist.pm25[[i]] = matrix(0, nrow = 3, ncol = 3)
  covlist.pm25[[i]][1:2,1:2] = covlist[[i]]
  covlist.pm25[[i]][3,3] = covid_data$weightedvarlist.pm25[i]
}

# Create joint distribution of age, sex, race, poverty, education, owner occupied in a matrix called CROSS (dimensions 3082 x # strata)
# Main analysis uses 96 strata (2 for poverty x 2 for education x 2 for owner occupied x 2 for age x 2 for sex x 3 for race)
# check that number of levels in age in pus.hus.merged matches otherwise will return error.

combs = expand.grid(poverty = c('NoPov', 'Pov'), 
                    education = c('NoGrad', 'Grad'),  
                    owner_occupied = c('NoOwner_occupied', 'Owner_occupied'), 
                    age = c('0.39', '40.'), 
                    sex = c('Male', 'Female'), 
                    race = c('White', 'Black.AfricanAmerican', 'OtherRace'))

cross = data.frame(matrix(NA, nrow = length(covid_data$fips), ncol = nrow(combs)))
colnames(cross) = do.call('paste', combs)
colnames(cross) = paste('Stratum:', colnames(cross)) 

# each column of cross denotes a unique combination of variables above, 
# eg poverty, grad, owner_occupied, 0.39, female, white

for (j in 1:length(covid_data$fips)){
  data = fips.to.puma(str_pad(covid_data$fips[j], 5, pad = '0'), pus.hus.merged)
  strata.prop = rep(NA, nrow(combs))
  for (i in 1:nrow(combs)){ # Sensitivity 
    # Account for weights with PWGTP
    strata.prop[i] = sum(data$PWGTP[data$age.factor == combs[i,]$age 
                                    & data$sex.factor == combs[i,]$sex 
                                    & data$race.factor == combs[i,]$race 
                                    & data$poverty.factor == combs[i,]$poverty 
                                    & data$education.factor == combs[i,]$education 
                                    & data$owner_occupied.factor == combs[i,]$owner_occupied], na.rm = T)
  }
  cross[j,] = strata.prop/sum(strata.prop) 
  # cell j,i in cross denotes the estimated proportion of people in stratum i in county j
}

# For Sensitivity Analyses: 
combs192 = expand.grid(poverty = c('NoPov', 'Pov'), 
                    education = c('NoGrad', 'Grad'),  
                    owner_occupied = c('NoOwner_occupied', 'Owner_occupied'), 
                    age = c('0.17', '18.39', '40.64', '65.'), 
                    sex = c('Male', 'Female'), 
                    race = c('White', 'Black.AfricanAmerican', 'OtherRace'))

cross192 = data.frame(matrix(NA, nrow = length(covid_data$fips), ncol = nrow(combs192)))
colnames(cross192) = do.call('paste', combs192)
colnames(cross192) = paste('Stratum:', colnames(cross192)) 

# each column of cross denotes a unique combination of variables above, 
# eg poverty, grad, owner_occupied, 18.39, female, white

for (j in 1:length(covid_data$fips)){
  data = fips.to.puma(str_pad(covid_data$fips[j], 5, pad = '0'), pus.hus.merged.192)
  strata.prop = rep(NA, nrow(combs192))
  for (i in 1:nrow(combs192)){ # Sensitivity 
    # Account for weights with PWGTP
    strata.prop[i] = sum(data$PWGTP[data$age.factor == combs192[i,]$age 
                                    & data$sex.factor == combs192[i,]$sex 
                                    & data$race.factor == combs192[i,]$race 
                                    & data$poverty.factor == combs192[i,]$poverty 
                                    & data$education.factor == combs192[i,]$education 
                                    & data$owner_occupied.factor == combs192[i,]$owner_occupied], na.rm = T)
  }
  cross192[j,] = strata.prop/sum(strata.prop) 
  # cell j,i in cross denotes the estimated proportion of people in stratum i in county j
}


# Convert population density from a 5 level factor into 4 individual indicators.
qpop = matrix(0, nrow = length(covid_data$q_popdensity), ncol = length(levels(factor(covid_data$q_popdensity)))) 
for (i in 1:nrow(qpop)){
  qpop[i,covid_data$q_popdensity[i]]=1
}
colnames(qpop) = c('qpopdensity 1', 'qpopdensity 2', 'qpopdensity 3', 'qpopdensity 4', 'qpopdensity 5')

# Create numeric states
states = as.numeric(as.factor(covid_data$state))

# Create whicha: matrix of 0s and 1s (design matrix for factor-level vars)
# dimension nrow(combs) x (number of categorical levels - number of categorical vars)
createwhicha = function(cats){ # some lines of this code are borrowed from Jackson's ecoreg package.
  aoff = cumsum(cats - 1) 
  aoff <- c(0, aoff[-length(aoff)])
  combs <- as.matrix(expand.grid(lapply(cats,
                                        function(x)(seq(length=x)))))  
  # matrix with one row for each cross-class category
  
  ### We need a reference category, so subtract one
  combs <- combs - 1
  ### Zero-th elements don't get picked, set to NA
  combs <- replace(combs, combs == 0, NA)
  whichalist <-  lapply(split(combs, 1:nrow(combs)), function(x) aoff + x) # creates list 
  whicha = matrix(0, nrow = length(whichalist), ncol = sum(cats)-length(cats)) # change ncol
  for (i in 1:length(whichalist)){ # turn list in a matrix of 0s and 1s.
    whicha[i,whichalist[[i]]]=1
  }
  return(whicha)
}

# number of levels of poverty, education, owner occupied, age, sex, race
whicha = createwhicha(c(2,2,2,2,2,3))
whicha192 = createwhicha(c(2,2,2,4,2,3))

############################################################################################
# COMPILE DATASETS

# Main Analysis: creates dat_main
adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
                                   N = covid_data$population, 
                                   # county-level data:
                                  mean_no2 = covid_data$mean_no2, 
                                  mean_ozone = covid_data$mean_ozone, 
                                  qpop[,2:5], 
                                  beds.pop = scale(covid_data$beds/covid_data$population), 
                                  mean_summer_temp = scale(covid_data$mean_summer_temp), 
                                  mean_winter_temp = scale(covid_data$mean_winter_temp), 
                                  mean_summer_rm = scale(covid_data$mean_summer_rm), 
                                  mean_winter_rm = scale(covid_data$mean_winter_rm), 
                                  obese = scale(covid_data$obese), 
                                  smoke = scale(covid_data$smoke), 
                                  # joint distribution of the categorical covariates within each county:
                                  cross96, 
                                  # continuous covariates:
                                  loghouseholdincome = covid_data$loghouseholdincome, 
                                  loghousevalue = covid_data$loghousevalue, 
                                  mean_pm25 = covid_data$mean_pm25)) 

## Assemble data needed for main model running (see RCCluster)
# dataframe, variances, states, matrix corresponding to categorical effects
save(adata, covlist.pm25, states, whicha, file = 'dat_main.RData')

############################################################################################

# Sensitivity Analyses 1-7: Creates dat_1, ..., dat_7

## Evaluate strata-specific risks
strata <- adata[,grep("Stratum", colnames(adata))] # extract strata from dataset
e <- rep(0, ncol(strata))
for (i in 1:ncol(strata)) { # loop over strata
  dat <- as.data.frame(adata[,"N"] * strata[,i]) # generate num people w/in strata
  mod <- glm(adata[,"y"] ~ ., family = poisson(), data = dat) # regress on number of deaths
  e[i] <- exp(coef(mod)[2]) - 1 # exponentiate coefficient and subtract 1 to get estimated risk parameter
}
## use log(e/(1-e)) as gamma_s for each strata
gamma_s = log(e/(1-e)) 
gamma_s[is.na(gamma_s)] <- 0

save(adata, covlist.pm25, states, gamma_s, whicha, file = 'dat_1.RData')

adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
                                           N = covid_data$population, 
                                           # county-level data:
                                           mean_no2 = covid_data$mean_no2, 
                                           mean_ozone = covid_data$mean_ozone, 
                                           qpop[,2:5], 
                                           beds.pop = scale(covid_data$beds/covid_data$population), 
                                           mean_summer_temp = scale(covid_data$mean_summer_temp), 
                                           mean_winter_temp = scale(covid_data$mean_winter_temp), 
                                           mean_summer_rm = scale(covid_data$mean_summer_rm), 
                                           mean_winter_rm = scale(covid_data$mean_winter_rm), 
                                           obese = scale(covid_data$obese), 
                                           smoke = scale(covid_data$smoke), 
                                           # joint distribution of the categorical covariates within each county:
                                           cross192, 
                                           # continuous covariates:
                                           loghouseholdincome = covid_data$loghouseholdincome, 
                                           loghousevalue = covid_data$loghousevalue, 
                                           mean_pm25 = covid_data$mean_pm25)) 

save(adata, covlist.pm25, states, whicha192, file = 'dat_2.RData')

## Evaluate strata-specific risks
strata <- adata[,grep("Stratum", colnames(adata))] # extract strata from dataset
e <- rep(0, ncol(strata))
for (i in 1:ncol(strata)) { # loop over strata
  dat <- as.data.frame(adata[,"N"] * strata[,i]) # generate num people w/in strata
  mod <- glm(adata[,"y"] ~ ., family = poisson(), data = dat) # regress on number of deaths
  e[i] <- exp(coef(mod)[2]) - 1 # exponentiate coefficient and subtract 1 to get estimated risk parameter
}
## use log(e/(1-e)) as gamma_s for each strata
gamma_s = log(e/(1-e)) 
gamma_s[is.na(gamma_s)] <- 0

save(adata, covlist.pm25, states, gamma_s, whicha192, file = 'dat_3.RData')

adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
                                           N = covid_data$population, 
                                           # county-level data:
                                           mean_pm25 = covid_data$mean_pm25,
                                           mean_no2 = covid_data$mean_no2, 
                                           mean_ozone = covid_data$mean_ozone, 
                                           qpop[,2:5], 
                                           beds.pop = scale(covid_data$beds/covid_data$population), 
                                           mean_summer_temp = scale(covid_data$mean_summer_temp), 
                                           mean_winter_temp = scale(covid_data$mean_winter_temp), 
                                           mean_summer_rm = scale(covid_data$mean_summer_rm), 
                                           mean_winter_rm = scale(covid_data$mean_winter_rm), 
                                           obese = scale(covid_data$obese), 
                                           smoke = scale(covid_data$smoke), 
                                           # joint distribution of the categorical covariates within each county:
                                           cross96, 
                                           # continuous covariates:
                                           loghouseholdincome = covid_data$loghouseholdincome, 
                                           loghousevalue = covid_data$loghousevalue
                                           )) 

save(adata, covlist, states, whicha, file = 'dat_4.RData')

## Evaluate strata-specific risks
strata <- adata[,grep("Stratum", colnames(adata))] # extract strata from dataset
e <- rep(0, ncol(strata))
for (i in 1:ncol(strata)) { # loop over strata
  dat <- as.data.frame(adata[,"N"] * strata[,i]) # generate num people w/in strata
  mod <- glm(adata[,"y"] ~ ., family = poisson(), data = dat) # regress on number of deaths
  e[i] <- exp(coef(mod)[2]) - 1 # exponentiate coefficient and subtract 1 to get estimated risk parameter
}
## use log(e/(1-e)) as gamma_s for each strata
gamma_s = log(e/(1-e)) 
gamma_s[is.na(gamma_s)] <- 0

save(adata, covlist, states, whicha, gamma_s, file = 'dat_5.RData')

adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
                                      N = covid_data$population, 
                                      # county-level data:
                                      mean_pm25 = covid_data$mean_pm25,
                                      mean_no2 = covid_data$mean_no2, 
                                      mean_ozone = covid_data$mean_ozone, 
                                      qpop[,2:5], 
                                      beds.pop = scale(covid_data$beds/covid_data$population), 
                                      mean_summer_temp = scale(covid_data$mean_summer_temp), 
                                      mean_winter_temp = scale(covid_data$mean_winter_temp), 
                                      mean_summer_rm = scale(covid_data$mean_summer_rm), 
                                      mean_winter_rm = scale(covid_data$mean_winter_rm), 
                                      obese = scale(covid_data$obese), 
                                      smoke = scale(covid_data$smoke), 
                                      # joint distribution of the categorical covariates within each county:
                                      cross192, 
                                      # continuous covariates:
                                      loghouseholdincome = covid_data$loghouseholdincome, 
                                      loghousevalue = covid_data$loghousevalue
)) 

save(adata, covlist, states, whicha192, file = 'dat_6.RData')

## Evaluate strata-specific risks
strata <- adata[,grep("Stratum", colnames(adata))] # extract strata from dataset
e <- rep(0, ncol(strata))
for (i in 1:ncol(strata)) { # loop over strata
  dat <- as.data.frame(adata[,"N"] * strata[,i]) # generate num people w/in strata
  mod <- glm(adata[,"y"] ~ ., family = poisson(), data = dat) # regress on number of deaths
  e[i] <- exp(coef(mod)[2]) - 1 # exponentiate coefficient and subtract 1 to get estimated risk parameter
}
## use log(e/(1-e)) as gamma_s for each strata
gamma_s = log(e/(1-e)) 
gamma_s[is.na(gamma_s)] <- 0

save(adata, covlist, states, whicha192, gamma_s, file = 'dat_7.RData')

############################################################################################
# ADDITIONAL ANALYSES

# # Additional Analysis 1
# merge(cbind.data.frame(y = covid_data$Deaths, 
#                        N = covid_data$population, 
#                        mean_pm25 = covid_data$mean_pm25), 
#       cbind.data.frame(factor(aggregate_pm_census_cdc_test_beds$q_popdensity), 
#                        scale(aggregate_pm_census_cdc_test_beds$poverty),
#                        scale(log(aggregate_pm_census_cdc_test_beds$medianhousevalue)),
#                        scale(log(aggregate_pm_census_cdc_test_beds$medhouseholdincome)),
#                        scale(aggregate_pm_census_cdc_test_beds$pct_owner_occ),
#                        scale(aggregate_pm_census_cdc_test_beds$education),
#                        scale(aggregate_pm_census_cdc_test_beds$pct_blk),
#                        scale(aggregate_pm_census_cdc_test_beds$hispanic),
#                        scale(aggregate_pm_census_cdc_test_beds$)
# adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
#                                    N = covid_data$population, 
#                                    mean_pm25 = covid_data$mean_pm25,
#                                    qpop[,2:5], 
#                                    poverty = scale()
#                                    # county-level data:
#                                    mean_no2 = covid_data$mean_no2, 
#                                    mean_ozone = covid_data$mean_ozone, 
#                                    beds.pop = scale(covid_data$beds/covid_data$population), 
#                                    mean_summer_temp = scale(covid_data$mean_summer_temp), 
#                                    mean_winter_temp = scale(covid_data$mean_winter_temp), 
#                                    mean_summer_rm = scale(covid_data$mean_summer_rm), 
#                                    mean_winter_rm = scale(covid_data$mean_winter_rm), 
#                                    obese = scale(covid_data$obese), 
#                                    smoke = scale(covid_data$smoke), 
#                                    # continuous covariates:
#                                    loghouseholdincome = covid_data$loghouseholdincome, 
#                                    loghousevalue = covid_data$loghousevalue, 
#                                    )) 
# 
# # Additional analyses 2 and 3 use date_of_study <- "06-18-2020"
# date_of_study <- "06-18-2020"
# 
# # Import outcome data from JHU CSSE
# covid <- read.csv(text = getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date_of_study, ".csv")))
# covid_us <- subset(covid, Country_Region == "US")[, 1:12]
# covid_us <- rbind(covid_us, subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS)) & Confirmed == 0 & Deaths == 0 & is.na(FIPS) == F))
# covid_us$FIPS <- str_pad(covid_us$FIPS, 5, pad = "0")
# 
# adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, 
#                                    N = covid_data$population, 
#                                    # county-level data:
#                                    mean_no2 = covid_data$mean_no2, 
#                                    mean_ozone = covid_data$mean_ozone, 
#                                    qpop[,2:5], 
#                                    beds.pop = scale(covid_data$beds/covid_data$population), 
#                                    mean_summer_temp = scale(covid_data$mean_summer_temp), 
#                                    mean_winter_temp = scale(covid_data$mean_winter_temp), 
#                                    mean_summer_rm = scale(covid_data$mean_summer_rm), 
#                                    mean_winter_rm = scale(covid_data$mean_winter_rm), 
#                                    obese = scale(covid_data$obese), 
#                                    smoke = scale(covid_data$smoke), 
#                                    # continuous covariates:
#                                    loghouseholdincome = covid_data$loghouseholdincome, 
#                                    loghousevalue = covid_data$loghousevalue, 
#                                    mean_pm25 = covid_data$mean_pm25)) 
# 
# ## Assemble data needed for main model running (see RCCluster)
# # dataframe, variances, states, matrix corresponding to categorical effects
# save(adata, covlist.pm25, states, whicha, file = 'dat_main.RData')
# 

############################################################################################

## Characteristics of the study cohort (code for Table 3)
# Census tract-level data
weighted.mean(x = censustract.pm25$pm25, w= censustract.pm25$population, na.rm = T)
sqrt(wtd.var(x = censustract.pm25$pm25, w= censustract.pm25$population))

# County-level data
mean(covid_data$Deaths/(covid_data$population/100000));sd(covid_data$Deaths/(covid_data$population/100000))
mean(covid_data$mean_no2); sd(covid_data$mean_no2)
mean(covid_data$mean_ozone); sd(covid_data$mean_ozone)
mean(covid_data$mean_summer_temp-273.15); sd(covid_data$mean_summer_temp)
mean(covid_data$mean_winter_temp-273.15); sd(covid_data$mean_winter_temp)
mean(covid_data$mean_summer_rm); sd(covid_data$mean_summer_rm)
mean(covid_data$mean_winter_rm); sd(covid_data$mean_winter_rm)
mean(covid_data$beds/(covid_data$population/100000)); sd(covid_data$beds/(covid_data$population/100000))
mean(100*covid_data$obese); sd(100*covid_data$obese)
mean(100*covid_data$smoke); sd(100*covid_data$smoke)

# Individual level data
sum(pus$PWGTP[pus$POVPIP < 100], na.rm = T)/sum(pus$PWGTP[!is.na(pus$POVPIP)])
sum(pus$PWGTP[pus$SCHL >= 16], na.rm = T)/sum(pus$PWGTP[!is.na(pus$SCHL)])
sum(hus$WGTP[hus$TEN <= 2], na.rm = T)/sum(hus$WGTP[!is.na(hus$TEN)]) # owner occupied, not owner occupied
sum(pus$PWGTP[pus$AGEP >= 40], na.rm = T)/sum(pus$PWGTP[!is.na(pus$AGEP)])
sum(pus$PWGTP[pus$SEX == 2])/sum(pus$PWGTP[!is.na(pus$SEX)])
sum(pus$PWGTP[pus$RAC1P == 1], na.rm = T)/sum(pus$PWGTP[!is.na(pus$RAC1P)])
sum(pus$PWGTP[pus$RAC1P == 2], na.rm = T)/sum(pus$PWGTP[!is.na(pus$RAC1P)])
sum(pus$PWGTP[pus$RAC1P %in% c("3","4","5","6","7","8","9")], na.rm = T)/sum(pus$PWGTP[!is.na(pus$RAC1P)])
weighted.mean(x = (hus$ADJINC/1000000)*hus$HINCP/1000, w = hus$WGTP, na.rm = T)
sqrt(wtd.var(x = (hus$ADJINC/1000000)*hus$HINCP/1000, w = hus$WGTP))
weighted.mean(x = hus$VALP/1000, w = hus$WGTP, na.rm = T)
sqrt(wtd.var(x = hus$VALP/1000, w = hus$WGTP))


        