library(dplyr)
library(stringr)
library(RCurl)
library(httr)
library(gdata)
library(Hmisc)

# Read in household PUMS
temp = tempfile()
download.file('https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/csv_pus.zip', temp)
husa = read.csv(unz(temp, 'psam_husa.csv'))
unlink(temp)

husa = read.csv('data/psam_husa.csv')
husb = read.csv('data/psam_husb.csv')
husc = read.csv('data/psam_husc.csv')
husd = read.csv('data/psam_husd.csv')
hus = rbind(husa, husb, husc, husd)

# Read in person PUMS
pusa = read.csv('data/psam_pusa.csv')
pusb = read.csv('data/psam_pusb.csv')
pusc = read.csv('data/psam_pusc.csv')
pusd = read.csv('data/psam_pusd.csv')
pus = rbind(pusa, pusb, pusc, pusd)

# Merge
pus.hus.merged = merge(pus[,c('PUMA', 'ST', 'SERIALNO', 'AGEP', 'SEX', 'RAC1P', 'POVPIP', 'SCHL', 'PWGTP')], hus[,c('PUMA', 'ST', 'SERIALNO', 'TEN', 'WGTP')], by = c('PUMA', 'ST', 'SERIALNO'), all.x = T)

# Age: 4 categories, creates 192 strata
#pus.hus.merged$age.factor[pus.hus.merged$AGEP <= 17] = '0.17'
#pus.hus.merged$age.factor[pus.hus.merged$AGEP >=18 & pus.hus.merged$AGEP <= 39] = '18.39'
#pus.hus.merged$age.factor[pus.hus.merged$AGEP >=40 & pus.hus.merged$AGEP <= 64] = '40.64'
#pus.hus.merged$age.factor[pus.hus.merged$AGEP >=65] = '65.'
#pus.hus.merged$age.factor = factor(pus.hus.merged$age.factor)

# Alternative Age, creates 96 strata
pus.hus.merged$age.factor[pus.hus.merged$AGEP <= 39] = '0.39'
pus.hus.merged$age.factor[pus.hus.merged$AGEP >=40] = '40.'
pus.hus.merged$age.factor = factor(pus.hus.merged$age.factor)

# Sex
pus.hus.merged$sex.factor = factor(pus.hus.merged$SEX)
levels(pus.hus.merged$sex.factor) = c('Male', 'Female')

# Race
pus.hus.merged$race.factor = factor(pus.hus.merged$RAC1P)
levels(pus.hus.merged$race.factor) <- list("White"=c("1"), "Black.AfricanAmerican"=c("2"), "OtherRace"=c("3","4","5","6","7","8","9"))

# Poverty
pus.hus.merged$poverty.factor = as.factor(ifelse(pus.hus.merged$POVPIP<100, 'Pov', 'NoPov'))

# Education
pus.hus.merged$education.factor = as.factor(ifelse(pus.hus.merged$SCHL < 16, 'NoGrad', 'Grad'))

# Owner Occupied
pus.hus.merged$owner_occupied.factor = as.factor(ifelse(pus.hus.merged$TEN <= 2, 'Owner_occupied', 'NoOwner_occupied'))


# Equivalency files

equivfilePaths <- list.files('data/pums_equivalencies', "\\.txt$", full.names = TRUE)
equiv <- do.call("rbind", lapply(equivfilePaths, function(x){read.delim(x, stringsAsFactor = FALSE, header = F)}))

# example: 796 34 017797950 0101 001 00882270  
# example: 796 01 017797750 0100 033 00161542
# example: 796 11 017023820 0101 001 01702382
# format:  796 StateFIPS StateNationalStandardCode PUMAcode CountyFIPS CountyNationalStandardCode

# Extract 796 strings
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


## The following code (up to line 326) is copied from the 2020 analysis by Xiao Wu (https://github.com/wxwx1993/PM_COVID/tree/master)

date_of_study <- "12-01-2020"
# Historical data
covid_hist <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist <- subset(covid_hist, Country_Region == "US" & is.na(FIPS) == F)

# Import outcome data from JHU CSSE
covid <- read.csv(text = getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date_of_study, ".csv")))
covid_us <- subset(covid, Country.Region == "US") # change here country_region and [,1:12]
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

state_test <- read.csv(text = getURL("https://api.covidtracking.com/v1/states/daily.csv"))
state_test <- subset(state_test, date == paste0(substring(str_remove_all(date_of_study, "-"), 5, 8),substring(str_remove_all(date_of_study, "-"), 1, 4)))[, - 38]
statecode <- read.csv(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))

hospitals <- read.csv(text = getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] <- NA

county_base_mortality <- read.table(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "", header = TRUE)
county_old_mortality <- read.table(text = getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "", header = TRUE)
county_014_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_014_mortality.txt", sep = "", header = TRUE)
county_1544_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_1544_mortality.txt", sep = "", header = TRUE)
county_4564_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_4564_mortality.txt", sep = "", header = TRUE)

colnames(county_old_mortality)[4] <- c("older_Population")
colnames(county_014_mortality)[4] <- c("014_Population")
colnames(county_1544_mortality)[4] <- c("1544_Population")
colnames(county_4564_mortality)[4] <- c("4564_Population")

county_base_mortality <- merge(county_base_mortality,county_old_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_014_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_1544_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_4564_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)

county_base_mortality$older_pecent <- county_base_mortality$older_Population / county_base_mortality$Population
county_base_mortality$"young_pecent" <- county_base_mortality$"014_Population" / county_base_mortality$Population
county_base_mortality$"prime_pecent" <- county_base_mortality$"1544_Population" / county_base_mortality$Population
county_base_mortality$"mid_pecent" <- county_base_mortality$"4564_Population" / county_base_mortality$Population
county_base_mortality$"older_pecent"[is.na(county_base_mortality$"older_pecent")] <- 0
county_base_mortality$"prime_pecent"[is.na(county_base_mortality$"prime_pecent")] <- 0
county_base_mortality$"mid_pecent"[is.na(county_base_mortality$"mid_pecent")] <- 0
county_base_mortality$"young_pecent"[is.na(county_base_mortality$"young_pecent")] <- 0

# Import NCHS Urban-Rural Classification Scheme for Counties
NCHSURCodes2013 <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/NCHSURCodes2013.csv")
NCHSURCodes2013$FIPS <- str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")

# Import FB survey on covid-like sympton data
script <- getURL("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/main/src/client/delphi_epidata.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Import social distancing measure data
state_policy <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/state_policy0410.csv")
colnames(state_policy)[6] <- "stay_at_home"

# merging data
state_test <- merge(state_test, statecode, by.x = "state" , by.y = "Code")
state_test <- merge(state_test, state_policy[, c(1, 6)], by = "State")
state_test$date_since_social <- as.numeric(as.Date(Sys.Date()) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_social) == TRUE, ]$date_since_social <- 0

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

county_base_mortality$County.Code <- str_pad(county_base_mortality$County.Code, 5, pad = "0")
aggregate_pm_census_cdc <- merge(aggregate_pm_census,
                                 county_base_mortality[, c("County.Code", 
                                                           "Population",
                                                           "older_pecent",
                                                           "young_pecent",
                                                           "prime_pecent",
                                                           "mid_pecent")],
                                 by.x = "fips",
                                 by.y = "County.Code",
                                 all.x = TRUE)

aggregate_pm_census_cdc <- aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) == F, ]

aggregate_pm_census_cdc_test <- merge(aggregate_pm_census_cdc,
                                      state_test[, !(names(state_test) %in% c("fips"))],
                                      by.x = "Province_State",
                                      by.y = "State")

aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,
                                           by.x = "fips",
                                           by.y = "COUNTYFIPS",
                                           all.x = TRUE)
aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] <- 0




aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test_beds,
                                           NCHSURCodes2013[, c(1, 7)],
                                           by.x = "fips",
                                           by.y = "FIPS", 
                                           all.x = TRUE)

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

vars <- c("mean_pm25", "poverty", "medianhousevalue", "medhouseholdincome", "pct_owner_occ",
          "education", "pct_blk", "hispanic", "older_pecent", "prime_pecent", "mid_pecent", "obese", "smoke",
          "mean_summer_temp", "mean_summer_rm", "mean_winter_temp", "mean_winter_rm")
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

# Merge the dataframe with historical averages of O3 and NO2
covid_data<-data.frame(read.csv('data/covid_data20210202_race3.csv'))
covid_data = merge(cbind.data.frame(fips = covid_data$fips, state = covid_data$State, mean_no2 = covid_data$mean_no2, mean_ozone = covid_data$mean_ozone),
                   cbind(fips = as.numeric(aggregate_pm_census_cdc_test_beds$fips), Deaths = aggregate_pm_census_cdc_test_beds$Deaths, population = aggregate_pm_census_cdc_test_beds$population, 
                         mean_pm25 = aggregate_pm_census_cdc_test_beds$mean_pm25, q_popdensity = aggregate_pm_census_cdc_test_beds$q_popdensity, 
                          beds = aggregate_pm_census_cdc_test_beds$beds, 
                          obese = aggregate_pm_census_cdc_test_beds$obese, smoke = aggregate_pm_census_cdc_test_beds$smoke, 
                         mean_summer_temp = aggregate_pm_census_cdc_test_beds$mean_summer_temp, mean_winter_temp= aggregate_pm_census_cdc_test_beds$mean_winter_temp,
                         mean_summer_rm = aggregate_pm_census_cdc_test_beds$mean_summer_rm, mean_winter_rm = aggregate_pm_census_cdc_test_beds$mean_winter_rm), by = 'fips')

censustract.pm25 = read.csv('data/census_tract_pm25_2018.csv')
censustract.pm25$geoid = str_pad(censustract.pm25$geoid, 11, pad = '0')
censustract.pm25$fips = paste(substring(censustract.pm25$geoid, 1, 2), substring(censustract.pm25$geoid, 3,5), sep = "")

# read in census 2018 data which has population by census tract
censusdata = read.csv('data/census_tract_2009_2019.csv')
census2018 = subset(censusdata, year == '2018')

# merge with population data from census2018
censustract.pm25 = merge(censustract.pm25, cbind(geoid = census2018$geoid, population = census2018$population), by = 'geoid', all.x = T)

# weighted variance calculations
weightedvarlist.pm25 = sapply(split(censustract.pm25, censustract.pm25$fips), function(x){wtd.var(x$pm25, x$population)})

# Merge weighted pm25 variance list with previous dataset to ensure same order of FIPS
covid_data = merge(as.data.frame(cbind(fips = as.numeric(names(weightedvarlist.pm25)), weightedvarlist.pm25)), covid_data, by = 'fips')

# Create list of 3 x 3 covariance matrices for log household income, log house value, PM2.5 (assume PM2.5 is uncorrelated with the previous two)
covlist.pm25 = vector(mode = "list", length = length(covid_data$fips))
for (i in 1:length(covlist)){
  covlist.pm25[[i]] = matrix(0, nrow = 3, ncol = 3)
  covlist.pm25[[i]][1:2,1:2] = covlist[[i]]
  covlist.pm25[[i]][3,3] = covid_data$weightedvarlist.pm25[i]
}
save(covlist.pm25, file = 'covlist.pm25.Rda')

# FINANCIAL VARIABLES : extract variance and means

hus$HINCP[which(hus$HINCP <= 0)] = NA # remove negative incomes. These are only 1 percent of households

## Updated with weights
covlist = vector(mode = "list", length = length(covid_data$fips))
loghouseholdincome = rep(NA, length(covid_data$fips))
loghousevalue = rep(NA, length(covid_data$fips))
## Create list of matrices, length of fips (each list item is the 2 x 2 covariance matrix for that fips)
for (i in 1:length(covid_data$fips)){
  fip = str_pad(covid_data$fips[i], 5, pad = '0')
  fipdat = fips.to.puma(fip, hus)
  data = na.omit(as.data.frame(cbind(HINCP = fipdat$HINCP, ADJINC = fipdat$ADJINC, VALP = fipdat$VALP, WGTP = fipdat$WGTP)))
  covmat = cov.wt(x = cbind(log((data$ADJINC/1000000)*(data$HINCP)), log(data$VALP)), wt = data$WGTP)# inflation factor ADJINC
  covlist[[i]] = covmat$cov
  loghouseholdincome[i] = covmat$center[1]
  loghousevalue[i] = covmat$center[2]
}

covid_data$loghouseholdincome = loghouseholdincome
covid_data$loghousevalue = loghousevalue

# Create joint distribution of age, sex, race, poverty, education, owner occupied: matrix called cross (3082 x # combs)
# 2 different options depending on how many categories age is (2 or 4, leads to 96 or 192 strata).
# check that age in pus.hus.merged matches.

# Sensitivity
pus.hus.merged$sex.factor = rbinom(nrow(pus.hus.merged),1,1/2)
pus.hus.merged$sex.factor = factor(ifelse(pus.hus.merged$sex.factor==0, 'Male', 'Female'))
#combs = expand.grid(poverty = c('NoPov', 'Pov'), education = c('NoGrad', 'Grad'),  owner_occupied = c('NoOwner_occupied', 'Owner_occupied'), age = c('0.39', '40.'), race = c('White', 'Black.AfricanAmerican', 'OtherRace'))
#combs = expand.grid(sex = c('Male', 'Female'))

combs = expand.grid(poverty = c('NoPov', 'Pov'), education = c('NoGrad', 'Grad'),  owner_occupied = c('NoOwner_occupied', 'Owner_occupied'), age = c('0.39', '40.'), sex = c('Male', 'Female'), race = c('White', 'Black.AfricanAmerican', 'OtherRace'))
#combs = expand.grid(poverty = c('NoPov', 'Pov'), education = c('NoGrad', 'Grad'),  owner_occupied = c('NoOwner_occupied', 'Owner_occupied'), age = c('0.17', '18.39', '40.64', '65.'), sex = c('Male', 'Female'), race = c('White', 'Black.AfricanAmerican', 'OtherRace'))

cross = data.frame(matrix(NA, nrow = length(covid_data$fips), ncol = nrow(combs)))
colnames(cross) = do.call('paste', combs)
colnames(cross) = paste('Stratum:', colnames(cross))
for (j in 1:length(covid_data$fips)){
  data = fips.to.puma(str_pad(covid_data$fips[j], 5, pad = '0'), pus.hus.merged)
  strata.prop = rep(NA, nrow(combs))
  for (i in 1:nrow(combs)){ # Sensitivity 
    strata.prop[i] = sum(data$PWGTP[data$age.factor == combs[i,]$age & data$sex.factor == combs[i,]$sex & data$race.factor == combs[i,]$race & data$poverty.factor == combs[i,]$poverty & data$education.factor == combs[i,]$education & data$owner_occupied.factor == combs[i,]$owner_occupied],na.rm = T)
    #strata.prop[i] = sum(data$PWGTP[data$sex.factor == combs[i,]],na.rm = T)
  }
  cross[j,] = strata.prop/sum(strata.prop)
}


# Create adata
qpop = matrix(0, nrow = length(covid_data$q_popdensity), ncol = length(levels(factor(covid_data$q_popdensity)))) # change ncol
for (i in 1:nrow(qpop)){
  qpop[i,covid_data$q_popdensity[i]]=1
}
colnames(qpop) = c('qpopdensity 1', 'qpopdensity 2', 'qpopdensity 3', 'qpopdensity 4', 'qpopdensity 5')

states = as.numeric(as.factor(covid_data$state))
adata = as.matrix(cbind.data.frame(y = covid_data$Deaths, N = covid_data$population, Intercept = rep(1, nrow(covid_data)), 
                        mean_no2 = covid_data$mean_no2, mean_ozone = covid_data$mean_ozone, 
                        qpop[,2:5], beds.pop = scale(covid_data$beds/covid_data$population), 
                        mean_summer_temp = scale(covid_data$mean_summer_temp), mean_winter_temp = scale(covid_data$mean_winter_temp), 
                        mean_summer_rm = scale(covid_data$mean_summer_rm), mean_winter_rm = scale(covid_data$mean_winter_rm), 
                        obese = scale(covid_data$obese), smoke = scale(covid_data$smoke), 
                        cross, 
                        loghouseholdincome = covid_data$loghouseholdincome, loghousevalue = covid_data$loghousevalue, 
                        mean_pm25 = covid_data$mean_pm25))

# whicha: create natrix of 0s and 1s with dimension nrow(combs) x (number of categorical levels - number of categorical vars)
createwhicha = function(cats){ # some lines of this code is borrowed from Jackson's ecoreg package.
  aoff = cumsum(cats - 1)
  aoff <- c(0, aoff[-length(aoff)])
  combs <- as.matrix(expand.grid(lapply(cats,
                                        function(x)(seq(length=x)))))  # matrix with one row for each cross-class category
  ### We need a reference category, so subtract one
  combs <- combs - 1
  ### Zero-th elements don't get picked, set to NA
  combs <- replace(combs, combs == 0, NA)
  whichalist <-  lapply(split(combs, 1:nrow(combs)), function(x) aoff + x)
  whicha = matrix(0, nrow = length(whichalist), ncol = sum(cats)-length(cats)) # change ncol
  for (i in 1:length(whichalist)){
    whicha[i,whichalist[[i]]]=1
  }
  return(whicha)
}

whicha = createwhicha(c(2,2,2,2,2,3))

# For Sensitivity Analyses: Estimate offsets for categorical variables
## Evaluate strata-specific risks

strata <- adata[,grep("Stratum", colnames(adata))] # extract strata from dataset
e <- rep(0, ncol(strata))
for (i in 1:ncol(strata)) { # loop over strata
  dat <- as.data.frame(adata[,"N"] * strata[,i]) # generate num people w/in strata
  mod <- glm(adata[,"y"] ~ ., family = poisson(), data = dat) # regress on number of deaths
  e[i] <- exp(coef(mod)[2]) - 1 # exponentiate coefficient and subtract 1 to get estimated risk parameter
}
## use log(e/(1-e)) as gamma_s for each strata
gamma_s = log(e/(1-e)) # mean -7.133298
gamma_s[is.na(gamma_s)] <- 0

## Data needed for model running
save(adata, covlist.pm25, states, gamma_s, whicha, file = 'ecoreg_main.RData')


