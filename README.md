# Ecological Bias: Air pollution and COVID-19 mortality in the United States

This is the data repository for public available code and data to reproduce analyses in S.M. Woodward\*, X. Wu\*, Z. Hou, D. Mork, D. Braun, F. Dominici, 2022. Combining information to estimate individual-level association between air pollution and COVID-19 mortality in the United States.

See [here](https://github.com/wxwx1993/PM_COVID/tree/master) for the previous analysis, Wu, X., Nethery, R. C., Sabath, M. B., Braun, D. and Dominici, F., 2020. Air pollution and COVID-19 mortality in the United States: Strengths and limitations of an ecological regression analysis. Science advances, 6(45), p.eabd4049.

## Code

 - **Preprocessing.R** includes the code to extract all necessary data and prepocess data for statistical analyses.
 - **ecoreg_random.stan** is the stan file for the main analysis. **ecoreg_random_fixedoffset.stan** is the stan file for some sensitivity analyses.
 - **RCCluster** contains R files and shell files for running all models on the FAS RC cluster. In particular, **fit_main.R** contained within includes code to run the main model. 
 - **print_results.R** includes the code to print OR estimates and confidence intervals in the Main Text and Supplementary Materials.
 - **Figure.R** includes the code to generate figures in the Main Text and Supplementary Materials.
 
## Data

 * **Public Use Microdata 5-yr Sample 2015-2019**, as well as equivalency files. These are a set of anonymized records of individuals and housing units, identified geographically by Public Use Microdata Areas (PUMAs). See
    - [data,](https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/)
    - [equivalency file 1](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Format_Layout.pdf)
    - [equivalency file 2](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Summary_Levels.pdf)
 * **county_no2_ozone.csv**: the county-level NO$_2$ and O$_3$ exposure data averaged across the period 2000-2016, averaged across grid cells within a zip code and then averaging across zip codes within a county. See *Q. Di, H. Amini, L. Shi, I. Kloog, R. Silvern, J. Kelly, M. B. Sabath, C. Choirat,P. Koutrakis, A. Lyapustin,et al., Assessing no2 concentration and model uncertainty with high spatiotemporal resolution across the contiguous united states using ensemble model averaging. Environmental science & technology 54, 1372–1384 (2019)* as well as *W. J. Requia, Q. Di, R. Silvern, J. T. Kelly, P. Koutrakis, L. J. Mickley, M. P. Sulprizio, H. Amini, L. Shi, J. Schwartz, An ensemble learning approach for estimating high spatiotemporal resolution of ground-level ozone in the contiguous united states.Environmental science & technology54, 11037–11047 (2020)*.
 * **census_tract_pm25_2018.csv**: census-tract level PM$_{2.5}$ estimates NEED MORE INFO
 * **census2018.csv**: census-tract level demographics, including population NEED MORE INFO
 * See README.md of the [2020 analysis](https://github.com/wxwx1993/PM_COVID/tree/master) for additional data sources including weather and behavioral risk factor variables.
 
## Thank you

## Terms of Use