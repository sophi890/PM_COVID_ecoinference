# Combining aggregate and individual-level data to estimate individual-level associations between air pollution and COVID-19 mortality in the United States

This is the data repository for public available code and data to reproduce analyses in S.M. Woodward\*, D. Mork\*, X. Wu, Z. Hou, D. Braun, F. Dominici, 2022. Combining aggregate and individual-level data to estimate individual-level associations between air pollution and COVID-19 mortality in the United States. 

See [here](https://github.com/wxwx1993/PM_COVID/tree/master) for the previous analysis, Wu, X., Nethery, R. C., Sabath, M. B., Braun, D. and Dominici, F., 2020. Air pollution and COVID-19 mortality in the United States: Strengths and limitations of an ecological regression analysis. Science advances, 6(45), p.eabd4049.

## Code

 - **Preprocessing.R** includes the code to extract all necessary data and preprocess data for statistical analyses.
 - **hierbayes_main.stan** is the stan file for the main analysis. Stan files for sensitivity analyses can be found in the folder **sensitivity**.
 - **RCCluster** contains R files and shell files for running all models on the FAS RC cluster. In particular, **fit_main.R** contained within includes code to run the main model.
 - **print_results.R** includes the code to print OR estimates and confidence intervals in the Main Text.
 - **Figure.R** includes the code to generate figures in the Main Text.
 
## Data

 * Access [data and results](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2F3ZU0AS&) on Harvard Dataverse.
 * **Public Use Microdata 5-yr Sample 2015-2019**, as well as equivalency files. These are a set of anonymized records of individuals and housing units, identified geographically by Public Use Microdata Areas (PUMAs). See
    - [data,](https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/)
    - [equivalency file 1](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Format_Layout.pdf)
    - [equivalency file 2](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Summary_Levels.pdf)
 
## Thank you

We thank Randall Martin and the members of the Atmospheric Composition Analysis Group at Dalhousie University for providing access to their open-source datasets. Their data (V4.NA.02.MAPLE) that we used can be found here: [https://sites.wustl.edu/acag/datasets/surface-pm2-5/](https://sites.wustl.edu/acag/datasets/surface-pm2-5/). 

Citation: 
van Donkelaar, A., R. V. Martin, C. Li, R. T. Burnett, Regional Estimates of Chemical Composition of Fine Particulate Matter using a Combined Geoscience-Statistical Method with Information from Satellites, Models, and Monitors, Environ. Sci. Technol., doi: 10.1021/acs.est.8b06392, 2019.

We thank Qian Di and collaborators for providing us with 1km x 1km estimates of NO<sub>2</sub>, and Weeberb J Requia and collaborators for providing us with 1km x 1km estimates of O<sub>3</sub>.

Citations: 

 * Q. Di, H. Amini, L. Shi, I. Kloog, R. Silvern, J. Kelly, M. B. Sabath, C. Choirat,P. Koutrakis, A. Lyapustin,et al., Assessing no2 concentration and model uncertainty with high spatiotemporal resolution across the contiguous united states using ensemble model averaging. Environmental science & technology 54, 1372–1384 (2019)
 * W. J. Requia, Q. Di, R. Silvern, J. T. Kelly, P. Koutrakis, L. J. Mickley, M. P. Sulprizio, H. Amini, L. Shi, J. Schwartz, An ensemble learning approach for estimating high spatiotemporal resolution of ground-level ozone in the contiguous united states.Environmental science & technology54, 11037–11047 (2020).
 
The seasonal temperature and relative humidity data can be created via 4km × 4km temperature and relative humidity predictions from Gridmet via google earth engine [https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET](https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET).

We thank John Abatzoglou and members of the Climatology Lab at University of Idaho for providing the GRIDMET open-source datasets.

Additional data required by the analyses can be directly extracted from data sources:

 * Johns Hopkins University the Center for Systems Science and Engineering (CSSE) Coronavirus Resource Center: [https://coronavirus.jhu.edu/](https://coronavirus.jhu.edu/)
 * Homeland Infrastructure Foundation-Level Data (HIFLD): [https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals](https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals)
* Robert Wood Johnson Foundation County Health Rankings: [https://www.countyhealthrankings.org/](https://www.countyhealthrankings.org/)

We thank all of them for making their data public and for enabling this research to be possible.

## Contact us

 * Email: [fdominic@hsph.harvard.edu](mailto:fdominic@hsph.harvard.edu)
 
## Terms of Use
Anyone who wishes to share, reuse, remix, or adapt this material must obtain permission from the corresponding author.

This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Combining aggregate and individual-level data to estimate individual-level associations between air pollution and COVID-19 mortality in the United States." The analyses rely upon publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.
