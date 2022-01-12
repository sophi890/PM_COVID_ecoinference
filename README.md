# Ecological Bias: Air pollution and COVID-19 mortality in the United States

This is the data repository for public available code and data to reproduce analyses in S.M. Woodward^\*, X. Wu^\*, Z. Hou, D. Mork, D. Braun, F. Dominici, 2022. Combining information to estimate individual-level association between air pollution and COVID-19 mortality in the United States.

See [here](https://github.com/wxwx1993/PM_COVID/tree/master) for the previous analysis, Wu, X., Nethery, R. C., Sabath, M. B., Braun, D. and Dominici, F., 2020. Air pollution and COVID-19 mortality in the United States: Strengths and limitations of an ecological regression analysis. Science advances, 6(45), p.eabd4049.

## Code

  - **Preprocessing.R** includes the code to extract all necessary data and prepocess data for statistical analyses.
  - **ecoreg_random.stan** is the stan file for the main analysis. **ecoreg_random_fixedoffset.stan** is the stan file for some sensitivity analyses.
  - **RCCluster** contains R files and shell files for running all models on the FAS RC cluster. In particular, **fit_main.R** contained within includes code to run the main model. 
  - **print_results.R** includes the code to print OR estimates and confidence intervals in the Main Text and Supplementary Materials.
  - **Figure.R** includes the code to generate figures in the Main Text and Supplementary Materials.
  
## Data

  - Public Use Microdata 5-yr Sample 2015-2019, as well as equivalency files. See
  - [data,](https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/)
  - [equivalency file 1](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Format_Layout.pdf)
  - [equivalency file 2](https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Summary_Levels.pdf)
  - FINISH THIS