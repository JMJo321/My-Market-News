# Weekly Meeting on February 8, 2022

## What I did

### Creating a (clean) Dataframe for Reports about Cattle

+ Because several reports also include information about sheep, goats, and swine, the DF also include those categories. 
+ The DF includes data of 509 reports.

### Investigating the DF

+ Find the primary keys of the DF.
+ Understanding the relationship among date(time)-associated variables.
+ etc.

### Issues

1. Normalization (of weight and price data) is required.
   + Ranges of weights and prices are presented in the reprots. 
   + Avgerage weights and prices are also available.
2. How to visualize weekly summary data?
3. More than 15 variables are required to identify an observation.
   + A variable is useless for a commodity to distinguish an observation, while the variable is necessary for other commodity.
     (e.g., while no quality-grade-related information is available for `class == "Bullocks"`, there are 5 kinds of quality grade measures for `class == "Bred Cows"`)
   + The possible values of a variable depends on other variable's specific value.
     (e.g., For `class == "Bred Cows"`, the available quality grade measures are `Approved`, `Common`, `Cull`, `Medium`, and `Supreme`. But for `class == "Bulls"`, those are `Number 1`, `Number 2`, `Number 3`, `Replacement`, and `Utility`.)



## What I will do

- [ ] Check [Run a Custom Report](https://www.marketnews.usda.gov/mnp/ls-report-config) webpage to all of API-providing information is available form it.
- [ ] Check whether the data available from the [Custom Average Tool](https://cat.ams.usda.gov/app/main#/dashboards/58d7ca1a733656a41a000029?embed=true&l=false&r=false&h=false&t=false&volatile=true) page can be computed from the API-providing data. 
- [ ] Make sample figures for builiding a R shiny app. 