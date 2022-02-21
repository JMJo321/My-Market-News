# Weekly Meeting on February 15, 2022

## What I did

### Studying about the DF for Cattle

+ Classify variables into 5 categories
  + Market-related: market type, location
  + Product-related: category, class, commodity
  + Quantity-related: head count, average weight
  + Price-related: average price
  + Characteristic-related: frame, muscle grade, quality grade, yield grade, dressing, pregnancy stage, offspring weight.
    (e.g., for `class == "Heifers" & commodity == "Feeder Cattle"`, `frame` and `muscle_grade` are only matter.)

+ Normalize weight and price data
  + Create `weight.bracket` by exploiting `avg_weight`
  + Compute the average price for each weight bracket
    + Simple average
    + Weighted average


### Creating Sample Figures

+ For auction data of a commodity,
  + Average prices by weigh bracket, in a market
  + Average prices by market, for a weight bracket
  + To avoid the case that a figure include too much information, 
    + Exclude maximum and minimum prices
    + Exclude maximum and minimum weights


### Findings

+ The [Custom Average Tool](https://cat.ams.usda.gov/app/main#/dashboards/58d7ca1a733656a41a000029?embed=true&l=false&r=false&h=false&t=false&volatile=true) page does not provide any information about `category == "Cattle"`.

  

## What I will do

- [ ] Make sample figures for other commodities.
- [ ] Check [Run a Custom Report](https://www.marketnews.usda.gov/mnp/ls-report-config) webpage to all of API-providing information is available form it.