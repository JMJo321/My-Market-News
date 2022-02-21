# Weekly Meeting on February 3, 2022

## What I did

### Updating Summaries

1. Add columns regarding
   + Publication frequency
   + Prices
   + Quantities



### Issues

1. Many reports have varying publication frequencies.
   + Due to corrections, no publications for a while, etc. 
   + The days between publications lie between 1 and 30.
   + Its median is 17
2. Data fields being in many reports do NOT exist in API responses.
   + Reports that have missing data fields also lack information about `category` and `commodity`.
   + But from their titles, it is inferable that the reports are mainly for truck rates and video/mobile auctions.
   + The missing data fields are usually price- or quantity-relevant information. 



### Reports for `category == "Cattle"`

1. There are two categories of market type:

   1. Auction (Weekly)

      ![For-Report-on-2022-02-01_Auction](/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/04_Reports/Tables/For-Report-on-2022-02-01_Auction.png)

   2. Summary (Daily or Weekly)

      ![For-Report-on-2022-02-01_Summary](/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/04_Reports/Tables/For-Report-on-2022-02-01_Summary.png)

2. Average prices and weights are available from their API reponses. 



## What I will do

- [ ] Check [Run a Custom Report](https://www.marketnews.usda.gov/mnp/ls-report-config) webpage to all of API-providing information is available form it.
- [ ] Check whether the data available from the [Custom Average Tool](https://cat.ams.usda.gov/app/main#/dashboards/58d7ca1a733656a41a000029?embed=true&l=false&r=false&h=false&t=false&volatile=true) page can be computed from the API-providing data. 