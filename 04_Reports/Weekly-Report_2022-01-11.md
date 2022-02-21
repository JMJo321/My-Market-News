# Weekly Meeting on January 11, 2022

## What I did

### Create a GitHub Repository

To track My Market News-related R scripts systematically, I make a [GitHub repository](https://github.com/JMJo321/My-Market-News).



### Making Lists of MMN Reports

1. There are two sources of MMN report list:
   + [MMN API](https://mymarketnews.ams.usda.gov/mymarketnews-api/reports)
   + `Report Index All` in [Reports](https://mymarketnews.ams.usda.gov/filerepo/reports?field_slug_id_value=&name=AMS_2714&field_slug_title_value=&field_published_date_value=&field_report_date_end_value=&field_api_market_types_target_id=All)
2. Findings from comparing the two lists
   + One list does NOT cover the other one entirely.
     + Number of reports in both lists: 685
     + Number of reports only in the list obtained from the API: 5
     + Number of reports only in the list obtained from `Report Index All`: 923
   + Both lists includes reports that are published after 2018. 
     + It seems that the two lists only show reports published recently.
     + For reports in the list obtained from the API, old reports are accessible by using the API.
   + The reports only in `Report Index All` are NOT available from the API.
     =>  For those reports, it might be necessary to look at PDF/TXT files one by one, in order to get detailed information.
3. It is difficult to make a summary of commodities, locations, frequencies, etc. based on information in the lists (especially, based on report title) because:
   - Several commodities can be found from both lists. 
     (e.g., Slaughter Cattle)
   - Many reports include information about multiple commodities.
     (e.g. `Texas Daily Grain Bids` report includes corn, soybeans, wheat, etc.)
   - There are many reports that no information about market location and publication cycle is in their title. 
     (e.g., `St. Louis Truck Grain Prices at Terminal Elevators (JC_GR111)`)



## What I will do

- [ ] Create Dataframes (DFs) from reports which are available from the API
- [ ] Check [Run a Custom Report](https://www.marketnews.usda.gov/mnp/ls-report-config) webpage to all of API-providing information is available form it
- [ ] Make a summary of commodities, locations, frequencies, etc. (If possible...)