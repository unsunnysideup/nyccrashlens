# CrashLens NYC
This is an interactive web application allowing exploration of various scales into the patterns and trends of over two million New York City's motor crashes. The following includes questions (not limited to) that can be explored in CrashLens NYC:

- Which regions have all-time high crashes or casualties? 
- What patterns is present regarding crashes or casualties in the broader view of NYC across certain time frames? 
- How does my residing region compare and contrast to other regions in terms of crashes across time and implication? 
- Which groups are more impacted by crashes and how do they differ across space and time?
- What are common reported crash factors across city-view, borough-view, and neighborhood-view?

Check out CrashLens NYC [here](https://txsu-n-nyccrashlens.share.connect.posit.cloud/)!

## Features
CrashLens NYC has three main tools:

1. **Home** - The homepage of the application: Features an interactive, scalable chloroplethic leaflet of NYC by neighborhoods and a borough table sorted by highest frequency. Both the leaflet and table display frequency with user's selection of timeframe from 2012 to 2026 and metric from a dropdown of three choices: crashes, injuries and fatalities. The leaflet also allows users to click into a neighborhood for more information regarding their name, borough, exact count, and top three reported factors for vehicular crashes.  

2. **Compare** - This page allows users to closely analyze two regions for compare and contrast with the option of selecting a time frame for specific analysis. Comparison of regions can indicate either borough vs. borough or neighborhood vs. neighborhood. Users select two boroughs or neighborhoods, and from there, they can analyze temporal patterns and casualty breakdowns while easily compare the trends between the two selected regions.  

3. **Database** - Here, users have access to the processed database across millions of crash entries with the choice to apply filters and export that filtered data as a .csv. 

## Sources
### Crash Data
[Data](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95/about_data) is sourced from the New York Police Department, released on June 15, 2026, and hosted by NYC Open Data.
Each row contains information of a crash event reported by the police that either has casualties or damages exceeding $1000. Each row contains the following information:

1. Crash Identification
2. Location of Crash
3. Casualty Breakdown (Injuries and Deaths of pedestrians, motorists, and cyclists)
4. Contributing Crash Factor
5. Vehicle Types involved in the Crash

The data has over 2.27 million crash entries spanning from 2012 to 2026, and has noticeable amounts of missing and messy values. 

### Geospatial Data 
The [data](https://github.com/nycehs/NYC_geography/blob/master/UHF42.geo.json) for the geospatial structures of New York City is sourced from the NYC Environmental Health Services Github Repo. 

### Setup 
#### Prerequisites:
- R 
- RStudio or Positron

#### Clone Repository
```r
git clone https://github.com/unsunnysideup/nyccrashlens.git
cd nyccrashlens
```
#### Data Retrieval
1. Export the [crash data ](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95/data_preview) as a .csv
2. Install all dependencies:
```r
renv::restore()
```
3. Run data processing scripts in order:
```r
source("scripts/geocoding.r")
source("scripts/wrangling.r")
```
geocoding.r: Recovers missing coordinates as possible
wrangling.r: Wrangles and preprocess data for app

4. Run App
```r
shiny::runApp()
```

### Optional Features:
If you decide to add a heatmap feature to the chloroplethic leaflet, the code is included in 'scripts/extra_code.r'. The heatmap was excluded from the final web application as it provided no meaningful insights and additional noise. 
