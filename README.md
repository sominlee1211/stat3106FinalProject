# Predicting Emergency Department Visits with Weather in NYC

## Project Summary
This project develops a two-week–ahead forecasting model for influenza-like illness (ILI) and pneumonia ED visits in New York City by combining historical visit counts, meteorological observations, and weather forecasts. Using a random forest (`ranger`) on 2.5 years of daily ZIP‐level data (177 ZIPs × ~1,000 days), we achieve a mean absolute error of 3 visits and explain 70 % of variance on unseen data. Targeted feature engineering (ED load, extreme-weather flags, interaction terms) yields a 20–38 % RMSE improvement over lag‐only or weather-only baselines. Time-series cross-validation and a four-week hold-out test validate robustness. The resulting early-warning tool enables proactive ED staffing and resource planning.



## How to Obtain Datasets

### Emergency Department Dataset  
Data on daily ILI/pneumonia ED visits is available from NYC Open Data:  
https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg/about_data  

1. In the portal, use the query builder to filter for the **most recent** `Extract_Date`.  
2. Click **Export → CSV** and save the file as
data/Emergency_Department_Visits_and_Admissions_for_Influenza-like_Illness_and_or_Pneumonia_YYYYMMDD.csv


### Weather Dataset  
Weather observations and 16-day forecasts are retrieved via the Open-Meteo API. To generate the merged file run:

```r
("R/stat3106_final_data_load.R")

```
# Running Analysis

## Install packages:
install.packages(c(
  "dplyr","tidyr","caret","ranger","zipcodeR",
  "httr","jsonlite","corrplot","here","viridis"
))

