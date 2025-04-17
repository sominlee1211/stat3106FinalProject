# stat3106FinalProject

## How to obtain Datasets

### Emergency Department Dataset

Data about daily visit number in Emergency Department in NYC can be optained by NYC Open Data source. https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg/about_data

First, create a query that filters all rows except for the ones from the most recent "Extract_Date". The most recent extract date will include all data from the past extract date so it's crucial to do this to vaoid downloading unnecessarily big file with repetitive data.
Once it's filtered out, you can easily download the data as .csv file by clicking "Export"

### Weather Dataset

This is a little tricky part that needs API access. The code to obtain this data is in "obtain_data.rmd". You can simply run this code to obtain the full dataset. But will take a while because free API rate is limited.
