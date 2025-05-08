# stat3106_final_data_load.R

library(dplyr)
library(zipcodeR)
library(httr)
library(jsonlite)


# Load Emergency Department Visits Data downloaded from OpenData.
# replace the directory to where the file
ed_data <- read.csv(
  file = "/Users/sominlee/Documents/Columbia/SPRING2025/STAT3106/final/Emergency_Department_Visits_and_Admissions_for_Influenza-like_Illness_and_or_Pneumonia_20250416.csv"
)


# Inspect extraction dates and select the most recent
date_counts <- ed_data %>% count(extract_date)

# Identify the extract_date with maximum rows
latest_date <- date_counts %>% arrange(desc(n)) %>% slice(1) %>% pull(extract_date)

# Filter to the latest extract and drop the extract_date column
ed_data_recent <- ed_data %>%
  filter(extract_date == latest_date) %>%
  select(-extract_date)

# Convert and explore date variable
ed_data_recent <- ed_data_recent %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  )

date_range <- range(ed_data_recent$date)

# Zip code coverage and missing dates
unique_zips <- unique(ed_data_recent$mod_zcta)
num_zips   <- length(unique_zips)

# Full daily sequence and any missing dates
full_date_seq <- seq(min(ed_data_recent$date), max(ed_data_recent$date), by = "day")
missing_dates <- setdiff(full_date_seq, unique(ed_data_recent$date))
ed_data_recent <- ed_data_recent %>% filter(mod_zcta != 10000)

# Count ZIPs per date, identify incomplete dates
zip_counts <- ed_data_recent %>%
  group_by(date) %>%
  summarise(num_zips = n_distinct(mod_zcta))

incomplete_dates <- zip_counts %>% filter(num_zips < num_zips)

#----------Fetching latitude and longitude information of zip code -------------
zip_info_list <- lapply(unique_zips, function(z) {
  res <- reverse_zipcode(z)
  if (nrow(res) == 0) return(NULL)
  data.frame(zip = z, lat = res$lat, lng = res$lng)
})
zip_latlon_df <- do.call(rbind, zip_info_list)

zip_latlon_df <- zip_latlon_df %>% filter(zip != 10000)

# Write lat/lon to CSV for external use
write.table(
  zip_latlon_df[, c("lat", "lng")],
  file       = "lat_lon_list.csv",
  sep        = ",",
  row.names  = FALSE,
  col.names  = FALSE,
  quote      = FALSE
)

#---------Weather data acquisition-----------
chunk_size <- 10
chunks     <- split(zip_latlon_df, ceiling(seq_len(nrow(zip_latlon_df)) / chunk_size))

safe_get <- function(url, wait_secs = 60) {
  repeat {
    resp <- GET(url)
    if (resp$status_code == 429) {
      Sys.sleep(wait_secs)
    } else {
      return(resp)
    }
  }
}

# Define weather variables
weather_vars <- paste0(
  "temperature_2m_mean,temperature_2m_max,temperature_2m_min,",
  "apparent_temperature_mean,apparent_temperature_max,apparent_temperature_min,",
  "wind_speed_10m_max,wind_gusts_10m_max,",
  "shortwave_radiation_sum,",
  "daylight_duration,sunshine_duration,rain_sum,snowfall_sum,",
  "precipitation_sum,precipitation_hours"
)

# Loop over batches to download weather data
start_batch <- 1
end_batch   <- length(chunks)

for (batch_num in start_batch:end_batch) {
  current_chunk     <- chunks[[batch_num]]
  weather_data_list <- list()
  
  for (i in seq_len(nrow(current_chunk))) {
    lat <- current_chunk$lat[i]
    lon <- current_chunk$lng[i]
    zip <- current_chunk$zip[i]
    
    # Archive (historical) data URL
    archive_url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", lat,
      "&longitude=", lon,
      "&start_date=2020-03-01",
      "&end_date=2022-12-06",
      "&daily=", weather_vars,
      "&timezone=America/New_York"
    )
    
    # Forecast data URL
    forecast_url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", lat,
      "&longitude=", lon,
      "&daily=", weather_vars,
      "&forecast_days=16",
      "&timezone=America/New_York"
    )
    
    resp1 <- safe_get(archive_url)
    if (resp1$status_code == 200) {
      df1 <- fromJSON(content(resp1, as = "text"), flatten = TRUE)$daily
      df1 <- as.data.frame(df1)
      df1$source <- "archive"
      df1$zip    <- zip
    } else {
      df1 <- NULL
    }
    
    resp2 <- safe_get(forecast_url)
    if (resp2$status_code == 200) {
      df2 <- fromJSON(content(resp2, as = "text"), flatten = TRUE)$daily
      df2 <- as.data.frame(df2)
      df2$source <- "forecast"
      df2$zip    <- zip
    } else {
      df2 <- NULL
    }
    
    weather_data_list[[i]] <- bind_rows(df1, df2)
    Sys.sleep(3)
  }
  
  weather_batch <- bind_rows(weather_data_list)
  saveRDS(weather_batch, paste0("weather_batch_", batch_num, ".rds"))
}

# Combine all batches and export to CSV
rds_files <- list.files(pattern = "weather_batch_.*\\.rds$")
all_weather <- bind_rows(lapply(rds_files, readRDS))
write.csv(all_weather, "weather_data_all_archiveANDforecast.csv", row.names = FALSE)
