
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(ranger)
library(tidyr)
library(corrplot)
library(ggplot2)

# load the ED Department Visit Data and make sure we only have the most recent ones
# - this should be done when downloading the data but for double checking.
ed_data <- read.csv(file="/Users/sominlee/Documents/Columbia/SPRING2025/STAT3106/final/Emergency_Department_Visits_and_Admissions_for_Influenza-like_Illness_and_or_Pneumonia_20250416.csv")
ed_data_recent <- ed_data %>%
  filter(extract_date == "12/05/2022") %>%
  select(-extract_date)
ed_data_recent <- ed_data_recent %>%
  filter(mod_zcta != 10000)

# load the weather data obtained from fetching API
weather_data <- read.csv(file="/Users/sominlee/Documents/Columbia/SPRING2025/STAT3106/final/weather_data_all_archiveANDforecast.csv")

# ----- Formatting --------
ed_data_recent <- ed_data_recent %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
str(weather_data$time)
weather_data <- weather_data %>%
  mutate(time = as.Date(time))

weather_filtered <- weather_data %>%
  filter(
    zip %in% ed_data_recent$mod_zcta,
    (time %in% ed_data_recent$date) | source == "forecast"
  )

ed_model_data <- ed_data_recent %>%
  inner_join(weather_filtered, by = c("mod_zcta" = "zip", "date" = "time"))

# Merge the ED data with weather data so we have full model data for training.
model_data <- ed_model_data %>%
  select(temperature_2m_max, temperature_2m_min, wind_speed_10m_max,
         wind_gusts_10m_max, precipitation_sum, shortwave_radiation_sum) %>%
  drop_na()

excluded_vars <- c("mod_zcta", "date", "total_ed_visits", "ili_pne_visits", "ili_pne_admissions")

model_data <- ed_model_data %>%
  select(-one_of(excluded_vars)) %>%
  drop_na()

weather_numeric <- ed_model_data %>%
  select(where(is.numeric))

# Check the Correlation and save the correlation plot 
png(
  filename = "outputs/corrplot.png", 
  width    = 800, 
  height   = 800, 
  res      = 100
)

corrplot::corrplot(
  cor_matrix, 
  method  = "color", 
  type    = "lower", 
  tl.cex  = 0.8
)

dev.off()


# ---------- Building Model Starts here ----------

weather_scaled <- scale(weather_numeric)

ed <- ed_data_recent %>%
  mutate(date = as.Date(date))

weather <- weather_filtered %>%
  rename(date = time) %>%
  mutate(date = as.Date(date))

# creating lag variable to save 14 days of ED data
# and the Y variable to the ED Visit number (influenza-like) in 2 weeks from now
ed_lags <- ed %>%
  arrange(mod_zcta, date) %>%
  group_by(mod_zcta) %>%
  mutate(
    lag1  = lag(ili_pne_visits,  1),
    lag2  = lag(ili_pne_visits,  2),
    lag3  = lag(ili_pne_visits,  3),
    lag4  = lag(ili_pne_visits,  4),
    lag5  = lag(ili_pne_visits,  5),
    lag6  = lag(ili_pne_visits,  6),
    lag7  = lag(ili_pne_visits,  7),
    lag8  = lag(ili_pne_visits,  8),
    lag9  = lag(ili_pne_visits,  9),
    lag10  = lag(ili_pne_visits,  10),
    lag11  = lag(ili_pne_visits,  11),
    lag12  = lag(ili_pne_visits,  12),
    lag13  = lag(ili_pne_visits,  13),
    lag14  = lag(ili_pne_visits,  14),
    # target is count at T + 14
    y_two_weeks = lead(ili_pne_visits, 14)
  ) %>%
  ungroup() %>%
  filter(!is.na(y_two_weeks))

# Filtering the weather data by its source
weather_arch <- weather %>% filter(source == "archive")     
weather_fc   <- weather %>% filter(source == "forecast")
weather_all  <- weather %>% select(-source)

# Create 4 different dfs to use
# A: ED-only 
df_A <- ed_lags %>%
  select(
    y_two_weeks,
    paste0("lag", 1:14)
  ) %>%
  na.omit()

# B: weather archive only 
df_B <- ed_lags %>%
  left_join(weather_arch, by = c("mod_zcta" = "zip", "date" = "date")) %>%
  select(
    y_two_weeks,
    temperature_2m_mean:precipitation_hours
  ) %>%
  na.omit()

# C: weather archive + forecast
df_C <- ed_lags %>%
  left_join(weather_all, by = c("mod_zcta" = "zip", "date" = "date")) %>%
  select(
    y_two_weeks,
    temperature_2m_mean:precipitation_hours
  ) %>%
  na.omit()

# D: combined ED lags + all weather
df_D <- ed_lags %>%
  left_join(weather_all, by = c("mod_zcta" = "zip", "date" = "date")) %>%
  select(
    y_two_weeks,
    paste0("lag", 1:14),
    temperature_2m_mean:precipitation_hours
  ) %>%
  na.omit()

# helper to train a single ranger and return OOB RMSE
# - This is just to choose which predictors we should use out of 4 quickly.
quick_rf <- function(df, ntree = 200) {
  mtry <- floor(sqrt(ncol(df) - 1))
  rf  <- ranger(
    formula      = y_two_weeks ~ .,
    data         = df,
    num.trees    = ntree,
    mtry         = mtry,
    importance   = "impurity",
    write.forest = TRUE,
    seed         = 42
  )
  sqrt(rf$prediction.error)  # this is the OOB RMSE
}

oob_errors <- c(
  ED_only   = quick_rf(df_A),
  Archive   = quick_rf(df_B),
  ArchiveFC = quick_rf(df_C),
  Combined  = quick_rf(df_D)
)

print(oob_errors) 

# We had the lowest oob error from df_D so we will use both ED data and weather.

# larger‐forest on the combined dataset (datset D with ed data + weather)
rf_full <- ranger(
  formula      = y_two_weeks ~ .,
  data         = df_D,
  num.trees    = 500,
  mtry         = floor(sqrt(ncol(df_D) - 1)),
  importance   = "impurity",
  write.forest = TRUE,
  seed         = 42
)

# --------------Feature Engineering------------
df_vis <- df_D %>%
  mutate(
    pred      = predict(rf_full, data = df_D)$predictions,
    resid     = y_two_weeks - pred
  )

# grab the two most important vars:
top2 <- names(sort(rf_full$variable.importance, decreasing = TRUE))[1:2]

# A) color by actual outcome y
p1 <- ggplot(df_vis, aes_string(x = top2[1], y = top2[2], color = "y_two_weeks")) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Top 2 Predictors vs Actual Visits",
    color = "ED Visits"
  )
# save it
ggsave("outputs/plot_actual_vs_top2.png", plot = p1, width = 6, height = 5, dpi = 150)


# B) color by prediction
p2 <- ggplot(df_vis, aes_string(x = top2[1], y = top2[2], color = "pred")) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Top 2 Predictors vs Predicted Visits",
    color = "Predicted"
  )
# save it
ggsave("outputs/plot_predicted_vs_top2.png", plot = p2, width = 6, height = 5, dpi = 150)


# C) color by residual
p3 <- ggplot(df_vis, aes_string(x = top2[1], y = top2[2], color = "resid")) +
  geom_point(alpha = 0.6) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  labs(
    title = "Residuals (Actual – Predicted)",
    color = "Residual"
  )
# save it
ggsave("outputs/plot_residuals_vs_top2.png", plot = p3, width = 6, height = 5, dpi = 150)


# 1) Rebuild df_D 
df_D_full <- ed_lags %>%
  left_join(weather_all, by = c("mod_zcta" = "zip", "date" = "date")) %>%
  select(
    mod_zcta,
    date,
    y_two_weeks,
    paste0("lag", 1:14),
    temperature_2m_mean:precipitation_hours
  ) %>%
  na.omit()

# Add engineered features
df_feats <- df_D_full %>%
  left_join(
    #1) total visits
    ed_data_recent %>% select(mod_zcta, date, total_ed_visits),
    by = c("mod_zcta", "date")
  ) %>%
  # 2) Flag extremes in binary
  mutate(
    cold_flag  = as.numeric(temperature_2m_mean < 32),
    heat_flag  = as.numeric(temperature_2m_max  > 85),
    rain_flag  = as.numeric(precipitation_sum   > 0),
    windy_flag = as.numeric(wind_speed_10m_max  > 20)
  ) %>%
  # 3) Interaction terms
  mutate(
    cold_and_windy = cold_flag * windy_flag
  ) %>%
  # 4) Binned temperature
  mutate(
    temp_bin = cut(temperature_2m_mean, breaks = seq(0, 100, 5), right = FALSE)
  )

df_feats_model <- df_feats %>%
  select(-mod_zcta, -date)

# Time‐Series Cross Validation

set.seed(42)
df_feats_clean <- df_feats_model %>% drop_na()
sum(is.na(df_feats_clean))  # should be 0

rf_grid <- expand.grid(
  mtry           = floor(sqrt(ncol(df_D) - 1)),
  splitrule      = "variance",
  min.node.size  = 5
)

library(doParallel)
library(caret)

tc2 <- trainControl(
  method        = "timeslice",
  initialWindow = 52*7,
  horizon       = 14,
  fixedWindow   = TRUE,
  allowParallel = TRUE,
  savePredictions = "final"
)

metric_choice <- "RMSE"

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Run the CV
set.seed(42)
rf_cv_feats <- train(
  y_two_weeks ~ .,
  data      = df_feats_clean,
  method    = "ranger",
  metric    = metric_choice,
  trControl = tc2,
  tuneGrid  = rf_grid,
  num.trees = 100,
  importance= "impurity"
)

stopCluster(cl)
registerDoSEQ()

print(rf_cv_feats)




df_D_clean <- df_D %>% drop_na()

# Training the same thing with no engineered features
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

set.seed(42)
rf_cv <- train(
  y_two_weeks ~ .,
  data      = df_D_clean,
  method    = "ranger",
  metric    = "RMSE",
  trControl = tc2,        
  tuneGrid  = rf_grid,    
  num.trees = 100,
  importance= "impurity",
  num.threads= 1
)
stopCluster(cl)
registerDoSEQ()

print(rv_cv)

cat("Old CV RMSE:", round(rf_cv$results$RMSE, 3), "\n")
cat("New CV RMSE:", round(rf_cv_feats$results$RMSE, 3), "\n")

# Splitting into Train vs Test


cat("Total rows in df_feats (with date):", nrow(df_feats), "\n")

cutoff_date <- max(df_feats$date) - 28
cat("Rows before cutoff:", nrow(filter(df_feats, date < cutoff_date)), "\n")
cat("Rows after cutoff: ", nrow(filter(df_feats, date >= cutoff_date)), "\n")


df_full <- df_feats %>%             
  drop_na()                      

train_df <- df_feats %>% filter(date < cutoff_date)
test_df  <- df_feats %>% filter(date >= cutoff_date)

train_set <- train_df %>% select(-mod_zcta, -date)
test_set  <- test_df  %>% select(-mod_zcta, -date)

final_rf <- ranger(
  y_two_weeks     ~ .,
  data            = train_set,
  num.trees       = 500,
  mtry            = floor(sqrt(ncol(train_set)-1)),
  importance      = "impurity",
  write.forest    = TRUE,
  seed            = 42,
  num.threads     = 1,        
  replace         = TRUE,    
  sample.fraction = 0.632    
)

preds_test  <- predict(final_rf, data = test_set)$predictions
actual_test <- test_set$y_two_weeks

test_stats <- postResample(pred = preds_test, obs = actual_test)


library(Metrics)
rmse_val <- rmse(actual_test, preds_test)
mae_val  <- mae(actual_test, preds_test)
mape_val <- mape(actual_test, preds_test) * 100

cat(sprintf("Hold-out RMSE   = %.2f\n", rmse_val))
cat(sprintf("Hold-out MAE    = %.2f\n", mae_val))
cat(sprintf("Hold-out MAPE   = %.1f%%\n", mape_val))
cat(sprintf("Hold-out R²     = %.2f\n", test_stats["Rsquared"]), "\n")


resid <- actual_test - preds_test

png("outputs/holdout_residuals.png", width=800, height=400, res=100)
par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(preds_test, resid,
     xlab = "Predicted y_two_weeks",
     ylab = "Residuals",
     main = "Residuals vs Predicted")
abline(h = 0, lty = 2)

hist(resid, breaks = 30,
     main = "Residual Distribution",
     xlab = "Actual – Predicted")
dev.off()



# Checking the predictions vs actuals with the plot with Enhanced model
df_vis_feats <- df_feats_clean %>%
  mutate(
    pred_enh  = predict(rf_cv_feats, newdata = .),
    resid_enh = y_two_weeks - pred_enh
  )

rf_enhanced <- rf_cv_feats$finalModel
top2_feats <- names(
  sort(rf_enhanced$variable.importance, decreasing = TRUE)
)[1:2]

# A) Actual vs top2_feats
p1e <- ggplot(df_vis_feats,
              aes_string(x = top2_feats[1], y = top2_feats[2],
                         color = "y_two_weeks")) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Enhanced Model: Actual vs Top 2", color="Actual")

# B) Predicted vs top2_feats
p2e <- ggplot(df_vis_feats,
              aes_string(x = top2_feats[1], y = top2_feats[2],
                         color = "pred_enh")) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Enhanced Model: Predicted vs Top 2", color="Predicted")

# C) Residuals vs top2_feats
p3e <- ggplot(df_vis_feats,
              aes_string(x = top2_feats[1], y = top2_feats[2],
                         color = "resid_enh")) +
  geom_point(alpha = 0.6) +
  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title = "Enhanced Model: Residuals vs Top 2", color="Residual")

ggsave("outputs/plot_actual_vs_top2_enhanced.png", p1e, width=6, height=5)
ggsave("outputs/plot_predicted_vs_top2_enhanced.png", p2e, width=6, height=5)
ggsave("outputs/plot_residuals_vs_top2_enhanced.png", p3e, width=6, height=5)
