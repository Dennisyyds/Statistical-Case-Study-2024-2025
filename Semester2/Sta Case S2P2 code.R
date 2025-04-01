# Load required packages
library(tidyverse)    # For data manipulation and visualization
library(broom)        # For tidying model outputs
library(caret)        # For performance evaluation
library(dplyr)
library(boot)
library(lubridate)


# Read the dataset
Data<- read.csv("/Users/rongzhi/Desktop/Stat Case study/semester2/part2/data/SCS_demand_modelling.csv")
Temp_data <- read.csv("/Users/rongzhi/Desktop/Stat Case study/semester2/part2/data/SCS_hourly_temp.csv")


# Time Series Plot: Daily Peak Demand over Time
Data$Date_form <- as.Date(Data$Date)

plot1 <- ggplot(Data, aes(x = Date_form, y = demand_gross)) +
  geom_line(color = "#1f77b4", linewidth = 0.5) +
  labs(
    title = "Time Series of Daily Peak Demand",
    x = "Date",
    y = "Peak Demand (MW)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#2c3e50", hjust = 0.5, margin = margin(b = 30)),
    axis.title = element_text(size = 16, color = "#34495e"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#7f8c8d", size = 12),
    axis.text.y = element_text(color = "#7f8c8d", size = 12),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.75),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    plot.margin = margin(40, 40, 40, 40),
    panel.background = element_rect(fill = "#f4f4f4", color = "#dcdcdc", size = 0.5),
    axis.ticks = element_line(color = "#dcdcdc")
  ) +
  scale_x_date(
    date_labels = "%b %Y", 
    date_breaks = "6 months", 
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, suffix = " MW")) +
  theme(legend.position = "none") +
  geom_smooth(method = "loess", color = "#e74c3c", fill = "#f39c12", alpha = 0.3, size = 1.2)
ggsave("Time_Series_Plot.png", plot = plot1, width = 12, height = 8, dpi = 900)

# Histogram of Demand
plot2 <- ggplot(Data, aes(x = demand_gross)) +
  geom_histogram(bins = 30, fill = "#3498db", color = "#ecf0f1", alpha = 0.7) +
  labs(
    title = "Histogram of Daily Peak Demand",
    x = "Peak Demand (MW)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#2c3e50", hjust = 0.5, margin = margin(b = 30)),
    axis.title = element_text(size = 16, color = "#34495e"),
    axis.text = element_text(color = "#7f8c8d", size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.75),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.5),
    plot.margin = margin(40, 40, 40, 40),
    panel.background = element_rect(fill = "#f9f9f9", color = "#dcdcdc", size = 0.5),
    axis.ticks = element_line(color = "#dcdcdc")
  ) +
  scale_x_continuous(labels = scales::comma_format(scale = 1, suffix = " MW")) +
  theme(legend.position = "none")
ggsave("Histogram_of_Demand.png", plot = plot2, width = 12, height = 8, dpi = 900)

# Scatter Plot: TO vs TE
ggplot(Data, aes(x = TE, y = TO)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "TO vs TE",
       x = "TE (°C)", y = "TO")

# Scatter Plot: Temp vs TE
ggplot(Data, aes(x = temp, y = TO)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Temp vs TE",
       x = "TE (°C)", y = "TO")

# Scatter Plot: TO vs temp
ggplot(Data, aes(x = TE, y = temp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "TO vs temp",
       x = "Temp", y = "TO")




model_TE <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex), data = Data)
summary(model_TE)

model_TO <- lm(demand_gross ~ TO + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex), data = Data)
summary(model_TO)

model_temp <- lm(demand_gross ~ temp + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex), data = Data)
summary(model_temp)

#solar_S 0.8065
model_Solar <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + factor(monthindex), data = Data)
summary(model_Solar)

#wind 0.8075
model_Wind <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + solar_S + factor(monthindex), data = Data)
summary(model_Wind)

#DSN 0.7914
model_DSN <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + wind + solar_S + factor(monthindex), data = Data)
summary(model_DSN)

#monthindex 0.7791
model_Month <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S, data = Data)
summary(model_Month)

#wdayindex 0.4807
model_Wday <- lm(demand_gross ~ TE + factor(start_year) + DSN + wind + solar_S + factor(monthindex), data = Data)
summary(model_Wday)

#start_year 0.4928
model_year <- lm(demand_gross ~ TE + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex), data = Data)
summary(model_year)

#model_DSN2
model_DSN2 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex) + I (DSN^2), data = Data)
summary(model_DSN2)

#model_TO_DSN2
model_TO_DSN2 <- lm(demand_gross ~ TO + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + factor(monthindex) + + I (DSN^2), data = Data)
summary(model_TO_DSN2)

#monthindex 0.7791
model_Month_num <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + monthindex + DSN + wind + solar_S, data = Data)
summary(model_Month_num)

#monthindex 0.44
model_Month_wday_num <- lm(demand_gross ~ TE + factor(start_year) + wdayindex + DSN + wind + solar_S, data = Data)
summary(model_Month)

#monthindex 0.5971
model_Month_year_num <- lm(demand_gross ~ TE + start_year + factor(wdayindex)  + DSN + wind + solar_S, data = Data)
summary(model_Month_year_num)

#monthindex 0.7753
model_Month_wind <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  + DSN + solar_S, data = Data)
summary(model_Month_wind)

#0.7724
model_Month_solar <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  + DSN + wind, data = Data)
summary(model_Month_solar)

#monthindex 0.7791
model_Month_TO <- lm(demand_gross ~ TO + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S, data = Data)
summary(model_Month_TO)

#monthindex 0.7791
model_Month_DSN2 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + I (DSN^2), data = Data)
summary(model_Month_DSN2)

#monthindex 0.7753
model_Month_wind_solar <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  + DSN, data = Data)
summary(model_Month_wind_solar)



AIC(model_TE, model_Solar, model_Wind, model_DSN, model_Month, model_Wday, model_year,model_DSN2,model_TO_DSN2,model_Month_wind,model_Month_solar,model_Month_TO,model_Month_DSN2,model_Month_wind_solar)
BIC(model_TE, model_Solar, model_Wind, model_DSN, model_Month, model_Wday, model_year,model_DSN2,model_TO_DSN2,model_Month_wind,model_Month_solar,model_Month_TO,model_Month_DSN2,model_Month_wind_solar)

threshold <- quantile(Data$demand_gross, 0.95)  # top 5%

# 2.2 Subset data to only high-demand days
df_high <- Data %>% 
  filter(demand_gross >= threshold)

# 3.1 Predict using each model
df_high$pred1 <- predict(model_TE, newdata = df_high)
df_high$pred2 <- predict(model_TO, newdata = df_high)
df_high$pred3 <- predict(model_temp, newdata = df_high)
df_high$pred4 <- predict(model_Solar, newdata = df_high)
df_high$pred5 <- predict(model_Wind, newdata = df_high)
df_high$pred6 <- predict(model_Month, newdata = df_high)
df_high$pred7 <- predict(model_DSN, newdata = df_high)
df_high$pred8 <- predict(model_Month_num, newdata = df_high)
df_high$pred9 <- predict(model_Month_wind, newdata = df_high)
df_high$pred10 <- predict(model_Month_solar, newdata = df_high)
df_high$pred11 <- predict(model_Month_TO, newdata = df_high)
df_high$pred12 <- predict(model_Month_DSN2, newdata = df_high)
df_high$pred13 <- predict(model_Month_wind_solar, newdata = df_high)

# 4.1 Mean Absolute Error (MAE) on high-demand subset
mae1 <- mean(abs(df_high$demand_gross - df_high$pred1))
mae2 <- mean(abs(df_high$demand_gross - df_high$pred2))
mae3 <- mean(abs(df_high$demand_gross - df_high$pred3))
mae4 <- mean(abs(df_high$demand_gross - df_high$pred4))
mae5 <- mean(abs(df_high$demand_gross - df_high$pred5))
mae6 <- mean(abs(df_high$demand_gross - df_high$pred6))
mae7 <- mean(abs(df_high$demand_gross - df_high$pred7))
mae8 <- mean(abs(df_high$demand_gross - df_high$pred8))
mae9 <- mean(abs(df_high$demand_gross - df_high$pred9))
mae10 <- mean(abs(df_high$demand_gross - df_high$pred10))
mae11 <- mean(abs(df_high$demand_gross - df_high$pred11))
mae12 <- mean(abs(df_high$demand_gross - df_high$pred12))
mae13 <- mean(abs(df_high$demand_gross - df_high$pred13))
# 4.3 Print results
cat("Tail (top 10%) MAE:\n",
    "Model1 =", mae1, "\n",
    "Model2 =", mae2, "\n",
    "Model3 =", mae3, "\n",
    "Model4 =", mae4, "\n",
    "Model5 =", mae5, "\n",
    "Model6 =", mae6, "\n",
    "Model7 =", mae7, "\n",
    "Model8 =", mae8, "\n",
    "Model9 =", mae9, "\n",
    "Model10 =", mae10, "\n",
    "Model11 =", mae11, "\n",
    "Model12 =", mae12, "\n","Model13 =", mae13, "\n")


summary(model_Month_DSN2)



results <- data.frame()
unique_winters <- sort(unique(Data$start_year))
n_winters <- length(unique_winters)
for (i in 1:(n_winters-1)) {
  # Train on winters [1..i], test on winter (i+1)
  test_year <- unique_winters[i+1]
  train_years <- setdiff(unique_winters,test_year)
  
  train_df <- Data %>% filter(start_year %in% train_years)
  test_df  <- Data %>% filter(start_year == test_year)
  
  # Fit model1
  fit1 <- lm(demand_gross ~ TE + start_year + wdayindex + DSN + wind + solar_S + monthindex, data = Data)
  # Fit model2
  fit2 <- lm(demand_gross ~ TE + start_year + wdayindex + wind + solar_S + DSN, data = Data)
  # Fit model3
  fit3 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex) + DSN + wind + solar_S + I (DSN^2),data = Data)
  # Fit model4
  fit4 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  + DSN + wind , data = Data)
  # Fit model5
  fit5 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  + DSN + solar_S, data = Data)
  # Fit model6
  fit6 <- lm(demand_gross ~ TE + factor(start_year) + factor(wdayindex)  +DSN, data = Data)
  # Predict on test
  test_df$pred1 <- predict(fit1, newdata=test_df)
  test_df$pred2 <- predict(fit2, newdata=test_df)
  test_df$pred3 <- predict(fit3, newdata=test_df)
  test_df$pred4 <- predict(fit4, newdata=test_df)
  test_df$pred5 <- predict(fit5, newdata=test_df)
  test_df$pred6 <- predict(fit6, newdata=test_df)
  # Evaluate tail performance in the test set
  threshold <- quantile(test_df$demand_gross, 0.95)
  test_df_high <- test_df %>% filter(demand_gross >= threshold)
  
  mae1 <- mean(abs(test_df_high$demand_gross - test_df_high$pred1))
  mae2 <- mean(abs(test_df_high$demand_gross - test_df_high$pred2))
  mae3 <- mean(abs(test_df_high$demand_gross - test_df_high$pred3))
  mae4 <- mean(abs(test_df_high$demand_gross - test_df_high$pred4))
  mae5 <- mean(abs(test_df_high$demand_gross - test_df_high$pred5))
  mae6 <- mean(abs(test_df_high$demand_gross - test_df_high$pred6))
  results <- rbind(results, data.frame(
    Fold = i,
    TestWinter = test_year,
    Model1_MAE = mae1,
    Model2_MAE = mae2,
    Model3_MAE = mae3,
    Model4_MAE = mae4,
    Model5_MAE = mae5,
    Model6_MAE = mae6
  ))
}

# Summarize
results_summary <- results %>%
  summarise(
    Mean_Model1_MAE = mean(Model1_MAE, na.rm=TRUE),
    Mean_Model2_MAE = mean(Model2_MAE, na.rm=TRUE),
    Mean_Model3_MAE = mean(Model3_MAE, na.rm=TRUE),
    Mean_Model4_MAE = mean(Model4_MAE, na.rm=TRUE),
    Mean_Model5_MAE = mean(Model5_MAE, na.rm=TRUE),
    Mean_Model6_MAE = mean(Model6_MAE, na.rm=TRUE)
  )

print(results_summary)

# Coefficients for year effect are something like:
coef_table <- coef(model_Month_wind_solar)
# Intercept is baseline (reference year), other years are relative offsets
coef_table

# block size in days, for example 7
block_size <- 7  
ts_data <- Data[order(Data$Date), ]  # ensure time order

block_boot_model <- function(data, indices) {
  # 'indices' will be block-based
  d <- data[indices, ]
  fit <- lm(demand_gross  ~ TE + factor(start_year) + factor(wdayindex)  +DSN, data=d)
  return(coef(fit))
}

set.seed(123)
ts_boot_res <- tsboot(ts_data, statistic=block_boot_model, R=1000,
                      l=block_size, sim="fixed")  # 'fixed' or 'geom' block length
ts_boot_res


# For the intercept (index=1):
boot.ci_intercept <- boot.ci(ts_boot_res, type = "perc", index = 1)
boot.ci_intercept

n_coefs <- length(ts_boot_res$t0)  # number of coefficients
ci_list <- vector("list", n_coefs)

for (i in seq_len(n_coefs)) {
  ci_list[[i]] <- boot.ci(ts_boot_res, type = "perc", index = i)
}

ci_list

# Set up a 2x2 plot grid
par(mfrow = c(2, 2))
plot(model_Month_wind_solar)
par(mfrow = c(1, 1))

# Alternatively, using ggplot2 for a Residuals vs Fitted Plot
Data$Fitted <- predict(model_Month,Data)
Data$Residuals <- residuals(model_Month)
plot3 <- ggplot(Data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.35, size = 3, color = "#2980b9") + 
  geom_hline(yintercept = 0, color = "#e74c3c") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#2c3e50", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 16, color = "#34495e"),
    axis.text = element_text(color = "#7f8c8d", size = 12),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.75),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    plot.margin = margin(40, 40, 40, 40),
    panel.background = element_rect(fill = "#f9f9f9", color = "#dcdcdc", size = 0.5),
    axis.ticks = element_line(color = "#dcdcdc"),
    legend.position = "none",
    panel.spacing = unit(1.5, "cm")
  )
ggsave("Residuals_vs_Fitted_Values.png", plot = plot3, width = 12, height = 8, dpi = 900)

# Q-Q Plot for normality of residuals
qqnorm(Data$Residuals)
qqline(Data$Residuals, col = "red")

Data <- Data %>%
  mutate(resid = residuals(model_Month_wind_solar))

# 2.1 Select only the columns of interest: the explanatory variables + residuals
df_long <- Data %>%
  select(TE, start_year, wdayindex, DSN, resid) %>%
  # 2.2 Convert the wide format into a "long" format, where each explanatory variable
  #      is identified by "variable", and its value is in "value"
  pivot_longer(
    cols = c(TE, start_year, wdayindex, DSN),
    names_to = "variable",
    values_to = "value"
  )

plot4 <- ggplot(df_long, aes(x = value, y = resid)) +
  geom_point(alpha = 0.35, size = 3, color = "#1f77b4") +
  geom_hline(yintercept = 0, color = "#e74c3c") +
  facet_wrap(~variable, scales = "free_x", ncol = 2) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#2c3e50", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 16, color = "#34495e"),
    axis.text = element_text(color = "#7f8c8d", size = 12),
    strip.text = element_text(size = 14, face = "bold", color = "#2c3e50"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.75),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    plot.margin = margin(40, 40, 40, 40),
    panel.background = element_rect(fill = "#f9f9f9", color = "#dcdcdc", size = 0.5),
    axis.ticks = element_line(color = "#dcdcdc"),
    legend.position = "none",
    panel.spacing = unit(1.5, "cm")
  ) +
  labs(
    title = "Residuals vs. Explanatory Variables",
    x = "Value of Explanatory Variable",
    y = "Model Residuals"
  )
ggsave("Residuals_vs_Explanatory_Variables.png", plot = plot4, width = 12, height = 8, dpi = 900)



hourly_temp <- Temp_data %>%
  mutate(
    DateTime = dmy_hm(Date),    # parse to R's Date-Time format
    date = as.Date(DateTime),        # extract just the date
    hour = hour(DateTime)            # extract the hour (0-23)
  )

# 1.3 Quick check
head(hourly_temp)
str(hourly_temp)

# 2.1 Filter rows where hour == 18 (6pm)
# 2.2 Average across any 6pm rows (e.g., if you have multiple readings per hour, or to handle missing data)
temp_6pm <- hourly_temp %>%
  filter(hour == 18) %>%
  group_by(date) %>%
  summarize(temp_6pm = mean(temp, na.rm = TRUE))

head(temp_6pm)

# 3.1 Ensure Date columns match in both datasets
#     Suppose scs_main$Date is also a Date object
#     If not, convert: scs_main$Date <- as.Date(scs_main$Date)

temp_6pm$date <- as.character(temp_6pm$date)

df_merged <- Data %>%
  left_join(temp_6pm, by = c("Date" = "date"))


plot5 <- ggplot(df_merged, aes(x = temp_6pm, y = demand_gross)) +
  geom_point(aes(color = temp_6pm), alpha = 0.8, size = 4) +
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "solid") +
  labs(
    title = "Daily Demand vs. Temperature at 6pm",
    x = "Temperature at 6 p.m. (°C)",
    y = "Electricity Demand Recorded at 6 p.m. (MW)"
  ) +
  scale_color_gradient(low = "#3498db", high = "#f39c12") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#2c3e50", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 16, color = "#34495e"),
    axis.text = element_text(color = "#7f8c8d", size = 12),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.4),
    plot.margin = margin(40, 40, 40, 40),
    panel.background = element_rect(fill = "#ecf0f1", color = "#dcdcdc", size = 0.5),
    axis.ticks = element_line(color = "#dcdcdc"),
    legend.position = "none"
  ) +
  scale_x_continuous(labels = scales::comma_format(scale = 1)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1))
ggsave("Daily_Demand_vs_Temperature.png", plot = plot5, width = 12, height = 8, dpi = 900)

cor_test <- cor.test(df_merged$temp_6pm, df_merged$demand_gross, use="complete.obs")
cor_test






base_2013_14 <- Data %>%
  filter(start_year == 2013) %>%
  arrange(DSN)  # ensure rows are in chronological order

# 'DSN' presumably is "days since November 1" or similar,
# so this ensures the base data is sorted from DSN=0 to DSN=some max

# ------------------------------------------------------------
# 3) Define a function to simulate demand for 2013–14 if we inject
#    another winter's weather data (TE, wind, solar_S)
# ------------------------------------------------------------
simulate_max_demand_2013 <- function(base_data, alt_weather, model) {
  # base_data: rows for the 2013–14 winter
  # alt_weather: rows for another winter (e.g. 1992–93) with the same DSN alignment
  # model: your fitted lm() model (model_Month)
  
  # 1) Make a copy of base_data
  sim_data <- base_data
  
  # 2) Overwrite TE, wind, solar_S with the alternative weather
  #    IMPORTANT: alt_weather must be sorted & aligned so that row i
  #    matches the same DSN in sim_data row i
  sim_data$TE      <- alt_weather$TE
  # 3) Predict demand using your model
  sim_data$demand_pred <- predict(model, newdata = sim_data)
  
  # 4) Return the maximum predicted demand in this simulated scenario
  return(max(sim_data$demand_pred))
}

# ------------------------------------------------------------
# 4) Example usage: Compare multiple past winters
# ------------------------------------------------------------
# Suppose you want to see how 2013–14 peak demand would differ
# if it had used the weather from start_year = 1991, 1992, 1993, etc.

years_of_interest <- c(1991, 1992, 1993, 1994)  # example set
results <- data.frame()

for (yr in years_of_interest) {
  # 1) Extract the weather data for that winter
  alt_data <- Data %>%
    filter(start_year == yr) %>%
    arrange(DSN)
  
  # 2) Ensure alt_data has the same number of rows as base_2013_14
  #    and that DSN lines up properly
  if (nrow(alt_data) == nrow(base_2013_14)) {
    # 3) Simulate max demand
    max_d <- simulate_max_demand_2013(
      base_data = base_2013_14,
      alt_weather = alt_data,
      model = model_Month_wind_solar
    )
    
    # 4) Store results
    results <- rbind(results,
                     data.frame(
                       AltWeatherYear = yr,
                       SimulatedMaxDemand = max_d
                     )
    )
  } else {
    cat("Skipping year", yr, "- row count mismatch or DSN mismatch.\n")
  }
}

# View results
print(results)

max_2013 <- Data %>%
  filter(start_year == 2013) %>%
  summarize(max_demand_2013 = max(demand_gross, na.rm = TRUE))

print(max_2013)