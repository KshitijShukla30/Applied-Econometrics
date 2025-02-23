library(readr)
library(dplyr)
library(GGally)
library(ggplot2)
library(reshape2)
library(lmtest)
library(sandwich)
library(car)
library(xtable)
library(stargazer)


#importing dataset 
data <- read_csv("C:/Users/R TARUN KUMAR/Downloads/hour.csv")
View(data)


#graph matrix
numeric_df <- data[, c("cnt", "atemp", "hum", "windspeed")]
ggpairs(numeric_df)
ggpairs(numeric_df, lower = list(continuous = wrap("points", color = "blue", alpha = 0.5)))


###################graphs to understand dataset#######################################
#scatterplot between atemp and count(cnt)
ggplot(data, aes(x = atemp, y = cnt)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of atemp vs cnt", 
       x = "Feeling Temperature (atemp)", 
       y = "Total Bike Rentals (cnt)") +
  theme_minimal()


#scatterplot between humidity and count
ggplot(data, aes(x = hum, y = cnt)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of humidity vs cnt", 
       x = "Humidity (hum)", 
       y = "Total Bike Rentals (cnt)") +
  theme_minimal()


#scatterplot between windspeed and count
ggplot(data, aes(x = windspeed, y = cnt)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of windspeed vs cnt", 
       x = "WindSpeed (windspped)", 
       y = "Total Bike Rentals (cnt)") +
  theme_minimal()


#average of bike rentals based on working day
aggregate_cnt <- aggregate(cnt ~ workingday, data = data, FUN = mean)
ggplot(aggregate_cnt, aes(x = factor(workingday), y = cnt, fill = factor(workingday))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Bike Rentals by Working Day",
       x = "Working Day (0 = No, 1 = Yes)",
       y = "Average Bike Rentals") +
  theme_minimal()


#Histogram of total bike rentals(count)
ggplot(data, aes(x = cnt)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Bike Rentals", x = "Total Bike Rentals (cnt)", y = "Frequency") +
  theme_minimal()


#histogram of bike rentals based on working day
ggplot(data, aes(x = cnt, fill = factor(workingday))) +
  geom_histogram(binwidth = 50, alpha = 0.5, position = "identity", color = "black") +
  labs(title = "Histogram of Bike Rentals by Working Day", 
       x = "Total Bike Rentals (cnt)", 
       y = "Frequency", 
       fill = "Working Day (0 = No, 1 = Yes)") +
  theme_minimal()


#actual values of bike rentals
ggplot(data, aes(x = dteday, y = cnt)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily Bike Rentals Over Time", 
       x = "", y = "Total Rentals (cnt)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#Average Bike rentals classified month wise
monthly_avg <- aggregate(cnt ~ mnth, data = data, FUN = mean)
ggplot(monthly_avg, aes(x = mnth, y = cnt)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  labs(title = "Average Monthly Bike Rentals", x = "Month", y = "Average Total Rentals") +
  theme_minimal()


#Average of Bike rentals classfied season wise
seasonal_avg <- aggregate(cnt ~ season, data = data, FUN = mean)
ggplot(seasonal_avg, aes(x = factor(season, levels = 1:4, 
                                    labels = c("Winter", "Spring", "Summer", "Fall")), 
                         y = cnt)) +
  geom_line(group = 1, color = "green", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Seasonal Bike Rentals", x = "Season", y = "Average Total Rentals") +
  theme_minimal()


#boxplot of count with season
ggplot(data, aes(x = factor(season, levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall")), y = cnt)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Bike Rentals by Season", 
       x = "Season", 
       y = "Total Rentals (cnt)") +
  theme_minimal()


#boxplot of count with weather situation(weathersit)
ggplot(data, aes(x = factor(weathersit, levels = 1:4, 
                            labels = c("Clear/Few Clouds", "Mist/Cloudy", "Light Snow/Rain", "Very Cloudy")), y = cnt)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Bike Rentals by Weather Situation", 
       x = "Weather Situation", 
       y = "Total Rentals (cnt)") +
  theme_minimal()


#boxplot of count with hour
ggplot(data, aes(x = factor(hr), y = cnt)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Bike Rentals by Hour of the Day", 
       x = "Hour of the Day", 
       y = "Total Rentals (cnt)") +
  theme_minimal()


#boxplot of count with workingday
ggplot(data, aes(x = factor(workingday, labels = c("Non-Working Day", "Working Day")), y = cnt)) +
  geom_boxplot(fill = "gold") +
  labs(title = "Bike Rentals: Working Day vs Non-Working Day", 
       x = "Working Day(0 = No, 1 = Yes)", 
       y = "Total Rentals (cnt)") +
  theme_minimal()


#boxplot of count with holiday
ggplot(data, aes(x = factor(holiday, labels = c("Non-Holiday", "Holiday")), y = cnt)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Bike Rentals: Holiday vs Non-Holiday", 
       x = "Holiday", 
       y = "Total Rentals (cnt)") +
  theme_minimal()


#Correlation matrix
num_vars <- data[, c("cnt", "atemp", "hum", "windspeed")]
cor_matrix <- cor(num_vars)
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Create heatmap tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +  # Show correlation values
  labs(title = "Heatmap of Correlations Between Numerical Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Interaction plot of count with temperature and season
ggplot(data, aes(x = atemp, y = cnt, color = factor(season))) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Bike Rentals vs Temperature by Season", 
       x = "Temperature (Normalized)", 
       y = "Total Rentals (cnt)", 
       color = "Season") +
  scale_color_manual(values = c("blue", "green", "orange", "red"), 
                     labels = c("Winter", "Spring", "Summer", "Fall")) +
  theme_minimal()


#Interaction plot of count with hour and count
ggplot(data, aes(x = factor(hr), y = cnt, color = factor(workingday))) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Bike Rentals vs Hour by Working Day", 
       x = "Hour of the Day", 
       y = "Total Rentals (cnt)", 
       color = "Working Day") +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Non-Working Day", "Working Day")) +
  theme_minimal()


#Interaction plot of count with weather and season
ggplot(data, aes(x = factor(weathersit), y = cnt, color = factor(season))) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Bike Rentals vs Weather by Season", 
       x = "Weather Situation", 
       y = "Total Rentals (cnt)", 
       color = "Season") +
  scale_x_discrete(labels = c("1" = "Clear", "2" = "Mist/Cloudy", "3" = "Light Snow/Rain")) +
  scale_color_manual(values = c("blue", "green", "orange", "red"), 
                     labels = c("Winter", "Spring", "Summer", "Fall")) +
  theme_minimal()


#Histogram of count with density plot
ggplot(data, aes(x = cnt)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "blue", color = "black", alpha = 0.5) +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Histogram & Density Plot of Bike Rentals", 
       x = "Total Rentals (cnt)", 
       y = "Density") +
  theme_minimal()


#Density of count
ggplot(data, aes(x = cnt)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot of Bike Rentals", 
       x = "Total Rentals (cnt)", 
       y = "Density") +
  theme_minimal()


#####################Making the regression dataset(reg_data)#################
#categorising hour into 4 categories - 0-5 ->1, 6-11 -> 2, 12-17 -> 3, 18-23 ->4
data <- data %>%
  mutate(hr_cat = case_when(
    hr >= 0 & hr <= 5  ~ 1,
    hr >= 6 & hr <= 11 ~ 2,
    hr >= 12 & hr <= 17 ~ 3,
    hr >= 18 & hr <= 23 ~ 4
  ))


#dropping redundant variables, making required log transformations, and factors of categorical variables
data <- data %>%
  mutate(time_index = row_number()) %>%
  select(-'dteday',-'yr') 
View(data)
reg_data <- data %>% select(-c( 'mnth', 'weekday','holiday', 'temp', 'casual', 'registered'))
View(reg_data)

reg_data$season <- as.factor(reg_data$season)
reg_data$workingday <- as.factor(reg_data$workingday)
reg_data$weathersit <- as.factor(reg_data$weathersit)
reg_data$hr_cat <- as.factor(reg_data$hr_cat)
reg_data$log_cnt <- log(reg_data$cnt + 1)
reg_data$log_windspeed <- log(reg_data$windspeed + 1)
reg_data$log_hum <- log(reg_data$hum + 1)


#model 1-> no transformations and non-linear terms
model_basic <- lm(
  cnt ~ hr_cat+
    windspeed + hum +
    season + workingday +
    weathersit + atemp,
  data = reg_data
)
summary(model_basic)


#model 2 -> non linear and interaction terms on independent variables
model_nonlinear<- lm(
  cnt ~ hr_cat+
    windspeed + I(windspeed^2) + hum +
    atemp*season +
    hum*atemp+ 
    season + workingday +
    weathersit + atemp + I(atemp^2) +
    I(atemp^3),
  data = reg_data
)
summary(model_nonlinear)


#model 3 -> actual mode. log transformation on cnt with all other transformations
#we will consider this model for analysis going ahead
model <- lm(
  log_cnt ~ hr_cat+
    windspeed + I(windspeed^2) + hum +
    atemp*season +
    hum*atemp+ #hr_cat*atemp +
    season + workingday +
    weathersit + atemp + I(atemp^2) +
    I(atemp^3),
  data = reg_data
)
summary(model)


#actual vs predicted values by model
reg_data$predicted <- predict(model, newdata = reg_data)
reg_data$actual <- exp(reg_data$log_cnt)
reg_data$predicted_original <- exp(reg_data$predicted)
reg_data$index <- 1:nrow(reg_data)
ggplot(reg_data, aes(x = index)) + 
  geom_line(aes(y = actual, color = "Actual"), size = 1) + 
  geom_line(aes(y = predicted_original, color = "Predicted"), size = 1, linetype = "dashed") + 
  labs(title = "Actual vs Predicted Count",
       x = "Index",
       y = "Count",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


#model diagnostics
model_diag <- fortify(model)
  #residual vs fitted values
ggplot(model_diag, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()
  # Normal Q-Q plot
ggplot(model_diag, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical quantiles of normal distribution", y= "Quantiles of observed residuals") +
  theme_minimal()


#barplot of coeffcients
standardized_coefs <- function(model) {
  coefs <- coef(model)
  if ("(Intercept)" %in% names(coefs)) {
    coefs <- coefs[-1]
  }
  model_matrix <- model.matrix(model)
  matching_vars <- intersect(names(coefs), colnames(model_matrix))
  if (length(matching_vars) == 0) {
    stop("No matching variables found between coefficients and model matrix.")
  }
  sds <- apply(model_matrix[, matching_vars, drop = FALSE], 2, sd, na.rm = TRUE)
  std_coefs <- coefs[matching_vars] * sds / sd(model$model[[1]], na.rm = TRUE)
  data.frame(
    Variable = names(std_coefs),
    Coefficient = std_coefs
  ) %>% arrange(desc(abs(Coefficient)))
}
coef_df <- standardized_coefs(model)
ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Standardized Coefficients",
       x = "Predictor Variables", y = "Standardized Coefficient") +
  theme_minimal()


#cumilative frequency of count
ggplot(reg_data, aes(x = cnt)) +
  geom_histogram(aes(y = cumsum(..count..)), bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  labs(
    title = "Cumulative Frequency of Daily Rentals",
    x = "Daily Rentals (cnt)",
    y = "Cumulative Count"
  ) +
  theme_minimal()


#model misspecification test
reset_test <- resettest(model, power = 2, type = "fitted")
print(reset_test)
  #we fail to reject the null hypothesis at the 5% significance level
  #This implies no strong evidence of model misspecification,


#Residuals are normally distributed test
residuals_data <- data.frame(residuals = residuals(model))
ggplot(residuals_data, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.6, color = "black") +  # Histogram
  geom_density(color = "red", size = 0.75) +  # Density line
  labs(
    title = "Kernel Density Estimate (KDE) of Residuals",
    x = "Residuals",
    y = "Density"
  ) +
  theme_minimal()
  #The model is slightly negatively skewed but we can consider the residuals normally distributed


#Homoskedasticity test
bp_test <- bptest(model)
print(bp_test)
  #there is significant evidence of heteroskedasticity in our model, so using robust standard errors
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
print(robust_se)


#multicollinearity test
  # vif_values <- vif(model)
  # print(vif_values)   When interactions are present, multicollinearity will be increased due these terms
vif_values <- vif(model, type = "predictor")
print(vif_values)
  #hum has very high vif value indicating collinearity to solve this we drop the hum*atemp term
  #because it is not significant and resulting in collinearity. standardizing or centering it wont
  #help unless this term is dropped
model_without_multicollinearity <- lm(  log_cnt ~ hr_cat+
                windspeed + I(windspeed^2) + hum +
                atemp*season +
                #hum_centered*atemp+ #hr_cat*atemp +
                season + workingday +
                weathersit + atemp + I(atemp^2) +
                I(atemp^3),
              data = reg_data)
vif_values_af <- vif(model_without_multicollinearity, type = "predictor")
print(vif_values_af)


#autocorrelation test
dw_test <- dwtest(model)
print(dw_test)
  #small p-value rejects the null hypothesis (no autocorrelation).
  #This implies that autocorrelation is present and statistically significant.
reg_data <- reg_data %>%
  arrange(time_index) %>%  # Ensure order
  mutate(log_cnt_lag = lag(log_cnt)) %>%
  select(-time_index)  # Drop index after use
View(reg_data)

model_after_ac<- lm(
  log_cnt ~ log_cnt_lag + hr_cat +
    windspeed + I(windspeed^2) + hum +
    atemp * season +
    hum * atemp + 
    season + workingday +
    weathersit + atemp + I(atemp^2) +
    I(atemp^3),
  data = reg_data,
  na.action = na.exclude 
)
summary(model_after_ac)
dw_test_af <- dwtest(model_after_ac)
print(dw_test_af)


reg_data$predicted <- predict(model_after_ac, newdata = reg_data)
reg_data$actual <- exp(reg_data$log_cnt)  
reg_data$predicted_original <- exp(reg_data$predicted)
reg_data$index <- 1:nrow(reg_data)
ggplot(reg_data, aes(x = index)) + 
  geom_line(aes(y = actual, color = "Actual"), size = 1) + 
  geom_line(aes(y = predicted_original, color = "Predicted"), size = 1, linetype = "dashed") + 
  labs(title = "Actual vs Predicted Count",
       x = "Index",
       y = "Count",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


#######################################  LATEX PLOTS  #####################################

# Robust Standard Errors to data frame
robust_se_df <- data.frame(
  Estimate = robust_se[, 1],
  "Std. Error" = robust_se[, 2],
  "t value" = robust_se[, 3],
  "Pr(>|t|)" = robust_se[, 4]
)
rownames(robust_se_df) <- rownames(robust_se)

# Convert BP test results to data frame
bp_test_df <- data.frame(
  "Test Statistic" = bp_test$statistic,
  "p-value" = bp_test$p.value
)
rownames(bp_test_df) <- "Breusch-Pagan"

# Convert DW test results to data frame before correction
dw_test_df <- data.frame(
  "Test Statistic" = dw_test$statistic,
  "p-value" = dw_test$p.value
)
rownames(dw_test_df) <- "Durbin-Watson"

# Second Durbin-Watson test results after correction
dw_test_df1 <- data.frame(
  "Test Statistic" = dw_test_af$statistic,
  "p-value" = dw_test_af$p.value
)
rownames(dw_test_df1) <- "Durbin-Watson"

# RESET test results
reset_test_df <- data.frame(
  "Test Statistic" = reset_test$statistic,
  "p-value" = reset_test$p.value
)
rownames(reset_test_df) <- "RESET-TEST"

#vif results before correction
gvif_df <- as.data.frame(vif_values)
gvif_df$Predictor <- rownames(vif_values)
gvif_df <- gvif_df[, c("Predictor", colnames(vif_values))]

#vif results after correction
gvif_df_af <- as.data.frame(vif_values_af)
gvif_df_af$Predictor <- rownames(vif_values_af)
gvif_df_af <- gvif_df_af[, c("Predictor", colnames(vif_values_af))]

sink("test_results.tex")

cat("\\section*{Robust Standard Errors}\n")
print(xtable(robust_se_df), type = "latex", comment = FALSE)
cat("\n\\section*{Breusch-Pagan Test}\n")
print(xtable(bp_test_df), type = "latex", comment = FALSE)
cat("\n\\section*{Durbin-Watson Test}\n")
print(xtable(dw_test_df), type = "latex", comment = FALSE)
cat("\n\\section*{Durbin-Watson Test}\n")
print(xtable(dw_test_df1), type = "latex", comment = FALSE)
cat("\n\\section*{Reset Test}\n")
print(xtable(reset_test_df), type = "latex", comment = FALSE)
cat("\n\\section*{GVIF Table}\n")
print(xtable(gvif_df), type = "latex", comment = FALSE)
cat("\n\\section*{GVIF Table}\n")
print(xtable(gvif_df_af), type = "latex", comment = FALSE)

sink()


#model comparision between basic, nonlinear, actual
stargazer(model_basic, model_nonlinear, model,
          title = "Regression Results",
          label = "tab:regression_results",
          align = TRUE,
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          dep.var.labels = c("Dependent Variable: cnt", "Dependent Variable: log_cnt"),
          model.names = FALSE,
          column.labels = c("Basic Model", "Nonlinear Model", "Full Model"),
          out = "regression_results.tex")


save.image(file = "workspace.RData")


