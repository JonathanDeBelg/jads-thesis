library(fixest)
library(kableExtra)
library(ggplot2)
library(fixest)

data <- read.csv('data/final/def_data_missing_smp.csv', sep=";")

# Prepare the data by converting necessary columns to factors and creating a time variable
data$Year <- as.factor(data$Year)
data$Week <- as.factor(data$Week)
data$time <- interaction(data$Year, data$Week)
data$smp_Total.Interactions <- ifelse(data$smp_Total.Interactions == -1, 0, data$smp_Total.Interactions)

data$Brand_Value <- log(data$smp_Total.Interactions + 1)
data$Persistency <- data$smp_postCount
data$Distinctiveness <- data$smp_cosine_distance
data$Minutes_Played <- data$igp_Min
data$In_game_Performance <- data$igp_Won
data$Google_Searches <- data$trends_Value

data_cleaned <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time")])

## 2.5.1 Social media content strategy and its effect on Brand Value
# persistant branding message leads to higher sme
m1 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency | Player.Name + time, data=data_cleaned)
m2 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned)
m3 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned)


data_cleaned2 <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time", "In_game_Performance")])
## 2.5.2 The moderating role of In game perforamance on the content strategy's impact on Brand Value
## the moderating role of igp on persistant - posting more often when performing well has a positive effect  (persistancy + postcount + persistancy * igp_min)
## the moderating role of igp on distinctiveness (distinctivenss + postcount + distinveness * igp_min)
m4 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency * In_game_Performance + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned2)
m5 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance + Persistency | Player.Name + time, data=data_cleaned2)
m6 <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency * In_game_Performance + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance  | Player.Name + time, data=data_cleaned2)


data_cleaned <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time", "Week")])
m1_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency | Player.Name + time, data=data_cleaned, cluster = c("Player.Name", "Week"))
m2_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned, cluster = c("Player.Name", "Week"))
m3_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned, cluster = c("Player.Name", "Week"))

## 2.5.2 The moderating role of In-game performance on the content strategy's impact on Brand Value
data_cleaned2 <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time", "Week", "In_game_Performance")])

m4_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency * In_game_Performance + Distinctiveness + I(Distinctiveness^2)  | Player.Name + time, data=data_cleaned2, cluster = c("Player.Name", "Week"))
m5_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance + Persistency | Player.Name + time, data=data_cleaned2, cluster = c("Player.Name", "Week"))
m6_week <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency * In_game_Performance + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance | Player.Name + time, data=data_cleaned2, cluster = c("Player.Name", "Week"))

data_cleaned <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time", "Week", "TeamSquad")])
m1_team <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency | Player.Name + time, data=data_cleaned, cluster = "TeamSquad")
m2_team <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned, cluster = "TeamSquad")
m3_team <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned, cluster = "TeamSquad")

data_cleaned2 <- na.omit(data[, c("Brand_Value", "Distinctiveness", "Persistency", "Minutes_Played", "Google_Searches", "Player.Name", "time", "Week", "In_game_Performance", "TeamSquad")])
## 2.5.2 The moderating role of In-game performance on the content strategy's impact on Brand Value
m4_team <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Persistency * In_game_Performance + Distinctiveness + I(Distinctiveness^2) | Player.Name + time, data=data_cleaned2, cluster = "TeamSquad")
m5_team <- feols(Brand_Value ~ Google_Searches + Minutes_Played + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance + Persistency | Player.Name + time, data=data_cleaned2, cluster = "TeamSquad")
m6_team <- feols(Brand_Value ~ Minutes_Played + Persistency * In_game_Performance + Distinctiveness * In_game_Performance + I(Distinctiveness^2) * In_game_Performance | Player.Name + time, data=data_cleaned2, cluster = "TeamSquad")

#etable(m1, m2, m3, m4, m5, m6, m1_week, m2_week, m3_week, m4_week, m5_week, m6_week, m1_team, m2_team, m3_team, m4_team, m5_team, m6_team)
etable(m1, m2, m3, m4, m5, m6, tex=TRUE)
etable(m1_week, m2_week, m3_week, m4_week, m5_week, m6_week, tex=TRUE)
etable(m1_week, m2_week, m3_week)
etable(m4_week, m5_week, m6_week)
etable(m1_team, m2_team, m3_team, m4_team, m5_team, m6_team, tex=TRUE)
etable(m1_team, m2_team, m3_team)
etable(m4_team, m5_team, m6_team)


plot_effect_with_moderator <- function(data, model, variable, moderator, include_squared = FALSE) {
  # Extract the variable names from the model formula
  model_vars <- all.vars(formula(model))
  
  # Filter out non-numeric or non-logical variables
  numeric_vars <- model_vars[sapply(data[model_vars], is.numeric) | sapply(data[model_vars], is.logical)]
  
  # Create a range for the variable of interest
  variable_range <- seq(min(data[[variable]], na.rm = TRUE), max(data[[variable]], na.rm = TRUE), length.out = 100)
  moderator_range <- seq(min(data[[moderator]], na.rm = TRUE), max(data[[moderator]], na.rm = TRUE), length.out = 3)  # Example: 3 levels
  
  # Calculate means for all other variables in the model
  mean_values <- sapply(numeric_vars, function(v) if (v != variable) mean(data[[v]], na.rm = TRUE) else NULL)
  mean_values <- mean_values[!sapply(mean_values, is.null)]  # Remove NULLs
  mean_time <- unique(data$time)[1]
  
  # Create a data frame for predictions for all players
  prediction_data <- expand.grid(
    Player.Name = unique(data$Player.Name),
    time = mean_time,
    In_game_Performance = moderator_range
  )
  
  # Add mean values for all variables except the variable of interest
  for (v in names(mean_values)) {
    prediction_data[[v]] <- mean_values[[v]]
  }
  
  # Replicate the data frame for the range of the variable of interest
  prediction_data <- prediction_data[rep(seq_len(nrow(prediction_data)), each = length(variable_range)), ]
  prediction_data[[variable]] <- rep(variable_range, times = nrow(prediction_data) / length(variable_range))
  
  if (include_squared) {
    prediction_data[[paste0("I(", variable, "^2)")]] <- prediction_data[[variable]]^2
    variable <- sym(paste0("I(", variable, "^2)"))
  }
  
  # Calculate predicted Brand Value
  prediction_data$Predicted_Brand_Value <- predict(model, newdata = prediction_data)

  aggregated_data <- prediction_data %>%
    group_by_at(vars(variable, moderator)) %>%
    summarise(Mean_Predicted_Brand_Value = mean(Predicted_Brand_Value, na.rm = TRUE))
  print(aggregated_data)
  # Plot the variable of interest against the aggregated predicted Brand Value
  ggplot(aggregated_data, aes_string(x = variable, y = "Mean_Predicted_Brand_Value", color = moderator)) +
    geom_line(size = 1) +
    labs(title = paste("Effect of", variable, "on Brand Value by", moderator),
         x = variable,
         y = "Mean Predicted Brand Value") +
    theme_minimal()
  
}

plot_effect_with_moderator(data_cleaned2, m4, "Persistency", "In_game_Performance")

# Plot for Distinctiveness
plot_effect_on_brand_value(data_cleaned, m2, "Distinctiveness")

plot_effect_on_brand_value(data_cleaned, m2, "Distinctiveness", include_squared = TRUE)

plot_effect_on_brand_value(data_cleaned2, m4, "In_game_Performance")
