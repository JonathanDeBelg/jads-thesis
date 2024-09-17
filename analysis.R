# install.packages('tidyverse')
# install.packages(c("tidyverse", "ggplot2", "dplyr", "kableExtra"))
# install.packages("fixest", "readr")
# install.packages(modelsummary)
library(tidyverse)
library(bootcamp)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(knitr)

library(fixest)
library(readr)

data <- read.csv('data/final/def_data_missing_smp.csv', sep=";")

# Display the structure of the data
str(data)

# Prepare the data by converting necessary columns to factors and creating a time variable
data$Year <- as.factor(data$Year)
data$Week <- as.factor(data$Week)
data$time <- interaction(data$Year, data$Week)
data$smp_Total.Interactions <- ifelse(data$smp_Total.Interactions == -1, 0, data$smp_Total.Interactions)

data$`Brand Value` <- data$smp_Total.Interactions

### HYPOTHESIS 1. A persistant branding message across social media platforms #NOTE!!
### leads to better social media engagement (SME) for athletes. 

# Drop rows with missing values in the relevant columns
data_cleaned <- na.omit(data[, c("smp_Total.Interactions", "smp_postCount", "Player.Name", "time", "igp_Min", "trends_Value")])
hist(log(data$smp_Total.Interactions + 1), main="Distribution of Log Social Media Engagement", xlab = "Log Total interactions/SME")
hist(data$smp_Total.Interactions, breaks=100, main="Distribution of Social Media Engagement", xlab = "Total interactions/SME")

# Run the two-way fixed effects model
h1_model_ols <- feols(log(smp_Total.Interactions + 1) ~ smp_postCount + igp_Min * trends_Value | `Player.Name` + time, data = data_cleaned)
h1_model_pos <- fepois(smp_Total.Interactions ~ smp_postCount + igp_Min * trends_Value | `Player.Name` + time, data = data_cleaned)
h1_model_negbin <- fenegbin(smp_Total.Interactions ~ smp_postCount + igp_Min * trends_Value | `Player.Name` + time, data = data_cleaned)
etable(h1_model_ols, h1_model_pos, h1_model_negbin)
summary(h1_model_negbin)
## NOTE remove interaction with control variables 

### HYPOTHESIS 2. Positive in-game performance, such as matches won and minutes played, 
### leads to increased SME for professional athletes


  
# Drop rows with missing values in the relevant columns
h2_data_cleaned <- na.omit(data[, c("smp_Total.Interactions", "smp_postCount", "Player.Name", "time", "igp_Min", "igp_Won", "igp_Started", "igp_Match.Count", "trends_Value")])

# Run the two-way fixed effects model
h2_model_ols <- feols(log(smp_Total.Interactions + 1) ~ igp_Min + igp_Match.Count * igp_Won | `Player.Name` + time, data = h2_data_cleaned)
h2_model_pos <- fepois(smp_Total.Interactions ~ igp_Min + igp_Match.Count * igp_Won | `Player.Name` + time, data = h2_data_cleaned)
h2_model_negbin<- fenegbin(smp_Total.Interactions ~ igp_Min + igp_Match.Count * igp_Won | `Player.Name` + time, data = h2_data_cleaned)
etable(h2_model_ols, h2_model_pos, h2_model_negbin)
summary(h2_model_negbin)

### HYPOTHESIS 3. A diverse social media content strategy, incorporating both 
### authentic and varied content, significantly enhances SME and visibility
### of athletes among their followers.

data_cleaned3 <- na.omit(data[, c("smp_Total.Interactions", "smp_cosine_distance", "smp_type_Image", "smp_type_Sidecar", "smp_type_Video", "smp_postCount", "Player.Name", "time")])
#I(smp_cosine_similarity^2)
h3_model_ols <- feols(log(smp_Total.Interactions + 1) ~ smp_cosine_similarity * (smp_type_Image + smp_type_Sidecar + smp_type_Video) | Player.Name + time, data = data_cleaned3.1)
h3_model_pos <- fepois(smp_Total.Interactions ~ smp_cosine_similarity * (smp_type_Image + smp_type_Sidecar + smp_type_Video)  | Player.Name + time, data = data_cleaned3.1)
h3_model_negbin <- fenegbin(smp_Total.Interactions ~ smp_cosine_similarity * (smp_type_Image + smp_type_Sidecar + smp_type_Video) | Player.Name + time, data = data_cleaned3.2)
etable(h3_model_ols, h3_model_pos, h3_model_negbin)
summary(h3_model_negbin)
## NOTE: Test interactions independently
