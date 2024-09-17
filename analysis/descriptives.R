# install.packages(modelsummary)
# install.packages("stargazer")

library(tidyverse)
library(bootcamp)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(knitr)
library(stargazer)
## RAW DATA

### GOOGLE
google_raw_data <- read.csv('data/google-trends/missing_data_values.csv')
# Round the numerical values as required
google_raw_data$Interest <- round(google_raw_data$Interest, 2)
google_raw_data$Year <- round(google_raw_data$Year, 0)
google_raw_data$Week <- round(google_raw_data$Week, 0)

# Generate summary statistics
summary_stats <- data.frame(
  Variable = c("Interest", "Year", "Week", "Player.Name"),
  Min = c(round(min(google_raw_data$Interest), 2), round(min(google_raw_data$Year), 0), round(min(google_raw_data$Week), 0), NA),
  Q1 = c(round(quantile(google_raw_data$Interest, 0.25), 2), round(quantile(google_raw_data$Year, 0.25), 0), round(quantile(google_raw_data$Week, 0.25), 0), NA),
  Median = c(round(median(google_raw_data$Interest), 2), round(median(google_raw_data$Year), 0), round(median(google_raw_data$Week), 0), NA),
  Mean = c(round(mean(google_raw_data$Interest), 2), round(mean(google_raw_data$Year), 0), round(mean(google_raw_data$Week), 0), NA),
  Q3 = c(round(quantile(google_raw_data$Interest, 0.75), 2), round(quantile(google_raw_data$Year, 0.75), 0), round(quantile(google_raw_data$Week, 0.75), 0), NA),
  Max = c(round(max(google_raw_data$Interest), 2), round(max(google_raw_data$Year), 0), round(max(google_raw_data$Week), 0), NA),
  Unique = c(NA, NA, NA, length(unique(google_raw_data$Player.Name))),
  Most_Frequent = c(NA, NA, NA, names(sort(table(google_raw_data$Player.Name), decreasing=TRUE)[1]))
)

# Ensure NA values are represented as empty strings for the categorical summary rows
summary_stats <- summary_stats %>% 
  mutate(
    Min = ifelse(is.na(Min), "", Min),
    Q1 = ifelse(is.na(Q1), "", Q1),
    Median = ifelse(is.na(Median), "", Median),
    Mean = ifelse(is.na(Mean), "", Mean),
    Q3 = ifelse(is.na(Q3), "", Q3),
    Max = ifelse(is.na(Max), "", Max)
  )

# Convert to character to preserve formatting
summary_stats <- summary_stats %>%
  mutate(across(Min:Max, as.character))

# Generate the table in text format
stargazer(summary_stats, type = "text", title = "Summary Statistics of Google Raw Data", summary = FALSE, rownames = FALSE)

# Generate the table in LaTeX format
stargazer(summary_stats, type = "latex", title = "Summary Statistics of Google Raw Data", summary = FALSE, rownames = FALSE)

### SOCIAL MEDIA
social_media_raw_data <- read.csv('data/instagram/apify/all_posts_eredivisie_players_full.csv')

# Format the data
social_media_raw_data$likesCount <- round(social_media_raw_data$likesCount, 2)
social_media_raw_data$commentsCount <- round(social_media_raw_data$commentsCount, 2)
social_media_raw_data$timestamp <- as.POSIXct(social_media_raw_data$timestamp)
social_media_raw_data$timestamp_formatted <- format(social_media_raw_data$timestamp, "%d-%m-%Y %H:%M:%S")

# Generate summary statistics
summary_stats <- data.frame(
  Variable = c("likesCount", "commentsCount", "ownerUsername", "ownerFullName", "caption", "timestamp"),
  Min = c(min(social_media_raw_data$likesCount, na.rm = TRUE), min(social_media_raw_data$commentsCount, na.rm = TRUE), NA, NA, NA, format(min(social_media_raw_data$timestamp), "%d-%m-%Y")),
  Q1 = c(quantile(social_media_raw_data$likesCount, 0.25, na.rm = TRUE), quantile(social_media_raw_data$commentsCount, 0.25, na.rm = TRUE), NA, NA, NA, format(quantile(social_media_raw_data$timestamp, 0.25, na.rm = TRUE), "%d-%m-%Y")),
  Median = c(median(social_media_raw_data$likesCount, na.rm = TRUE), median(social_media_raw_data$commentsCount, na.rm = TRUE), NA, NA, NA, format(median(social_media_raw_data$timestamp, na.rm = TRUE), "%d-%m-%Y")),
  Mean = c(mean(social_media_raw_data$likesCount, na.rm = TRUE), mean(social_media_raw_data$commentsCount, na.rm = TRUE), NA, NA, NA, format(mean(social_media_raw_data$timestamp, na.rm = TRUE), "%d-%m-%Y")),
  Q3 = c(quantile(social_media_raw_data$likesCount, 0.75, na.rm = TRUE), quantile(social_media_raw_data$commentsCount, 0.75, na.rm = TRUE), NA, NA, NA, format(quantile(social_media_raw_data$timestamp, 0.75, na.rm = TRUE), "%d-%m-%Y")),
  Max = c(max(social_media_raw_data$likesCount, na.rm = TRUE), max(social_media_raw_data$commentsCount, na.rm = TRUE), NA, NA, NA, format(max(social_media_raw_data$timestamp), "%d-%m-%Y")),
  Unique = c(NA, NA, length(unique(social_media_raw_data$ownerUsername)), length(unique(social_media_raw_data$ownerFullName)), length(unique(social_media_raw_data$caption)), NA),
  Most_Frequent = c(NA, NA, names(sort(table(social_media_raw_data$ownerUsername), decreasing = TRUE)[1]), names(sort(table(social_media_raw_data$ownerFullName), decreasing = TRUE)[1]), NA, NA)
)

# Generate the table in text format
stargazer(summary_stats, type = "text", title = "Summary Statistics of Social Media Raw Data", summary = FALSE, rownames = FALSE)

# Generate the table in LaTeX format
stargazer(summary_stats, type = "latex", title = "Summary Statistics of Social Media Raw Data", summary = FALSE, rownames = FALSE)

### IGP DATA

new_data <- read.csv('data/player-stats/def_match_data.csv')
new_data$Min <- ifelse(new_data$Min == "Match Report" | is.na(new_data$Min), 0, new_data$Min)
new_data$Min <- as.numeric(new_data$Min)

numerical_vars <- c("Min", "Gls", "Ast", "PK", "PKatt", "Sh", "SoT", "CrdY", "CrdR", "Touches", "Tkl", "Int", "Blocks", "xG", "npxG", "xAG", "SCA", "GCA", "Cmp", "Cmp.", "PrgP", "Carries", "PrgC", "Pass.Attempted", "Succ", "TakeOns.Attempted")
new_data[numerical_vars] <- round(new_data[numerical_vars], 2)
# Separate numeric and non-numeric variables
numeric_summary <- sapply(new_data[, numerical_vars], function(x) {
  if (is.numeric(x)) {
    return(c(min(x, na.rm = TRUE), quantile(x, 0.25, na.rm = TRUE), median(x, na.rm = TRUE), round(mean(x, na.rm = TRUE),2), quantile(x, 0.75, na.rm = TRUE), max(x, na.rm = TRUE)))
  } else {
    return(rep(NA, 6))  # Placeholder for non-numeric variables
  }
})

categorical_summary <- sapply(new_data[, !names(new_data) %in% numerical_vars], function(x) {
  if (!is.numeric(x)) {
    return(c(length(unique(x)), names(sort(table(x), decreasing = TRUE)[1])))
  } else {
    return(rep(NA, 2))  # Placeholder for numeric variables
  }
})

# Combine summary statistics
summary_stats <- data.frame(
  Variable = c(numerical_vars, names(categorical_summary)),
  Min = numeric_summary[1, ],
  Q1 = numeric_summary[2, ],
  Median = numeric_summary[3, ],
  Mean = numeric_summary[4, ],
  Q3 = numeric_summary[5, ],
  Max = numeric_summary[6, ],
  Unique = categorical_summary[1, ]
)

# Ensure NA values are represented as empty strings for the categorical summary rows
summary_stats <- summary_stats %>% 
  mutate(
    Min = ifelse(is.na(Min), "", Min),
    Q1 = ifelse(is.na(Q1), "", Q1),
    Median = ifelse(is.na(Median), "", Median),
    Mean = ifelse(is.na(Mean), "", Mean),
    Q3 = ifelse(is.na(Q3), "", Q3),
    Max = ifelse(is.na(Max), "", Max),
    Unique = ifelse(is.na(Unique), "", Unique),
    Most_Frequent = ifelse(is.na(Most_Frequent), "", Most_Frequent)
  )

# Convert to character to preserve formatting
summary_stats <- summary_stats %>%
  mutate(across(Min:Max, as.character))

# Generate the table in text format
stargazer(summary_stats, type = "text", title = "Summary Statistics of New Data", summary = FALSE, rownames = FALSE)

# Generate the table in LaTeX format
stargazer(summary_stats, type = "latex", title = "Summary Statistics of New Data", summary = FALSE, rownames = FALSE)

## FINAL DATASET
data <- read.csv('data/final/def_data_missing_smp.csv', sep = ";")

# Generate descriptive statistics using bootcamp package
summary_stats <- data.frame(
  Variable = colnames(data),
  sd = sapply(data, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
  min = sapply(data, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
  max = sapply(data, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
  median = sapply(data, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
  range = sapply(data, function(x) if(is.numeric(x)) diff(range(x, na.rm = TRUE)) else NA),
  mad = sapply(data, function(x) if(is.numeric(x)) mad(x, na.rm = TRUE) else NA)
)

# Format the numbers to make them more readable
summary_stats[] <- lapply(summary_stats, function(x) {
  if (is.numeric(x)) {
    if (any(x %% 1 != 0)) {
      round(x, 2)
    } else {
      round(x, 0)
    }
  } else {
    x
  }
})

# Create the summary statistics table using stargazer
stargazer(summary_stats, type = "text", title = "Summary Statistics of the Final Dataset", summary = FALSE, rownames = FALSE)

# Create the summary statistics table in LaTeX format using stargazer
stargazer(summary_stats, type = "latex", title = "Summary Statistics of the Final Dataset", summary = FALSE, rownames = FALSE)


# Create the summary statistics table in LaTeX format using stargazer
stargazer(selected_stats, type = "latex", title = "Summary Statistics of the Final Dataset", summary = FALSE, rownames = FALSE)


writeLines(df, 'analysis/plots/df.tex')

data <- data %>%
  mutate(YearWeek = paste(Year, sprintf("%02d", Week), sep = "-"))

# Convert YearWeek to a factor to maintain the order in the plot
data$YearWeek <- factor(data$YearWeek, levels = unique(data$YearWeek[order(data$Year, data$Week)]))

data <- data %>%
  mutate(YearWeek = Year + Week / 52)


bootcamp::descriptives(data)

ggplot(data, aes(x = YearWeek, y = smp_Total.Interactions)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Linear Growth of Social Media Engagement Over Time",
       x = "Year-Week",
       y = "Total Interactions")


ggplot2::ggplot(data, ggplot2::aes(x = smp_Total.Interactions)) +
  ggplot2::geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  ggplot2::labs(title = "Distribution of Social Media Engagement",
                x = "Total Interactions",
                y = "Frequency")


ggplot2::ggplot(data, ggplot2::aes(x = smp_postCount, y = smp_Total.Interactions)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggplot2::labs(title = "Frequency of Posting vs Social Media Engagement",
                x = "Number of Posts",
                y = "Total Interactions")


ggplot2::ggplot(data, ggplot2::aes(x = content_diversity, y = smp_Total.Interactions)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggplot2::labs(title = "Content Diversity vs Social Media Engagement",
                x = "Content Diversity",
                y = "Total Interactions")


ggplot2::ggplot(data, ggplot2::aes(x = igp_Match.Count, y = smp_Total.Interactions)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggplot2::labs(title = "In-Game Performance vs Social Media Engagement",
                x = "Number of Matches Played",
                y = "Total Interactions")

bootcamp::corrMat(numeric_data)
