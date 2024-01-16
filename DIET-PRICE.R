library(sf)
library(spdep)
library(sf)
library(dplyr)
library(ggplot2)
library(priceR)
library(lubridate)

# Read CSV file
file <- read.csv("C:/Users/HP/Desktop/IITA/IITA 2023/IITA2023/122023/data.csv")

# Mutate Food_Group
df <- file %>%
  mutate(Food_Group = case_when(
    Food_Group == "Animal_source"           ~ "Animal Sourced Foods",
    Food_Group == "Fruits"                  ~ "Fruits & Vegetables",
    Food_Group == "Vegetables"              ~ "Fruits & Vegetables",
    Food_Group == "Legumes_nuts_seeds"      ~ "Legumes & Nuts",
    Food_Group == "Oils_fats"               ~ "Oils and Fats",
    Food_Group == "Starchy"                 ~ "Roots/Tubers & Cereals",
    TRUE                                   ~ Food_Group  
  ))


columns_to_drop <- c('Commodity', 'year', 'month', 'Date_Update_2','unique_month_value','Date_Update')
df <- df[, !(names(df) %in% columns_to_drop)]


df$Date <- as.Date(df$Date)

# Filter data for the specified date range
start_date <- as.Date("2021-08-09")
end_date <- as.Date("2022-08-06")

filtered_df <- df %>%
  filter(Date >= start_date, Date <= end_date)

filtered_df <- filtered_df %>%
  mutate(Week = floor_date(Date, "week", week_start = 1)) %>%
  mutate(Week = as.numeric(format(Week, "%U")) + 1)


filtered_df <- filtered_df %>%
  mutate(Week = floor((Date - start_date) / 7) + 1)

filtered_df <- filtered_df %>%
  mutate(Week = paste("Week", Week, sep="_"))



convert_to_usd <- function(price, exchange_rate) {
  return(price / exchange_rate)
}

# Example exchange rates for 2021 and 2022
exchange_rate_2021 <- 988.62  # Replace with your actual exchange rate for 2021
exchange_rate_2022 <- 1030.31  # Replace with your actual exchange rate for 2022

# Assuming 'Date' column is already in Date format
filtered_df$Date <- as.Date(filtered_df$Date)


# Add a 'USD_Price' column
newdf <- filtered_df %>%
  mutate(USD_Price = ifelse(year(Date) == 2021, convert_to_usd(Price, exchange_rate_2021),
                            ifelse(year(Date) == 2022, convert_to_usd(Price, exchange_rate_2022),
                                   NA)))

newdf <- newdf %>%
  group_by(Week,District) %>%
  summarize(mean_price = mean(USD_Price))


diet<- read.csv("C:/Users/HP/Desktop/IITA/IITA 2023/IITA2023/112023/data/DQQ_Rwanda_Dataset_Full_Anon 3.csv")


# Convert "Start_Date" to Date type with the format "YYYY-MM-DD"
diet$Start_Date <- mdy(diet$Start_Date)

# Assuming your dataset is named 'diet'
selected_columns <- diet %>% 
  select(Start_Date,District, Gender, Age.Group, Ubudehe, Sector, GBDR_Score)

# Format "Start_Date" as "YYYY-MM-DD"
selected_columns$Start_Date <- format(selected_columns$Start_Date, "%Y-%m-%d")

# View the updated 'selected_columns' dataframe
print(selected_columns)


# Calculate the mean GBDR score for each district and each category (Ubudehe)
selected_columns <- selected_columns %>%
  group_by(Start_Date,District, Ubudehe) %>%
  summarize(mean_Gbdr_score = mean(GBDR_Score, na.rm = TRUE))


selected_columns$Start_Date <- as.Date(selected_columns$Start_Date)
selected_columns <- selected_columns %>%
  rename(Date = Start_Date)

selected_columns<-selected_columns %>% 
  mutate(District = toupper(District))

# Filter data for the specified date range
start_date <- as.Date("2021-08-09")
end_date <- as.Date("2022-08-06")

selected_columns<- selected_columns %>%
  filter(Date >= start_date, Date <= end_date)

selected_columns <-selected_columns %>%
  mutate(Week = floor_date(Date, "week", week_start = 1)) %>%
  mutate(Week = as.numeric(format(Week, "%U")) + 1)


selected_columns<-selected_columns %>%
  mutate(Week = floor((Date - start_date) / 7) + 1)

selected_columns <- selected_columns %>%
  mutate(Week = paste("Week", Week, sep="_"))


category_summary <- selected_columns %>%
  group_by(Week,District, Ubudehe) %>%
  summarize(mean_Gbdr_score = mean(mean_Gbdr_score, na.rm = TRUE))

# Merge datasets on 'Date' and 'District'
merged_df <- merge(newdf, category_summary, by = c('Week', 'District'))


library(ggplot2)

ggplot(merged_df, aes(x = Week)) +
  geom_line(aes(y = mean_Gbdr_score, color = Ubudehe)) +
  scale_y_continuous(name = "GBDR Score", sec.axis = sec_axis(~., name = "USD Price")) +
  geom_line(aes(y = mean_price, linetype = "USD Price"), size = 1, color = "black") +
  labs(title = "GBDR Score and USD Price Over Time by Ubudehe") +
  theme_minimal() +
  scale_color_manual(values = c("Animal Sourced Foods" = "red", "Plant Sourced Foods" = "blue")) +
  scale_linetype_manual(name = "", values = c("USD Price" = "solid")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))
