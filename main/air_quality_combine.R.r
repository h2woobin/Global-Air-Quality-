library(dplyr)

file_path <- "../data/global_air_quality_dataset.csv"

if(file.exists(file_path)) {
  air_quality <- read.csv(file_path)
  head(air_quality)
} else {
  print("Can't fined the file. Please check the path.")
}

summary(air_quality)

str(air_quality)

#Data cleaning
colnames(air_quality) <- c(
  "Date",
  "City",
  "Country",
  "AQI",
  "PM2.5",
  "PM10",
  "NO2",
  "SO2",
  "CO",
  "O3",
  "Temperature",
  "Humidity",
  "WindSpeed"
)

print("Missing values per column:")
print(colSums(is.na(air_quality)))

air_quality$Date <- as.Date(air_quality$Date)


air_quality_clean <- air_quality 
head(air_quality_clean)
