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

par(bg = "white")

plot(air_quality_clean$SO2, air_quality_clean$`PM2.5`,
     main = "SO2 vs PM2.5",
     xlab = "SO2 (ppb)", ylab = "PM2.5 (µg/m³)",
     pch = 19, col = "blue")

plot(air_quality_clean$O3, air_quality_clean$`PM2.5`,
     main = "O3 vs PM2.5",
     xlab = "O3 (ppb)", ylab = "PM2.5 (µg/m³)",
     pch = 19, col = "darkgreen")

plot(air_quality_clean$CO, air_quality_clean$`PM2.5`,
     main = "CO vs PM2.5",
     xlab = "CO (ppm)", ylab = "PM2.5 (µg/m³)",
     pch = 19, col = "red")

#correlation between PM2.5 and other numerical variables
num_vars <- c(
  "PM2.5",
  "NO2",
  "O3",
  "CO",
  "SO2",
  "Temperature",
  "Humidity",
  "WindSpeed",
  "PM10"
)

cor <- cor(air_quality_clean[, num_vars]) 
cor_PM25 <- cor["PM2.5", ]

cor_df <- data.frame(
  Variable = names(cor_PM25),
  Correlation = cor_PM25
)

cor_df <- cor_df[order(-cor_df$Correlation), ]
cor_df

pollutants=c("NO2", "O3", "CO", "SO2")

results <- list()
counter <- 1

for (i in 1:length(pollutants)) {
  combos=combn(pollutants, i, simplify=FALSE)
  for (comp in combos) {
    formula_str=paste("PM2.5 ~", paste(comp, collapse = " + "))
    model=lm(as.formula(formula_str), data = air_quality_clean)
    
    results[[counter]]=data.frame(
      Combination = paste(comp, collapse = " + "),
      Adj_R2=summary(model)$adj.r.squared
    )
    counter=counter + 1
  }
}

adj_R2 <- do.call(rbind, results)
adj_R2 <- adj_R2[order(-adj_R2$Adj_R2), ]
print(adj_R2)

model <- lm(PM2.5 ~ O3 + CO + SO2, data=air_quality_clean)
summary(model)

par(bg = "white")

plot(model$fitted.values, model$residuals, 
    main = "Residuals vs Fitted Values",
    xlab="Fitted value", 
    ylab="Residual",
    pch = 19, 
    col = "black")

par(bg = "white")

plot(model, which = 2,
     main = "QQ Plot of Residuals for PM2.5 Model",
     pch = 19,
     col = "blue",
     cex = 1.2)

abline(0, 1, col = "red", lwd = 5)
