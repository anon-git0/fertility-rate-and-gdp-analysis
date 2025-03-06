library(ggplot2)

# The first 4 rows of the imported csv files do not contain data.
# skip = 4 skips importing the first 4 rows.
fertility_data <- read.csv("fertility.csv", skip = 4)
gdp_data <- read.csv("gdp_per_capita.csv", skip = 4)

# Remove all columns except for the country name and the 2022 data.
fertility_data <- fertility_data[, c(1, 67)]
gdp_data <- gdp_data[, c(1, 67)]

# Set up the column names.
colnames(fertility_data)[1] <- "country"
colnames(fertility_data)[2] <- "fertility_2022"
colnames(gdp_data)[1] <- "country"
colnames(gdp_data)[2] <- "gdp_2022"

# Data cleaning - remove rows with things that are clearly not countries (e.g. groups of countries)
# Note: 'africa ' is used rather than 'africa' to prevent the removal of South Africa.
fertility_data <- fertility_data[!grepl("euro|asia|africa |world|demog|ida |income|small|coun|conflict|sahara|ibrd|class|oecd", fertility_data$country, ignore.case = TRUE), ]
gdp_data <- gdp_data[!grepl("euro|asia|africa |world|demog|ida |income|small|coun|conflict|sahara|ibrd|class|oecd", gdp_data$country, ignore.case = TRUE), ]

# Merge the two datasets into one, combining rows by country name.
fert_gdp <- merge(fertility_data, gdp_data, by = "country")

# Remove any rows with any N/A entries.
fert_gdp <- na.omit(fert_gdp)

# Scatter plot for fertility rate versus GDP per capita.
plot(fert_gdp$fertility_2022, fert_gdp$gdp_2022,
     main = "Fertility rate vs GDP per capita",
     xlab = "Fertility rate", ylab = "GDP per capita")

# Pearson correlation for fertility rate versus GDP per capita.
cor(fert_gdp$fertility_2022, fert_gdp$gdp_2022)

# Use a linear regression model.
linear_model <- lm(fert_gdp$gdp_2022 ~ fert_gdp$fertility_2022, data = fert_gdp)
summary(linear_model)

# Add linear model to plot.
abline(linear_model, col = "red")

ggplot(fert_gdp, aes(x = fertility_2022, y = gdp_2022)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Fertility rate vs GDP per capita",
       x = "Fertility rate", y = "GDP per capita")


### STUDY FOR COUNTRIES WITH FERTILITY RATE < 3

fertunder3_gdp <- fert_gdp[fert_gdp$fertility_2022 < 3, ]

# Scatter plot for fertility rate versus GDP per capita.
plot(fertunder3_gdp$fertility_2022, fertunder3_gdp$gdp_2022,
     main = "Fertility rate vs GDP p.c. for countries with fertility rate < 3",
     xlab = "Fertility rate", ylab = "GDP per capita")

# Pearson correlation for fertility rate versus GDP per capita.
cor(fertunder3_gdp$fertility_2022, fertunder3_gdp$gdp_2022)

# Use a linear regression model.
linear_model <- lm(fertunder3_gdp$gdp_2022 ~ fertunder3_gdp$fertility_2022, data = fertunder3_gdp)
summary(linear_model)

# Add linear model to plot.
abline(linear_model, col = "red")

ggplot(fertunder3_gdp, aes(x = fertility_2022, y = gdp_2022)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Fertility rate vs GDP per Capita (fertility < 3)",
       x = "Fertility rate", y = "GDP per capita")

