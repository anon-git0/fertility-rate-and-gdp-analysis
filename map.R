library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# maps library contains coordinate data for the world.
coordinates <- map_data("world")
colnames(coordinates)[5] <- "country"

# These countries don't have the same names in our World Bank data as they
# do in the maps world data, so we change them to match.
country_replacements <- c(
  "United States" = "USA",
  "United Kingdom" = "UK",
  "Venezuela, RB" = "Venezuela",
  "Cote d'Ivoire" = "Ivory Coast",
  "Egypt, Arab Rep." = "Egypt",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Republic of Congo",
  "Yemen, Rep." = "Yemen",
  "Russian Federation" = "Russia",
  "Turkiye" = "Turkey",
  "Syrian Arab Republic" = "Syria",
  "Iran, Islamic Rep." = "Iran",
  "Viet Nam" = "Vietnam",
  "Lao PDR" = "Laos",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Korea, Rep." = "South Korea",
  "Czechia" = "Czech Republic",
  "Slovak Republic" = "Slovakia",
  "Gambia, The" = "Gambia",
  "Eswatini" = "Swaziland",
  "West Bank and Gaza" = "Palestine",
  "Micronesia, Fed. Sts." = "Micronesia")

# Function to apply the country name changes to the column named 'country'.
replace_countries <- function(data) {
  data %>%
    mutate(country = recode(country, !!!country_replacements))}
# !!! splices the vector.

# Apply these changes to fertility and gdp per capita data.
fertility_data <- replace_countries(fertility_data)
gdp_data <- replace_countries(gdp_data)

# We use left_join because we want to join fertility data whenever the country matches.
# But if there is no match, we don't want to delete the coordinate data (so that countries
# without available data still show up on the map.)
coordinates <- left_join(coordinates, fertility_data, by = "country")
coordinates <- left_join(coordinates, gdp_data, by = "country")

# For a visualisation with GDP per capita, log GDP is a lot nicer.
# GDPs per capita appear on very different scales worldwide. e.g.
# most countries in Africa have values in low-mid thousands, while
# the most developed countries in the world have around 100,000.
# So showing the raw value would result in very little colour
# variation within Africa, for example.
coordinates <- coordinates %>%
  mutate(log_gdp_2022 = log(gdp_2022))

# Build the map.
# geom_polygon takes long and lat as its first arguments because longitude is the x axis
# and latitude the y axis for the map.
# group=group: regions with the same colour/coordinate data are grouped together.
# fill = fertility_2022 colours the countries by their fertility rate.
# Then we define a tooltip showing the country's name, fertility rate and GDP p.c.
map_fert <- ggplot(coordinates) + 
  geom_polygon(aes(long,lat, group=group, fill=fertility_2022,text = paste("Country: ", country, "\nFertility rate: ", fertility_2022, "\nGDP per capita: ", gdp_2022)),
               color = "black",
               linewidth = 0.1)+
  scale_fill_gradient(low = "blue", high = "red", name = "Fertility rate") +
  labs(title = "Fertility rate by country", x = "Longitude", y = "Latitude")

fert_out <- ggplotly(map_fert, tooltip = "text")
fert_out

map_gdp <- ggplot(coordinates) + 
  geom_polygon(aes(long,lat, group=group, fill=log_gdp_2022,text = paste("Country: ", country, "\nFertility rate: ", fertility_2022, "\nGDP per capita: ", gdp_2022)),
               color = "black",
               linewidth = 0.1)+
  scale_fill_gradient(low = "red", high = "blue", name = "Log GDP\nper capita") +
  labs(title = "Log of GDP per capita by country", x = "Longitude", y = "Latitude")

gdp_out <- ggplotly(map_gdp, tooltip = "text")
gdp_out