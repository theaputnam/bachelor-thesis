# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(directlabels)


# Read data ---------------------------------------------------------------
data <- read_excel("../data/crime-rates.xlsx")

data2 <- data %>%
  filter(Indicator == "Crime and Theft") %>%
  select(`Country Name`, `2017`) %>%
  rename(country = 'Country Name') %>%
  filter(
    country %in% c(
      "Mexico",
      "Argentina",
      "Brazil",
      "Chile",
      "Colombia",
      "Costa Rica",
      "Cuba",
      "Dominican Republic",
      "Ecuador",
      "El Salvador",
      "Guatemala",
      "Honduras",
      "Nicaragua",
      "Paraguay",
      "Puerto Rico",
      "Panama",
      "Peru",
      "Venezuela",
      "Uruguay",
      "United States",
      "China",
      "Russia",
      "Australia",
      "Germany",
      "Italy",
      "Turkey",
      "Netherlands",
      "India",
      "Sweden",
      "South Korea",
      "Nigeria",
      "Poland",
      "Albania",
      "Zambia",
      "Greece",
      "Ireland",
      "Saudi Arabia",
      "Vietnam",
      "Romania",
      "Ukraine"
    )
  ) %>%
  rename("crime_rate" = `2017`)


# Graph: crime indexes ----------------------------------------------------
graph1 <-
  ggplot(data = data2, aes(
    x = reorder(country, crime_rate),
    y = crime_rate,
    fill = factor(ifelse(
      country %in% c(
        "Mexico",
        "Argentina",
        "Brazil",
        "Bolivia",
        "Chile",
        "Colombia",
        "Costa Rica",
        "Cuba",
        "Dominican Republic",
        "Ecuador",
        "El Salvador",
        "Guatemala",
        "Honduras",
        "Nicaragua",
        "Paraguay",
        "Panama",
        "Puerto Rico",
        "Peru",
        "Venezuela",
        "Uruguay"
      ),
      "Latin America",
      "Other"
    ))
  )) +
  labs(
    x = " ",
    y = " ",
    title = "Figure 1. Crime Index Rates 2017",
    subtitle = "Measured on a scale from 0 to 30",
    caption = "Source: World Bank"
  ) +
  geom_col() + coord_flip() + scale_fill_grey() + theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "botom")

graph1

# Save graph
ggsave(
  "crime-rates.png",
  plot = graph1,
  path = "../figures",
  width = 8,
  height = 10,
  dpi = 300
)
