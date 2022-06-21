# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

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


# Graph: Crime indexes ----------------------------------------------------

graph1 <-
  ggplot(
    data = data2,
    aes(
      x = reorder(country, -crime_rate),
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
    # title = "Figure 1. Crime Index Rates 2017",
    # subtitle = "Measured on a scale from 0 to 30",
    caption = "Source: World Bank"
  ) +
  geom_col() +
  scale_fill_manual(values=c("#343779", "#cdcdcd"))+
  # coord_flip()  +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=9, color="dimgray")
  )

graph1

graph2 <-
  ggplot(
    data = data2,
    aes(
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
    # title = "Figure 1. Crime Index Rates 2017",
    # subtitle = "Measured on a scale from 0 to 30"
    caption = "Source: World Bank"
  ) +
  geom_col() +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=9, color="dimgray")
  )

graph2

# Save graphs

ggsave(
  "crime-rates.png",
  plot = graph1,
  path = "../figures",
  width = 16,
  height = 3 / 4 * 16,
  units = "cm",
  dpi = 300
)

ggsave(
  "crime-rates-2.png",
  plot = graph2,
  path = "../figures",
  width = 16,
  height = 4 / 4 * 16,
  units = "cm",
  dpi = 300
)

