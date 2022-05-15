# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load packages -----------------------------------------------------------

library(ggcorrplot)
library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/latinobarometro-dummies.RData")

countries <- c(
  "argentina",
  "bolivia",
  "chile",
  "colombia",
  "costa_rica",
  "dominican_republic",
  "ecuador",
  "el_salvador",
  "guatemala",
  "honduras",
  "mexico",
  "nicaragua",
  "panama",
  "paraguay",
  "peru",
  "uruguay",
  "venezuela"
)

years <- c(
  "year_2010",
  "year_2011",
  "year_2013",
  "year_2015",
  "year_2016",
  "year_2017",
  "year_2018"
)


# Correlation matrix ------------------------------------------------------

lb_dummies <- lb_dummies %>%
  # Remove the years 2008 and 2009
  filter(year_2008 != 1,
         year_2009 != 1) %>%
  # Remove "id", "Brazil" (reference country), "year_2008" and "year_2009" from
  # the data set
  select(-id,-brazil,-year_2008,-year_2009) %>%
  rename(
    "Age" = "age",
    "Catholic" = "catholic",
    "White" = "white",
    "Married" = "married",
    "Female" = "female",
    "Insufficient Income" = "insufficient_income",
    "Unemployed" = "unemployed",
    "University" = "university",
    "High School" = "high_school",
    "Employed" = "employed",
    "Relative Victim of Crime" = "relative_victim",
    "Victim of Crime" = "victim",
    "Judiciary Trust" = "trust_judiciary",
    "Government Trust" = "trust_government",
    "Police Trust" = "trust_police",
    "Democracy Satisfaction" = "satisfaction_democracy",
    "General Trust" = "trust",
    "Life Satisfaction" = "satisfaction_life",
    "Sufficient Income" = "sufficient_income"
  )

correlations <- round(cor(lb_dummies),
                      digits = 3)

# Remove country and year variables
lb_dummies <- lb_dummies %>%
  select(-all_of(countries),-all_of(years))

correlations <- round(cor(lb_dummies),
                      digits = 3)


# Visualize correlation matrix --------------------------------------------

correlation_plot <- ggcorrplot(
  correlations,
  hc.order = TRUE,
  type = "lower",
  colors = c("#1D87C4", "#FFFFFF", "#E0773A"),
  lab = TRUE,
  insig = "blank",
  legend.title = "Correlation\nCoefficient",
  title = "Variable Correlations"
) +
  labs(caption = "Source: Latinobarómetro Survey")

ggsave(
  "correlation-plot.png",
  path = "../figures",
  plot = correlation_plot,
  dpi = 300,
  width = 12,
  height = 12
)
