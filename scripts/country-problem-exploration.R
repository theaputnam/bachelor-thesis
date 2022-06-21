# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/latinobarometro.RData")

problems_pre_2018 <- c(
  "15" = "Crime / Public Security",
  "-1" = NA,
  "-2" = NA,
  "-4" = NA
)

problems_2018 <- c(
  "24" = "Crime / Public Security",
  "-1" = NA,
  "-2" = NA,
  "-4" = NA
)


# Select year and problem_country variables -------------------------------

problem_country_year_pre_2018 <- lb %>%
  select(year, problem_country) %>%
  filter(year < 2018, year >= 2010) %>%
  # Re-code values according to "problems"
  mutate(problem_country = recode(problem_country,!!!problems_pre_2018,
                                  .default = "Other"))

problem_country_year_2018 <- lb %>%
  select(year, problem_country) %>%
  filter(year == 2018) %>%
  # Re-code values according to "problems"
  mutate(problem_country = recode(problem_country,!!!problems_2018,
                                  .default = "Other"))

# Combine two data frames
problem_country_year <- bind_rows(problem_country_year_2018,
                                  problem_country_year_pre_2018)

# Remove temporary data frames from the environment
rm(problem_country_year_2018)
rm(problem_country_year_pre_2018)

head(problem_country_year)

problem_country_year <- problem_country_year %>%
  drop_na() %>%
  group_by(year, problem_country) %>%
  summarise(count = n()) %>%
  # Reshape data frame from long to wide to sum the `Crime / Public Security`
  # and the `Other` columns and then divide the `Crime / Public Security` by the
  # new `total` column to get the fraction/percentage share
  pivot_wider(names_from = problem_country, values_from = count) %>%
  mutate(total = `Crime / Public Security` + Other) %>%
  mutate(`Crime / Public Security Fraction` = `Crime / Public Security` / total)

head(problem_country_year)


# Plot 1: Crime as the Most Important National Problem over Time ----------

plot1 <- ggplot(data = problem_country_year,
                mapping = aes(x = year,
                              y = `Crime / Public Security Fraction`)) +
  geom_line(size = 1.1, color="#343779") +
  geom_point(size = 3.1, color="#343779") +
  labs(
    x = "",
    y = "",
    #title = "Crime as the Most Important National Problem",
    caption = "Source: Latinobarómetro Survey"
  ) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_bw()+ 
  theme(
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=10, color="dimgray"))

plot1

ggsave(
  "national-problem-year.png",
  path = "../figures",
  plot = plot1,
  width = 16,
  height = 2.5 / 4 * 16,
  units = "cm",
  dpi = 300
)
