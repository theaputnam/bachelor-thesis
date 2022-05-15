# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/latinobarometro.RData")

countries <- c(
  "32" = "Argentina",
  "68" = "Bolivia",
  "76" = "Brasil",
  "152" = "Chile",
  "170" = "Colombia",
  "188" = "Costa Rica",
  "214" = "Dominican Republic",
  "218" = "Ecuador",
  "222" = "El Salvador",
  "320" = "Guatemala",
  "340" = "Honduras",
  "484" = "Mexico",
  "558" = "Nicaragua",
  "591" = "Panama",
  "600" = "Paraguay",
  "604" = "Peru",
  "858" = "Uruguay",
  "862" = "Venezuela"
)

victimizations <- c(
  "-4" = "Not asked",
  "-2" = "No answer",
  "-1" = "Don't know",
  "1" = "You",
  "2" = "Some relative",
  "3" = "Both of them",
  "4" = "No"
)

# Data exploration --------------------------------------------------------

victim_year_country <- lb %>%
  select(year, country, victim) %>%
  mutate(country = as.factor(recode(country,!!!countries)),
         victim = as.factor(recode(victim,!!!victimizations)))

# Count missing values by variable
sapply(victim_year_country, function(x)
  sum(is.na(x)))

victim_count <- victim_year_country %>%
  group_by(year, country, victim) %>%
  summarise(victim_count = length(victim)) %>%
  ungroup() %>%
  group_by(year, country) %>%
  mutate(count = sum(victim_count)) %>%
  ungroup() %>%
  mutate(victim_proportion = victim_count / count) %>%
  mutate(victim = factor(
    victim,
    level = c(
      "You",
      "Some relative",
      "Both of them",
      "No",
      "Don't know",
      "No answer",
      "Not asked"
    )
  ))


# Graph 1: Type of Victimization by Country Over Time ---------------------

victim_count_plot <- victim_count %>%
  ggplot(mapping = aes(y = country,
                       x = victim_proportion,
                       fill = victim)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "You" = "black",
      "Some relative" = "#1D87C4",
      "Both of them" = "#E0773A",
      "No" = "gray",
      "Don't know" = "#d1d1d1",
      "No answer" = "#f3f3f3",
      "Not asked" = "red"
    ),
    labels = c(
      "Respondent",
      "Relative",
      "Both",
      "neither",
      "Don't Know",
      "No Answer",
      "Not Asked"
    )
  ) +
  facet_wrap( ~ year) +
  labs(
    x = "",
    y = "",
    title = "Type of Victimization by Country Over Time",
    fill = "",
    caption = "Source: Latinobarómetro Survey"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

victim_count_plot

ggsave(
  "victim-count.png",
  path = "../figures",
  plot = victim_count_plot,
  dpi = 300,
  width = 8,
  height = 10
)


# Graph 2: Number of Victims by Type Over Time by country ----------------------

victim_country_count_year <- victim_count %>%
  select(year, country, victim, victim_count) %>%
  filter(!(year %in% c(2008, 2009))) %>%
  filter(!(victim %in% c("Don't know", "No answer", "Not asked"))) %>%
  group_by(year, country, victim) %>%
  summarise(victim_count = sum(victim_count))

victim_count_year_plot <-
  ggplot(victim_country_count_year,
         mapping = aes(x = year, y = victim_count, color = victim)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  scale_color_manual(
    values = c(
      "You" = "black",
      "Some relative" = "#1D87C4",
      "Both of them" = "#E0773A",
      "No" = "gray"
    ),
    labels = c("Respondent", "Relative", "Both", "neither")
  ) +
  labs(
    x = "",
    y = "",
    title = "Crime Victimization by Country",
    color = "",
    caption = "Source: Latinobarómetro Survey"
  ) +
  facet_wrap( ~ country, ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

victim_count_year_plot

ggsave(
  "victimization.png",
  path = "../figures",
  plot = victim_count_year_plot,
  dpi = 300,
  width = 8,
  height = 10
)


# Victims of Crime over Time ----------------------------------------------

victim_count_year <- victim_count %>%
  select(year, victim, victim_count) %>%
  filter(!(year %in% c(2008, 2009))) %>%
  filter(!(victim %in% c("Don't know", "No answer", "Not asked"))) %>%
  group_by(year, victim) %>%
  summarise(victim_count = sum(victim_count))


total_victim_count <-
  ggplot(victim_count_year,
         mapping = aes(x = year, y = victim_count, color = victim)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  scale_color_manual(
    values = c(
      "You" = "black",
      "Some relative" = "#1D87C4",
      "Both of them" = "#E0773A",
      "No" = "gray"
    ),
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  labs(
    x = "",
    y = "",
    title = "Crime Victimization In Latin America",
    color = "",
    caption = "Source: Latinobarómetro Survey"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

total_victim_count

ggsave(
  "total-victimization.png",
  path = "../figures",
  plot = total_victim_count,
  dpi = 300,
  width = 8,
  height = 6
)
