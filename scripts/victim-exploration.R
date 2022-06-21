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
  ggplot(
    mapping = aes(
      y = country,
      x = victim_proportion,
      fill = victim
    )
  ) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "#343779",
      "#982062",
      "#F86041",
      "#cbcbcb",
      "white",
      "white",
      "white"
    ),
    labels = c(
      "Respondent",
      "Relative",
      "Both",
      "Neither",
      "Don't Know",
      "No Answer",
      "Not Asked"
    )
  ) +
  facet_wrap(~ year) +
  labs(
    x = "",
    y = "",
    # title = "Type of Victimization by Country Over Time",
    fill = "",
    caption = "Source: Latinobarómetro Survey"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    legend.position = "bottom",
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=10, color="dimgray")
  )

victim_count_plot

ggsave(
  "victim-count.png",
  path = "../figures",
  plot = victim_count_plot,
  width = 16,
  height = 24,
  units = "cm",
  dpi = 300
)


# Graph 2: Number of Victims by Type Over Time by country ----------------------

victim_country_count_year <- victim_count %>%
  select(year, country, victim, victim_count) %>%
  filter(!(year %in% c(2008, 2009))) %>%
  filter(!(victim %in% c("Don't know", "No answer", "Not asked"))) %>%
  group_by(year, country, victim) %>%
  summarise(victim_count = sum(victim_count))%>%
  drop_na()

victim_count_year_plot <-
  ggplot(victim_country_count_year,
         mapping = aes(x = year, y = victim_count, color = victim)) +
  geom_line(mapping = aes(linetype = victim), size = 0.8) +
  geom_point(mapping = aes(shape = victim, fill = victim), size = 1.5) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  scale_y_continuous(
    labels = scales::comma) +
  scale_color_manual(
    values = c(
      "#343779",
      "#982062",
      "#F86041",
      "#cbcbcb"
    ),
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_fill_manual(
    values = c(
      "#343779",
      "#982062",
      "#F86041",
      "#cbcbcb"
    ),
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_linetype_discrete(
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_shape_manual(
    values = c(21, 22, 23, 24),
    labels = c("Respondent", "Relative", "Both", "Neither")) +
  labs(
    x = "",
    y = "",
    # title = "Crime Victimization In Latin America",
    caption = "Source: Latinobarómetro Survey",
    color = "",
    linetype = "",
    shape = "",
    fill = ""
  ) +
  facet_wrap( ~ country, ncol = 3) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour="black", fill="white"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=10, color="dimgray")
   )

victim_count_year_plot

ggsave(
  "victimization.png",
  path = "../figures",
  plot = victim_count_year_plot,
  width = 16,
  height = 24,
  units = "cm",
  dpi = 300
)

# Victims of Crime over Time ----------------------------------------------

victim_count_year <- victim_count %>%
  select(year, victim, victim_count) %>%
  filter(!(year %in% c(2008, 2009))) %>%
  filter(!(victim %in% c("Don't know", "No answer", "Not asked"))) %>%
  group_by(year, victim) %>%
  summarise(victim_count = sum(victim_count)) %>%
  ungroup() %>%
  drop_na()


total_victim_count <-
  ggplot(
    victim_count_year,
    mapping = aes(
      x = year,
      y = victim_count,
      color = victim
    )
  ) +
  geom_line(mapping = aes(linetype = victim), size = 1.2) +
  geom_point(mapping = aes(shape = victim, fill = victim), size = 2.7) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 15000, by = 5000),
    labels = scales::comma
  ) +
  # scale_fill_grey(labels = c("Respondent", "Relative", "Both", "Neither")) +
  # scale_color_grey(labels = c("Respondent", "Relative", "Both", "Neither")) +
  scale_color_manual(
    values = c(
      "#343779",
      "#982062",
      "#F86041",
      "#cbcbcb"
    ),
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_fill_manual(
    values = c(
      "#343779",
      "#982062",
      "#F86041",
      "#cbcbcb"
    ),
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_linetype_discrete(
    labels = c("Respondent", "Relative", "Both", "Neither")
  ) +
  scale_shape_manual(
    labels = c("Respondent", "Relative", "Both", "Neither"),
    values = c(21, 22, 23, 24)
  ) +
  labs(
    x = "",
    y = "",
    # title = "Crime Victimization In Latin America"
    caption = "Source: Latinobarómetro Survey",
    color = "",
    linetype = "",
    shape = "",
    fill = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size=10, color="dimgray")
  )

total_victim_count

ggsave(
  "total-victimization.png",
  path = "../figures",
  plot = total_victim_count,
  width = 16,
  height = 2.5 / 4 * 16,
  units = "cm",
  dpi = 300
)


