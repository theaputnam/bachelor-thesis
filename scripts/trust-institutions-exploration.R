# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/latinobarometro-dummies.RData")


# Deselect id and change variable names -----------------------------------

trust_year <- lb_dummies %>%
  select(id,
         trust,
         trust_government,
         trust_judiciary,
         trust_police,
         satisfaction_democracy) %>%
  # Derive year column from id variable
  mutate(year = as.numeric(substr(id, start = 1, stop = 4)),
         .before = id) %>%
  select(-id) %>%
  group_by(year) %>%
  summarise(
    trust_count = sum(trust),
    trust_government_count = sum(trust_government),
    trust_judiciary_count = sum(trust_judiciary),
    trust_police_count = sum(trust_police),
    satisfaction_democracy_count = sum(satisfaction_democracy),
    count = length(trust)
  ) %>%
  ungroup() %>%
  mutate(
    trust_proportion = trust_count / count,
    trust_government_proportion = trust_government_count / count,
    trust_judiciary_proportion = trust_judiciary_count / count,
    trust_police_proportion = trust_police_count / count,
    satisfaction_democracy_proportion = satisfaction_democracy_count / count
  ) %>%
  filter(year >= 2010, year <= 2018)

head(trust_year)
glimpse(trust_year)


# Graph 1: Trust in Institutions Over Time --------------------------------

trust_year_plot <- trust_year %>%
  select(
    year,
    Police = trust_police_proportion,
    Judiciary = trust_judiciary_proportion,
    Government = trust_government_proportion,
    Democracy = satisfaction_democracy_proportion
  ) %>%
  pivot_longer(!year, names_to = "variable", values_to = "proportion") %>%
  ggplot(mapping = aes(x = year, y = proportion)) +
  geom_line(size = 0.9, color = "#343779") +
  geom_point(size = 2.7, color = "#343779") +
  coord_cartesian(ylim=c(0.2, 0.6))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2018, by = 1)) +
  labs(
    x = " ",
    y = " ",
    # title = "Trust in Institutions in Latin America",
    caption = "Source: Latinobarómetro Survey"
  ) +
  facet_wrap(# Reorder variables
    ~ factor(
      variable,
      levels = c("Police",
                 "Judiciary",
                 "Government",
                 "Democracy")
    ),
    ncol = 1) +
  theme_bw() +
  theme(
    # Remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(colour="black", fill="white"),
    plot.caption = element_text(size=10, color="dimgray")
  )

trust_year_plot

ggsave(
  "trust-institutions.png",
  path = "../figures",
  plot = trust_year_plot,
  width = 16,
  height = 4 / 4 * 16,
  units = "cm",
  dpi = 300
)
