# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load packages -----------------------------------------------------------

library(wooldridge)
library(jtools)
library(margins)
library(tidyverse)
library(lmtest)
library(pscl)
library(rstatix)
library(stargazer)

# Load data ---------------------------------------------------------------

load("../data/latinobarometro-dummies.RData")

lb_dummies <- lb_dummies %>%
  # Remove "id" variable
  select(-id)

lb_dummies <- lb_dummies %>%
  filter(year_2008 != 1, year_2009 != 1) %>%
  select(-year_2008, -year_2009)

data <- lb_dummies %>%
  filter(female == 1) %>%
  select(-female, -year_2018, -brazil)

data2 <- lb_dummies %>%
  filter(year_2015 == 1) %>%
  select(
    -year_2010,
    -year_2011,
    -year_2013,
    -year_2015,
    -year_2016,
    -year_2018,
    -year_2017,
    -brazil
  )

# Summary statistics
stargazer(
  as.data.frame(data),
  summary = TRUE,
  omit.summary.stat = "n",
  title = "Summary Statistics-Latinobarómetro Data",
  type = "html",
  out = "../figures/female-summary-statistics.html"
)

# Police
probit_police <- glm(
  trust_police ~ . - trust_judiciary - trust_government -
    satisfaction_democracy,
  family = binomial(link = "probit"),
  data = data
)

summary(probit_police)


# Democracy
probit_democracy <- glm(
  satisfaction_democracy ~ . - trust_judiciary - trust_government -
    trust_police,
  family = binomial(link = "probit"),
  data = data
)

summary(probit_democracy)


# Government
probit_government <- glm(
  trust_government ~ . - trust_judiciary - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = data
)

summary(probit_government)


# Judiciary
probit_judiciary <- glm(
  trust_judiciary ~ . - trust_government - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = data
)

summary(probit_judiciary)


# Table
stargazer(
  probit_police,
  probit_democracy,
  probit_government,
  probit_judiciary,
  dep.var.labels = c("Police", "Democracy", "Government", "Judiciary"),
  title = "The Impact of Crime Victimization on  Trust in Institutions using Female Sample",
  style = "aer",
  keep.stat = NULL,
  type = "html",
  out = "../figures/female-table.html"
)

# Robust SE ---------------------------------------------------------------
rse_police <-
  coeftest(probit_police, vcov = vcovHC(probit_police, type = "HC1"))

rse_democracy <-
  coeftest(probit_democracy, vcov = vcovHC(probit_democracy, type = "HC1"))

rse_government <-
  coeftest(probit_government, vcov = vcovHC(probit_government, type = "HC1"))

rse_judiciary <-
  coeftest(probit_judiciary, vcov = vcovHC(probit_judiciary, type = "HC1"))

stargazer(
  rse_police,
  rse_democracy,
  rse_government,
  rse_judiciary,
  dep.var.labels = c("Police", "Democracy", "Government", "Judiciary"),
  title = "The Impact of Crime Victimization on Trust in Institutions using Female Sample with Robust Standard Errors",
  style = "aer",
  keep.stat = NULL,
  type = "html",
  out = "../figures/rse-female-table.html"
)

# Marginal effects --------------------------------------------------------

# Average marginal effect Police
probit_police_margins <- margins(probit_police)
summary(probit_police_margins)

# Average marginal effect Democracy
probit_democracy_margins <- margins(probit_democracy)
summary(probit_democracy_margins)

# Average marginal effect Government
probit_government_margins <- margins(probit_government)
summary(probit_government_margins)

# Average marginal effect Judiciary
probit_judiciary_margins <- margins(probit_judiciary)
summary(probit_judiciary_margins)


# Table
export_summs(
  probit_police_margins,
  probit_democracy_margins,
  probit_government_margins,
  probit_judiciary_margins,
  model.names = c("Police", "Democracy", "Government", "Judiciary"),
  to.file = "html",
  file.name = "../figures/female-marginal-effects-table.html",
  number_format = "%.3f"
)
