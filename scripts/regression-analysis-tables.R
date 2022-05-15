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
library(sandwich)
library(lmtest)

# Load data ---------------------------------------------------------------

load("../data/latinobarometro-dummies.RData")

lb_dummies <- lb_dummies %>%
  # Remove "id" variable
  select(-id)

data <- lb_dummies %>%
  filter(year_2008 != 1, year_2009 != 1) %>%
  select(-year_2008,-year_2009,-year_2018,-brazil)

data.frame(data)
attach(data)


# Linear probability model ------------------------------------------------

# Police
lpm_police <- lm(trust_police ~ . - trust_judiciary - trust_government -
                   satisfaction_democracy,
                 data = data)

summary(lpm_police)


# Democracy
lpm_democracy <- lm(satisfaction_democracy ~ . - trust_judiciary - trust_government -
                      trust_police,
                    data = data)

summary(lpm_democracy)


# Government
lpm_government <- lm(trust_government ~ . - trust_judiciary - trust_police -
                       satisfaction_democracy,
                     data = data)

summary(lpm_government)


# Judiciary
lpm_judiciary <- lm(trust_judiciary ~ . - trust_police - trust_government -
                      satisfaction_democracy,
                    data = data)

summary(lpm_judiciary)


# Probit model ------------------------------------------------------------

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

coeftest(probit_police)
coeftest(probit_democracy)
coeftest(probit_government)
coeftest(probit_judiciary)
# Table -------------------------------------------------------------------

# Summary statistics
stargazer(
  as.data.frame(data),
  covariate.labels = c(
    "Age",
    "Trust Police",
    "Victim",
    "Relative Victim",
    "Female",
    "White",
    "Married",
    "Catholic",
    "Employed",
    "Unemployed",
    "Trust",
    "Sufficient Income",
    "Insufficient Income",
    "Life Satisfaction",
    "Democracy Satisfaction",
    "Government Trust",
    "Judiciary Trust",
    "High School",
    "University",
    "Year 2010",
    "Year 2011",
    "Year 2013",
    "Year 2015",
    "Year 2016",
    "Year 2017",
    "Argentina",
    "Bolivia",
    "Chile",
    "Colombia",
    "Costa rica",
    "Dominican Republic",
    "Ecuador",
    "El salvador",
    "Guatemala",
    "Honduras",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Uruguay",
    "Venezuela"
  ),
  summary = TRUE,
  omit.summary.stat = "n",
  title = "Summary Statistics-Latinobarómetro Data",
  type = "html",
  out = "../figures/summary-statistics.html"
)

# Probit Model
stargazer(
  probit_police,
  probit_democracy,
  probit_government,
  probit_judiciary,
  covariate.labels = c(
    "Age",
    "Victim",
    "Relative Victim",
    "Female",
    "White",
    "Married",
    "Catholic",
    "Employed",
    "Unemployed",
    "Trust",
    "Sufficient Income",
    "Insufficient Income",
    "Life Satisfaction",
    "High School",
    "University",
    "Year 2010",
    "Year 2011",
    "Year 2013",
    "Year 2015",
    "Year 2016",
    "Year 2017",
    "Argentina",
    "Bolivia",
    "Chile",
    "Colombia",
    "Costa rica",
    "Dominican Republic",
    "Ecuador",
    "El salvador",
    "Guatemala",
    "Honduras",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Uruguay",
    "Venezuela"
  ),
  dep.var.labels = c("Police", "Democracy", "Government", "Judiciary"),
  title = "The Impact of Crime Victimization on Trust in Institutions-Probit Model",
  style = "aer",
  keep.stat = NULL,
  type = "html",
  out = "../figures/probit-table.html"
)

# Linear Probability Model
stargazer(
  lpm_police,
  lpm_democracy,
  lpm_government,
  lpm_judiciary,
  dep.var.labels = c("Police", "Democracy", "Government", "Judiciary"),
  covariate.labels = c(
    "Age",
    "Victim",
    "Relative Victim",
    "Female",
    "White",
    "Married",
    "Catholic",
    "Employed",
    "Unemployed",
    "Trust",
    "Sufficient Income",
    "Insufficient Income",
    "Life Satisfaction",
    "High School",
    "University",
    "Year 2010",
    "Year 2011",
    "Year 2013",
    "Year 2015",
    "Year 2016",
    "Year 2017",
    "Argentina",
    "Bolivia",
    "Chile",
    "Colombia",
    "Costa rica",
    "Dominican Republic",
    "Ecuador",
    "El salvador",
    "Guatemala",
    "Honduras",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Uruguay",
    "Venezuela"
  ),
  title = "The Impact of Victimization on Institutions-Linear Probability Model",
  style = "aer",
  keep.stat = NULL,
  type = "html",
  out = "../figures/linear-probability-table.html"
)

# Robust Standard Errors --------------------------------------------------

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
  covariate.labels = c(
    "Age",
    "Victim",
    "Relative Victim",
    "Female",
    "White",
    "Married",
    "Catholic",
    "Employed",
    "Unemployed",
    "Trust",
    "Sufficient Income",
    "Insufficient Income",
    "Life Satisfaction",
    "High School",
    "University",
    "Year 2010",
    "Year 2011",
    "Year 2013",
    "Year 2015",
    "Year 2016",
    "Year 2017",
    "Argentina",
    "Bolivia",
    "Chile",
    "Colombia",
    "Costa rica",
    "Dominican Republic",
    "Ecuador",
    "El salvador",
    "Guatemala",
    "Honduras",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Uruguay",
    "Venezuela"
  ),
  dep.var.labels = c("Police", "Democracy", "Government", "Judiciary"),
  title = "The Impact of Crime Victimization on Trust in Institutions-Probit Model With Robust Standard Errors",
  style = "aer",
  keep.stat = NULL,
  type = "html",
  out = "../figures/rse_probit-table.html"
)

# Pseudo R-squared --------------------------------------------------------
library(DescTools)

pR2(probit_police)
pR2(probit_democracy)
pR2(probit_government)
pR2(probit_judiciary)

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
  file.name = "../figures/marginal-effects-table.html",
  number_format = "%.3f"
)

# Predicted probabilities victim ------------------------------------------

# convert victim to factor
lb_dummies2 <- data %>%
  mutate(victim = factor(victim))

class(lb_dummies2$victim)

lb_dummies3 <- lb_dummies2 %>%
  group_by(victim) %>%
  summarize_all(mean)


police <-
  glm(
    trust_police ~ . - trust_judiciary - trust_government - satisfaction_democracy,
    family = binomial(link = "probit"),
    data = lb_dummies2
  )

lb_dummies3$predict_police <-
  predict(police, lb_dummies3, type = "response")


# Democracy
democracy <- glm(
  satisfaction_democracy ~ . - trust_judiciary - trust_government -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies2
)

lb_dummies3$predict_democracy <-
  predict(democracy, lb_dummies3, type = "response")


# Government
government <- glm(
  trust_government ~ . - trust_judiciary - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies2
)

lb_dummies3$predict_government <-
  predict(government, lb_dummies3, type = "response")

# Judiciary
judiciary <- glm(
  trust_judiciary ~ . - trust_government - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies2
)

lb_dummies3$predict_judiciary <-
  predict(judiciary, lb_dummies3, type = "response")


pp_victim <- lb_dummies3 %>%
  select(victim,
         predict_police,
         predict_democracy,
         predict_government,
         predict_judiciary)


# Predicted probabilities for victim & trust ------------------------------
lb_dummies4 <- data %>%
  mutate(victim = factor(victim), trust = factor(trust))

lb_dummies5 <- lb_dummies4 %>%
  group_by(victim, trust) %>%
  summarize_all(mean)


# Police
police2 <- glm(
  trust_police ~ . - trust_judiciary - trust_government - trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies4
)

lb_dummies5$predict_police <-
  predict(police2, lb_dummies5, type = "response")


# Democracy
democracy2 <- glm(
  satisfaction_democracy ~ . - trust_judiciary - trust_government -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies4
)

lb_dummies5$predict_democracy <-
  predict(democracy2, lb_dummies5, type = "response")

# Government
government2 <- glm(
  trust_government ~ . - trust_judiciary - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies4
)

lb_dummies5$predict_government <-
  predict(government2, lb_dummies5, type = "response")

# Government
judiciary2 <- glm(
  trust_judiciary ~ . - trust_judiciary - satisfaction_democracy - trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies4
)

lb_dummies5$predict_judiciary <-
  predict(judiciary2, lb_dummies5, type = "response")


pp_victim_trust <- lb_dummies5 %>%
  select(
    victim,
    trust,
    predict_police,
    predict_democracy,
    predict_government,
    predict_judiciary
  )


# Predicted probabilities for victim & relative victim --------------------
lb_dummies6 <- data %>%
  mutate(victim = factor(victim),
         relative_victim = factor(relative_victim))


lb_dummies7 <- lb_dummies6 %>%
  group_by(victim, relative_victim) %>%
  summarize_all(mean)


# Police
police3 <- glm(
  trust_police ~ . - trust_judiciary - satisfaction_democracy -
    trust_government,
  family = binomial(link = "probit"),
  data = lb_dummies6
)

lb_dummies7$predict_police <- predict(police3,
                                      lb_dummies7,
                                      type = "response")


# Democracy
democracy3 <- glm(
  satisfaction_democracy ~ . - trust_judiciary - trust_police -
    trust_government,
  family = binomial(link = "probit"),
  data = lb_dummies6
)

lb_dummies7$predict_democracy <- predict(democracy3,
                                         lb_dummies7,
                                         type = "response")


# Government
government3 <- glm(
  trust_government ~ . - trust_judiciary - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies6
)

lb_dummies7$predict_government <- predict(government3,
                                          lb_dummies7,
                                          type = "response")

# Judiciary
judiciary3 <- glm(
  trust_judiciary ~ . - trust_government - satisfaction_democracy -
    trust_police,
  family = binomial(link = "probit"),
  data = lb_dummies6
)

lb_dummies7$predict_judiciary <- predict(judiciary3,
                                         lb_dummies7,
                                         type = "response")


pp_victim_relative <- lb_dummies7 %>%
  select(
    victim,
    relative_victim,
    predict_police,
    predict_democracy,
    predict_government,
    predict_judiciary
  )
