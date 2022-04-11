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
  select(-id)

lb_dummies <- as.data.frame(lb_dummies)
attach(lb_dummies)

# OLS model ---------------------------------------------------------------

lpm <- lm(trust_police ~ ., data = lb_dummies)

summary(lpm)

# Probit model ------------------------------------------------------------

probit <-
  glm(trust_police ~ .,
      family = binomial(link = "probit"),
      data = lb_dummies)

summary(probit)

# Pseudo R-squared --------------------------------------------------------
pR2(probit)

# Marginal effects --------------------------------------------------------

# Average marginal effect
probit_margins <- margins(probit)
summary(probit_margins)

# Tables ------------------------------------------------------------------

stargazer(lb_dummies, type = "html", out = "..figures/table1.html")

stargazer(lpm,
          probit,
          keep.stat = NULL,
          type = "html",
          out = "..figures/table2.html")

stargazer(
  lpm,
  probit,
  keep.stat = NULL,
  ci = TRUE,
  ci.level = 0.95,
  type = "html",
  out = "..figures/table3.html"
)

export_summs(lpm,
             probit,
             probit_margins,
             to.file = "html",
             file.name = "..figures/table4.html")


# Predicted probabilities victim ------------------------------------------

# convert victim to factor

lb_dummies2 <- lb_dummies %>%
  mutate(victim = factor(victim))
class(new_data$victim)

probit2 <- glm(trust_police ~ . - trust_police,
               family = binomial(link = "probit"),
               data = lb_dummies2)

lb_dummies2 <- lb_dummies2 %>%
  group_by(victim) %>%
  summarize_all(mean)

lb_dummies2$predict_trust_police <- 
  predict(probit2, lb_dummies2, type = "response")

# Predicted probabilities for victim & trust ------------------------------

lb_dummies3 <- lb_dummies %>%
  mutate(victim = factor(victim), trust = factor(trust))

probit3 <- glm(trust_police ~ . - trust_police,
               family = binomial(link = "probit"),
               data = lb_dummies3)

lb_dummies3 <- lb_dummies3 %>%
  group_by(victim, trust) %>%
  summarize_all(mean)

lb_dummies3$predict_trust_police <- predict(probit3,
                                            lb_dummies3,
                                            type = "response")

# Predicted probabilities for victim & relative victim --------------------

lb_dummies4 <- lb_dummies %>%
  mutate(victim = factor(victim), relative_victim = factor(relative_victim))

probit4 <- glm(trust_police ~ . - trust_police,
               family = binomial(link = "probit"),
               data = lb_dummies4)

lb_dummies4 <- lb_dummies4 %>%
  group_by(victim, relative_victim) %>%
  summarize_all(mean)

lb_dummies4$predict_trust_police <- predict(probit4,
                                            lb_dummies4,
                                            type = "response")