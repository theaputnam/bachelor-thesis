# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load packages -----------------------------------------------------------

library(ggcorrplot)


# Load data ---------------------------------------------------------------

countries <- c("argentina", "bolivia", "chile", "colombia", "costa_rica", 
               "dominican_republic", "ecuador", "el_salvador", 
               "guatemala", "honduras", "mexico", "nicaragua", "panama", 
               "paraguay", "peru", "uruguay", "venezuela")

years <- c("year_2008", "year_2009", "year_2010", "year_2011", "year_2013", 
           "year_2015", "year_2016", "year_2017")


# Load packages -----------------------------------------------------------

load("../data/latinobarometro-dummies.RData")


# Deselect id and change varibale names -----------------------------------

lb_dummies <- lb_dummies %>%
  select(-id) %>%
  rename("Age" = "age",
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
         "Political Party Trust" = "trust_parties",
         "Judiciary Trust" = "trust_judiciary",
         "Government Trust" = "trust_government",
         "Police Trust" = "trust_police",
         "Democracy Satisfaction" = "satisfaction_democracy",
         "General Trust" = "trust",
         "Life Satisfaction" = "satisfaction_life",
         "Sufficient Income" = "sufficient_income")

lb_dummies <- as.data.frame(lb_dummies)

# Calculate all correlations ----------------------------------------------

correlations <- cor(lb_dummies)
round(correlations,3)


# Deselect year and country variables -------------------------------------

lb_dummies <- lb_dummies %>%
  select(-all_of(countries), -all_of(years))

lb_dummies <- as.data.frame(lb_dummies)


# Attach data -------------------------------------------------------------

attach(lb_dummies)

# Calculate correlations --------------------------------------------------

correlations <- cor(lb_dummies)

round(correlations,3)

#  Create correlation matrix ----------------------------------------------

corr_table <- ggcorrplot(
  correlations,
  hc.order = TRUE,
  type = "lower",
  colors = c("#1D87C4", "#FFFFFF", "#E0773A"),
  lab = TRUE,
  insig = "blank",
  legend.title = "Correlation\nCoefficient",
  title = "Variable Correlations"
)

corr_table


# Save correlation matrix to figures folder -------------------------------

ggsave("corr_table.png", path = "../figures", plot = corr_table, dpi = 300, width = 12, height = 12)
