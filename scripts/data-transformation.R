# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load packages -----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/latinobarometro.RData")

unique(lb$country)

attach(lb)


# Create dummy variables --------------------------------------------------

# Trust in the police dummy
unique(trust_police)

# 1 = a lot of confidence
# 2 = some confidence
# 3 = little confidence
# 4 = no confidence
# -1 = DK
# -2 = no answers

lb <- lb %>%
  mutate(
    trust_police_dummy = case_when(
      trust_police == 1 ~ 1,
      trust_police == 2 ~ 1,
      trust_police == 3 ~ 0,
      trust_police == 4 ~ 0
    )
  )

unique(lb$trust_police_dummy)
summary(lb$trust_police_dummy)
table(lb$trust_police_dummy, useNA = "always")

# Crime victim dummy
unique(victim)

# 1 = you
# 2 = relative
# 3 = both
# 4 = no
# -1 = DK
# -2 = no answers
# -4 = not asked

lb <- lb %>%
  mutate(victim_dummy = case_when(victim == 1 ~ 1,
                                  victim == 3 ~ 1,
                                  victim == 4 ~ 0,
                                  victim == 2 ~ 0))

table(lb$victim_dummy, useNA = "always")

# Relative crime victim dummy
lb <- lb %>%
  mutate(relative_victim_dummy = case_when(victim == 2 ~ 1,
                                           victim == 3 ~ 1,
                                           victim == 4 ~ 0,
                                           victim == 1 ~ 0))

# Gender dummy
unique(gender)

# 1 = male
# 2 = female

lb <- lb %>%
  mutate(female_dummy = case_when(gender == 2 ~ 1,
                                  gender == 1 ~ 0))

table(lb$female_dummy, useNA = "always")

# White ethnicity dummy
unique(race)

# 1 = asian
# 2 = black
# 3 = indigenous
# 4 = Mestizo
# 5 = Mulato
# 6 = White
# 7 = Other
# -1 = DK
# -1 = No answer
# -4 not asked

lb <- lb %>%
  mutate(
    white_dummy = case_when(
      race == 6 ~ 1,
      race == 1 ~ 0,
      race == 2 ~ 0,
      race == 3 ~ 0,
      race == 4 ~ 0,
      race == 5 ~ 0,
      race == 7 ~ 0
    )
  )

table(lb$white_dummy, useNA = "always")

# Married dummy
unique(marital_status)

# 1 = Married / Living with partner
# 2 = Single
# 3 = Separated / Divorced / Widowed
# -1 = Don´t know / No answer
# -4 = Not asked

lb <- lb %>%
  mutate(married_dummy = case_when(
    marital_status == 1 ~ 1,
    marital_status == 2 ~ 0,
    marital_status == 3 ~ 0
  ))

table(lb$married_dummy, useNA = "always")

# Religion dummy
unique(religion)

# 1 = Catholic
# 2 = Evangelic without specification
# 3 = Evangelic Baptist
# 4 = Evangelic Methodist
# 5 = Evangelic Pentecostal
# 6 = Adventist
# 7 = Jehovah witness
# 8 = Mormon
# 9 = Jewish
# 10 = Protestant
# 11 = Afro-American Cult, Umbanda, etc
# 12 = Believer, not belonging to any church
# 13 = Agnostic
# 14 = Atheist
# 96 = other
# 97 = none
# -1 = dk
# -2 = no answer
# -4 = not asked

lb <- lb %>%
  mutate(
    catholic_dummy = case_when(
      religion == 1 ~ 1,
      religion == 2 ~ 0,
      religion == 3 ~ 0,
      religion == 4 ~ 0,
      religion == 5 ~ 0,
      religion == 6 ~ 0,
      religion == 7 ~ 0,
      religion == 8 ~ 0,
      religion == 9 ~ 0,
      religion == 10 ~ 0,
      religion == 11 ~ 0,
      religion == 12 ~ 0,
      religion == 13 ~ 0,
      religion == 14 ~ 0,
      religion == 96 ~ 0,
      religion == 97 ~ 0
    )
  )

table(lb$catholic_dummy, useNA = "always")

# Job employment dummy
unique(employment)

# 1 = self employed
# 2 = salaried employee in public company
# 3 = salaried employee in private company
# 4 = temporarily out of work
# 5 = retired
# 6 = don't work/responsible for shopping and house work
# 7 = student
# -1 = dk
# -2 = no answer

lb <- lb %>%
  mutate(
    employed_dummy = case_when(
      employment == 1 ~ 1,
      employment == 2 ~ 1,
      employment == 3 ~ 1,
      employment == 4 ~ 0,
      employment == 5 ~ 0,
      employment == 6 ~ 0,
      employment == 7 ~ 0
    )
  )

table(lb$employed_dummy, useNA = "always")

# Unemployment dummy
lb <- lb %>%
  mutate(
    unemployed_dummy = case_when(
      employment == 1 ~ 0,
      employment == 2 ~ 0,
      employment == 3 ~ 0,
      employment == 4 ~ 1,
      employment == 5 ~ 0,
      employment == 6 ~ 0,
      employment == 7 ~ 0
    )
  )

table(lb$unemployed_dummy, useNA = "always")

# Trust dummy
unique(trust)

# 1 = most people can be trusted
# 2 = one can never be too careful when dealing with others
# -2 = dk
# -4 = not asked

lb <- lb %>%
  mutate(trust_dummy = case_when(trust == 1 ~ 1,
                                 trust == 2 ~ 0))

table(lb$trust_dummy, useNA = "always")

# Subjective highly sufficient income dummy
unique(income)

# 1 = It is sufficient, you can save
# 2 = It is sufficient, without major problems
# 3 = It is not sufficient, you have problems
# 4 = It is not sufficient, you have big problems
# -1 = dk
# -2 = no answer
# -4 = not asked

lb <- lb %>%
  mutate(sufficient_income_dummy = case_when(income == 1 ~ 1,
                                             income == 2 ~ 0,
                                             income == 3 ~ 0,
                                             income == 4 ~ 0))

# Subjective highly insufficient income dummy
lb <- lb %>%
  mutate(insufficient_income_dummy = case_when(income == 1 ~ 0,
                                               income == 2 ~ 0,
                                               income == 3 ~ 0,
                                               income == 4 ~ 1))

# Life satisfaction dummy
unique(satisfaction_life)

# 1 = very satisfied
# 2 = fairly satisfied
# 3 = not very satisfied
# 4 = not at all satisfied
# -2 = no answer
# -4 not asked


lb <- lb %>%
  mutate(
    satisfaction_life_dummy = case_when(
      satisfaction_life == 1 ~ 1,
      satisfaction_life == 2 ~ 1,
      satisfaction_life == 3 ~ 0,
      satisfaction_life == 4 ~ 0
    )
  )

table(lb$satisfaction_life_dummy, useNA = "always")

# Satisfaction with democracy dummy
unique(satisfaction_democracy)

# 1 = very satisfied
# 2 = rather satisfied
# 3 = not very satisfied
# 4 = not at all satisfied
# -1 = dk
# -2 = no answer
# -3 = not applicable
# -4 not asked

lb <- lb %>%
  mutate(
    satisfaction_democracy_dummy = case_when(
      satisfaction_democracy == 1 ~ 1,
      satisfaction_democracy == 2 ~ 1,
      satisfaction_democracy == 3 ~ 0,
      satisfaction_democracy == 4 ~ 0
    )
  )

table(lb$satisfaction_democracy_dummy, useNA = "always")

# Trust in government dummy
unique(trust_government)

# 1 = alot of confidence
# 2 = some confidence
# 3 = little confidence
# 4 = no confidence at all
# -1 = dk
# -2 = no answer
# -4 = not asked

lb <- lb %>%
  mutate(
    trust_government_dummy = case_when(
      trust_government == 1 ~ 1,
      trust_government == 2 ~ 1,
      trust_government == 3 ~ 0,
      trust_government == 4 ~ 0
    )
  )

table(lb$trust_government_dummy, useNA = "always")

# Trust in judiciary dummy
unique(trust_judiciary)

# 1 = alot of confidence
# 2 = some confidence
# 3 = little confidence
# 4 = no confidence at all
# -1 = dk
# -2 = no answer
# -4 = not asked

lb <- lb %>%
  mutate(
    trust_judiciary_dummy = case_when(
      trust_judiciary == 1 ~ 1,
      trust_judiciary == 2 ~ 1,
      trust_judiciary == 3 ~ 0,
      trust_judiciary == 4 ~ 0
    )
  )

table(lb$trust_judiciary_dummy, useNA = "always")

# Trust in political parties dummy
unique(trust_parties)

# 1 = alot of confidence
# 2 = some confidence
# 3 = little confidence
# 4 = no confidence at all
# -1 = dk
# -2 = no answer
# -4 = not asked

lb <- lb %>%
  mutate(
    trust_parties_dummy = case_when(
      trust_parties == 1 ~ 1,
      trust_parties == 2 ~ 1,
      trust_parties == 3 ~ 0,
      trust_parties == 4 ~ 0
    )
  )

table(lb$trust_parties_dummy, useNA = "always")

# High school dummy
unique(education)

# 1 = without education
# 2 = 1 year
# 3 = 2 years
# 4 = 3
# 5 = 4
# 6 = 5
# 7 = 6
# 8 = 7
# 9 = 8
# 10 = 9
# 11 = 10
# 12 = 11
# 13 = 12
# 14 = incomplete university
# 15 = completed university
# 16 = high school/ academies/incomplete technical training
# 17 = high school/academies/complete technical training
# -1 = dk
# -2 = no answer

lb <- lb %>%
  mutate(
    high_school_dummy = case_when(
      education == 12 ~ 1,
      education == 14 ~ 1,
      education == 15 ~ 1,
      education == 16 ~ 1,
      education == 17 ~ 1,
      education == 1 ~ 0,
      education == 2 ~ 0,
      education == 3 ~ 0,
      education == 4 ~ 0,
      education == 5 ~ 0,
      education == 6 ~ 0,
      education == 7 ~ 0,
      education == 8 ~ 0,
      education == 9 ~ 0,
      education == 10 ~ 0,
      education == 11 ~ 0,
      education == 12 ~ 0
    )
  )

table(lb$high_school_dummy, useNA = "always")

# University dummy
lb <- lb %>%
  mutate(
    university_dummy = case_when(
      education == 12 ~ 0,
      education == 14 ~ 0,
      education == 15 ~ 1,
      education == 16 ~ 0,
      education == 17 ~ 0,
      education == 1 ~ 0,
      education == 2 ~ 0,
      education == 3 ~ 0,
      education == 4 ~ 0,
      education == 5 ~ 0,
      education == 6 ~ 0,
      education == 7 ~ 0,
      education == 8 ~ 0,
      education == 9 ~ 0,
      education == 10 ~ 0,
      education == 11 ~ 0,
      education == 12 ~ 0
    )
  )

table(lb$university_dummy, useNA = "always")

# Year dummies

# Year 2008
lb <- lb %>%
  mutate(
    year_2008_dummy = case_when(
      year == 2008 ~ 1,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2009
lb <- lb %>%
  mutate(
    year_2009_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 1,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2010
lb <- lb %>%
  mutate(
    year_2010_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 1,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2011
lb <- lb %>%
  mutate(
    year_2011_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 1,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2013
lb <- lb %>%
  mutate(
    year_2013_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 1,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2015
lb <- lb %>%
  mutate(
    year_2015_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 1,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2016
lb <- lb %>%
  mutate(
    year_2016_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 1,
      year == 2017 ~ 0,
      year == 2018 ~ 0
    )
  )

# Year 2017
lb <- lb %>%
  mutate(
    year_2017_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 1,
      year == 2018 ~ 0
    )
  )

# Year 2018
lb <- lb %>%
  mutate(
    year_2018_dummy = case_when(
      year == 2008 ~ 0,
      year == 2009 ~ 0,
      year == 2010 ~ 0,
      year == 2011 ~ 0,
      year == 2012 ~ 0,
      year == 2013 ~ 0,
      year == 2015 ~ 0,
      year == 2016 ~ 0,
      year == 2017 ~ 0,
      year == 2018 ~ 1
    )
  )

# Country dummies
unique(country)

# 32 = Argentina
# 68 = Bolivia
# 76 = Brazil
# 152 = Chile
# 170 = Colombia
# 188 = Costa Rica
# 214 = Rep. Dominicana
# 218 = Ecuador
# 222 = El Salvador
# 320 = Guatemala
# 340 = Honduras
# 484 = Mexico
# 558 = Nicaragua
# 591 = Panama
# 600 = Paraguay
# 604 = Peru
# 724 = Spain
# 858 = Uruguay
# 862 = Venezuela

# 32 = Argentina dummy
lb <- lb %>%
  mutate(
    argentina_dummy = case_when(
      country == 32 ~ 1,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 68 = Bolivia dummy
lb <- lb %>%
  mutate(
    bolivia_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 1,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 76 = Brazil dummy
lb <- lb %>%
  mutate(
    brazil_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 1,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 152 = Chile dummy
lb <- lb %>%
  mutate(
    chile_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 1,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 170 = Colombia dummy
lb <- lb %>%
  mutate(
    colombia_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 1,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 188 = Costa Rica dummy
lb <- lb %>%
  mutate(
    costa_rica_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 1,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 214 = Rep. Dominicana dummy
lb <- lb %>%
  mutate(
    dominican_republic_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 1,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 218 = Ecuador dummy
lb <- lb %>%
  mutate(
    ecuador_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 1,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 222 = El Salvador dummy
lb <- lb %>%
  mutate(
    el_salvador_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 1,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 320 = Guatemala dummy
lb <- lb %>%
  mutate(
    guatemala_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 1,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 340 = Honduras dummy
lb <- lb %>%
  mutate(
    honduras_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 1,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 484 = Mexico dummy
lb <- lb %>%
  mutate(
    mexico_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 1,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 558 = Nicaragua dummy
lb <- lb %>%
  mutate(
    nicaragua_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 1,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 591 = Panama dummy
lb <- lb %>%
  mutate(
    panama_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 1,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 600 = Paraguay dummy
lb <- lb %>%
  mutate(
    paraguay_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 1,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 604 = Peru dummy
lb <- lb %>%
  mutate(
    peru_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 1,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 0
    )
  )

# 858 = Uruguay dummy
lb <- lb %>%
  mutate(
    uruguay_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 1,
      country == 862 ~ 0
    )
  )

# 862 = Venezuela dummy
lb <- lb %>%
  mutate(
    venezuela_dummy = case_when(
      country == 32 ~ 0,
      country == 68 ~ 0,
      country == 76 ~ 0,
      country == 152 ~ 0,
      country == 170 ~ 0,
      country == 188 ~ 0,
      country == 214 ~ 0,
      country == 218 ~ 0,
      country == 222 ~ 0,
      country == 320 ~ 0,
      country == 340 ~ 0,
      country == 484 ~ 0,
      country == 558 ~ 0,
      country == 591 ~ 0,
      country == 600 ~ 0,
      country == 604 ~ 0,
      country == 724 ~ 0,
      country == 858 ~ 0,
      country == 862 ~ 1
    )
  )


# Clean dummy data frame --------------------------------------------------

# Remove null values
lb_dummies <- drop_na(lb)

lb_dummies <- lb_dummies %>%
  # Select "id", "age" and all columns ending with "_dummy"
  select(id, age, ends_with("_dummy")) %>%
  # Remove "_dummy" suffix from column names
  # "$" indicates that the string "_dummy" at the END of the string (column
  # name) should be replaced (in this case with an empty string)
  rename_all(~ stringr::str_replace(., "_dummy$", ""))

lb_dummies <- lb_dummies %>%
  select(-trust_parties)
# Save dummy data frame ---------------------------------------------------

save(lb_dummies, file = "../data/latinobarometro-dummies.RData")
write.csv(lb_dummies, file = "../data/latinobarometro-dummies.csv")
