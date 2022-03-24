# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Read data from other statistical formats
library(haven)

library(tidyverse)


# Load data files ---------------------------------------------------------

# Extract folders
unzip(zipfile = "../data/latinobarometro-1990s.zip", exdir = "../data")
unzip(zipfile = "../data/latinobarometro-2000s.zip", exdir = "../data")
unzip(zipfile = "../data/latinobarometro-2010s.zip", exdir = "../data")
unzip(zipfile = "../data/latinobarometro-2020s.zip", exdir = "../data")

# Read RData files
load(file = "../data/latinobarometro-2020-r/latinobarometro-2020.rdata")
load(file = "../data/latinobarometro-2017-r/latinobarometro-2017.rdata")
load(file = "../data/latinobarometro-2016-r/latinobarometro-2016.rdata")
load(file = "../data/latinobarometro-2015-r/latinobarometro-2015.rdata")
load(file = "../data/latinobarometro-2013-r/latinobarometro-2013.rdata")

# Rename data frames
lb_2020_raw <- Latinobarometro_2020_Eng
lb_2017_raw <- Latinobarometro2017Eng_v20180117
lb_2016_raw <- Latinobarometro2016Eng_v20170205
lb_2015_raw <- Latinobarometro_2015_Eng
lb_2013_raw <- Latinobarometro2013Eng

rm(Latinobarometro_2020_Eng)
rm(Latinobarometro2017Eng_v20180117)
rm(Latinobarometro2016Eng_v20170205)
rm(Latinobarometro_2015_Eng)
rm(Latinobarometro2013Eng)

# Read rds file
lb_2018_raw <-
  readRDS("../data/latinobarometro-2018-r/latinobarometro-2018.rds")

# Read SPSS files
lb_2011_raw <-
  read_sav("../data/latinobarometro-2011-spss/latinobarometro-2011.sav")
lb_2010_raw <-
  read_sav("../data/latinobarometro-2010-spss/latinobarometro-2010.sav")
lb_2009_raw <-
  read_sav("../data/latinobarometro-2009-spss/latinobarometro-2009.sav")
lb_2008_raw <-
  read_sav("../data/latinobarometro-2008-spss/latinobarometro-2008.sav")


# Select variables of interest --------------------------------------------

# Define lists of variables

lb_variables_2020 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "P13STGBS.B",
    # victim of crime
    "p64st",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "edad",
    # gender
    "sexo",
    # race
    "s12",
    # marital status
    # religion
    "s10",
    # years of education
    "s16",
    # employment status
    "s24.a",
    # general interpersonal trust
    "p9stgbs",
    # satisfaction with life
    "p1st",
    # satisfaction with democracy
    "P11STGBS.A",
    # confidence in the government
    "p13st.e",
    # confidence in the judiciary
    "p13st.f",
    # confidence in the political parties
    "p13st.g"
  )

lb_variables_2018 <-
  c(
    # identification number
    "NUMENTRE",
    # confidence in the police
    "P15STGBSC.B",
    # victim of crime
    "P69ST.2",
    # country identification
    "IDENPA",
    # region/geographical area
    "REG",
    # age
    "EDAD",
    # gender
    "SEXO",
    # race
    "S6",
    # marital status
    "S23",
    # religion
    "S5",
    # years of education
    "S10",
    # employment status
    "S14A",
    # general interpersonal trust
    "P11STGBS",
    # satisfaction with life
    "P1STC",
    # satisfaction with democracy
    "P13STGBS.A",
    # confidence in the government
    "P15STGBSC.E",
    # confidence in the judiciary
    "P15STGBSC.F",
    # confidence in the political parties
    "P15STGBSC.G",
    # subjective income 
    "S4"
  )

lb_variables_2017 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "P14STGBS.B",
    # victim of crime
    "P65ST.B",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "edad",
    # gender
    "sexo",
    # race
    "S10",
    # marital status
    "S6",
    # religion
    "S9",
    # years of education
    "S14",
    # employment status
    "S18.A",
    # general interpersonal trust
    "P13STGBS",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P9STGBSC.A",
    # confidence in the government
    "P14ST.E",
    # confidence in the judiciary
    "P14ST.F",
    # confidence in the political parties
    "P14ST.G",
    # subjective income
    "S5"
  )

lb_variables_2016 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "P13STGBSB",
    # victim of crime
    "P37ST",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "edad",
    # gender
    "sexo",
    # race
    "S9",
    # marital status
    "S5",
    # religion
    "S8",
    # years of education
    "S13",
    # employment status
    "S18A",
    # general interpersonal trust
    "P12STGBS",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P9STGBSA",
    # confidence in the government
    "P13STE",
    # confidence in the judiciary
    "P13STF",
    # confidence in the political parties
    "P13STG",
    # subjective income
    "S4"
  )

lb_variables_2015 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "P16TGB.B",
    # victim of crime
    "P60ST",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "S13",
    # gender
    "S12",
    # race
    "S23",
    # marital status
    "S11",
    # religion
    "S16",
    # years of education
    "S19",
    # employment status
    "S21.A",
    # general interpersonal trust
    "P15STGBS",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P12TG.A",
    # confidence in the government
    "P16ST.G",
    # confidence in the judiciary
    "P16ST.H",
    # confidence in the political parties
    "P19ST.C",
    # subjective income
    "S4"
  )

lb_variables_2013 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "P28TGB.B",
    # victim of crime
    "P68ST.A",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "S11",
    # gender
    "S10",
    # race
    "S21",
    # marital status
    "S9",
    # religion
    "S14",
    # education
    "S17",
    # employment status
    "S19.A",
    # general interpersonal trust
    "P29STGBS",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P13TGB.A",
    # confidence in the government
    "P26TGB.B",
    # confidence in the judiciary
    "P26TGB.E",
    # confidence in the political parties
    "P26TGB.G",
    # subjective income
    "S6"
  )

lb_variables_2011 <-
  c(
    # identification number
    "NUMENTRE",
    # confidence in the police
    "P20ST.C",
    # victim of crime
    "P80ST.A",
    # country identification
    "IDENPA",
    # region/geographical area
    "REG",
    # age
    "EDAD",
    # gender
    "SEXO",
    # race
    "S27",
    # marital status
    "S15",
    # religion
    "S18",
    # years of education
    "S21",
    # employment status
    "S23A",
    # general interpersonal trust
    "P25ST",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P14ST.A",
    # confidence in the government
    "P20ST.A",
    # confidence in the judiciary
    "P22ST.B",
    # confidence in the political parties
    "P22ST.C",
    # subjective income
    "S10ICC12"
  )

lb_variables_2010 <-
  c(
    # identification number
    "NUMENTRE",
    # confidence in the police
    "P18ST.C",
    # victim of crime
    "P70ST.A",
    # country identification
    "IDENPA",
    # region/geographical area
    "REG",
    # age
    "EDAD",
    # gender
    "SEXO",
    # race
    "S20",
    # marital status
    "S5",
    # religion
    "S9",
    # years of education
    "S14",
    # employment status
    "S16A",
    # general interpersonal trust
    "P55ST",
    # satisfaction with life
    "P1ST",
    # satisfaction with democracy
    "P11ST.A",
    # confidence in the government
    "P18ST.A",
    # confidence in the judiciary
    "P20ST.B",
    # confidence in the political parties
    "P20ST.C",
    # subjective income
    "S4"
  )

lb_variables_2009 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "p24st.c",
    # victim of crime
    "p73stm.a",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "s6",
    # gender
    "s5",
    # race
    "s18",
    # marital status
    "s3",
    # religion
    "s7",
    # years of education
    "s12",
    # employment status
    "s14a",
    # general interpersonal trust
    "p58st",
    # satisfaction with life
    "p1st",
    # satisfaction with democracy
    "p12st.a",
    # confidence in the government
    "p24st.a",
    # confidence in the judiciary
    "p26st.b",
    # confidence in the political parties
    "p26st.c",
    # subjective income 
    "s2"
  )

lb_variables_2008 <-
  c(
    # identification number
    "numentre",
    # confidence in the police
    "p31st.c",
    # victim of crime
    "p72st.a",
    # country identification
    "idenpa",
    # region/geographical area
    "reg",
    # age
    "s9",
    # gender
    "s8",
    # race
    "s11",
    # marital status
    "s3",
    # religion
    "s5",
    # education
    "s15",
    # employment status
    "s17a",
    # general interpersonal trust
    "p21wvsst",
    # satisfaction with life
    "p27st",
    # satisfaction with democracy
    "p22st.a",
    # confidence in the government
    "p31s.ta",
    # confidence in the judiciary
    "p28st.b",
    # confidence in the political parties
    "p28st.c",
    # subjective income
    "s2"
  )

# Select and rename variables and add a corresponding year variable

lb_2020 <- lb_2020_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2020)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = P13STGBS.B,
    victim = p64st,
    country = idenpa,
    region = reg,
    age = edad,
    gender = sexo,
    race = s12,
    religion = s10,
    education = s16,
    employment = s24.a,
    trust = p9stgbs,
    satisfaction_life = p1st,
    satisfaction_democracy = P11STGBS.A,
    trust_government = p13st.e,
    trust_judiciary = p13st.f,
    trust_parties = p13st.g
  ) %>%
  # Add year column
  mutate(year = 2020, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2018 <- lb_2018_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2018)) %>%
  # Rename columns
  rename(
    id = NUMENTRE,
    trust_police = P15STGBSC.B,
    victim = P69ST.2,
    country = IDENPA,
    region = REG,
    age = EDAD,
    gender = SEXO,
    race = S6,
    marital_status = S23,
    religion = S5,
    education = S10,
    employment = S14A,
    trust = P11STGBS,
    satisfaction_life = P1STC,
    satisfaction_democracy = P13STGBS.A,
    trust_government = P15STGBSC.E,
    trust_judiciary = P15STGBSC.F,
    trust_parties = P15STGBSC.G,
    income = S4
  ) %>%
  # Add year column
  mutate(year = 2018, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2017 <- lb_2017_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2017)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = P14STGBS.B,
    victim = P65ST.B,
    country = idenpa,
    region = reg,
    age = edad,
    gender = sexo,
    race = S10,
    marital_status = S6,
    religion = S9,
    education = S14,
    employment = S18.A,
    trust = P13STGBS,
    satisfaction_life = P1ST,
    satisfaction_democracy = P9STGBSC.A,
    trust_government = P14ST.E,
    trust_judiciary = P14ST.F,
    trust_parties = P14ST.G,
    income = S5
  ) %>%
  # Add year column
  mutate(year = 2017, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2016 <- lb_2016_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2016)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = P13STGBSB,
    victim = P37ST,
    country = idenpa,
    region = reg,
    age = edad,
    gender = sexo,
    race = S9,
    marital_status = S5,
    religion = S8,
    education = S13,
    employment = S18A,
    trust = P12STGBS,
    satisfaction_life = P1ST,
    satisfaction_democracy = P9STGBSA,
    trust_government = P13STE,
    trust_judiciary = P13STF,
    trust_parties = P13STG,
    income = S4
  ) %>%
  # Add year column
  mutate(year = 2016, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2015 <- lb_2015_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2015)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = P16TGB.B,
    victim = P60ST,
    country = idenpa,
    region = reg,
    age = S13,
    gender = S12,
    race = S23,
    marital_status = S11,
    religion = S16,
    education = S19,
    employment = S21.A,
    trust = P15STGBS,
    satisfaction_life = P1ST,
    satisfaction_democracy = P12TG.A,
    trust_government = P16ST.G,
    trust_judiciary = P16ST.H,
    trust_parties = P19ST.C,
    income = S4
  ) %>%
  # Add year column
  mutate(year = 2015, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2013 <- lb_2013_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2013)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = P28TGB.B,
    victim = P68ST.A,
    country = idenpa,
    region = reg,
    age = S11,
    gender = S10,
    race = S21,
    marital_status = S9,
    religion = S14,
    education = S17,
    employment = S19.A,
    trust = P29STGBS,
    satisfaction_life = P1ST,
    satisfaction_democracy = P13TGB.A,
    trust_government = P26TGB.B,
    trust_judiciary = P26TGB.E,
    trust_parties = P26TGB.G,
    income = S6
  ) %>%
  # Add year column
  mutate(year = 2013, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2011 <- lb_2011_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2011)) %>%
  # Rename columns
  rename(
    id = NUMENTRE,
    trust_police = P20ST.C,
    victim = P80ST.A,
    country = IDENPA,
    region = REG,
    age = EDAD,
    gender = SEXO,
    race = S27,
    marital_status = S15,
    religion = S18,
    education = S21,
    employment = S23A,
    trust = P25ST,
    satisfaction_life = P1ST,
    satisfaction_democracy = P14ST.A,
    trust_government = P20ST.A,
    trust_judiciary = P22ST.B,
    trust_parties = P22ST.C,
    income = S10ICC12
  ) %>%
  # Add year column
  mutate(year = 2011, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2010 <- lb_2010_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2010)) %>%
  # Rename columns
  rename(
    id = NUMENTRE,
    trust_police = P18ST.C,
    victim = P70ST.A,
    country = IDENPA,
    region = REG,
    age = EDAD,
    gender = SEXO,
    race = S20,
    marital_status = S5,
    religion = S9,
    education = S14,
    employment = S16A,
    trust = P55ST,
    satisfaction_life = P1ST,
    satisfaction_democracy = P11ST.A,
    trust_government = P18ST.A,
    trust_judiciary = P20ST.B,
    trust_parties = P20ST.C,
    income = S4
  ) %>%
  # Add year column
  mutate(year = 2010, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2009 <- lb_2009_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2009)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = p24st.c,
    victim = p73stm.a,
    country = idenpa,
    region = reg,
    age = s6,
    gender = s5,
    race = s18,
    marital_status = s3,
    religion = s7,
    education = s12,
    employment = s14a,
    trust = p58st,
    satisfaction_life = p1st,
    satisfaction_democracy = p12st.a,
    trust_government = p24st.a,
    trust_judiciary = p26st.b,
    trust_parties = p26st.c,
    income = s2
  ) %>%
  # Add year column
  mutate(year = 2009, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )

lb_2008 <- lb_2008_raw %>%
  # Select columns of interest
  select(all_of(lb_variables_2008)) %>%
  # Rename columns
  rename(
    id = numentre,
    trust_police = p31st.c,
    victim = p72st.a,
    country = idenpa,
    region = reg,
    age = s9,
    gender = s8,
    race = s11,
    marital_status = s3,
    religion = s5,
    education = s15,
    employment = s17a,
    trust = p21wvsst,
    satisfaction_life = p27st,
    satisfaction_democracy = p22st.a,
    trust_government = p31s.ta,
    trust_judiciary = p28st.b,
    trust_parties = p28st.c,
    income = s2
  ) %>%
  # Add year column
  mutate(year = 2008, .before = id) %>%
  # Rewrite id column
  mutate(# Remove space between year and row number create by the paste function
    id = gsub(" ",
              "",
              # Concatenate the year and the row number to form a unique id
              # number
              paste(
                year, formatC(
                  row_number(),
                  width = 5,
                  format = "d",
                  flag = "0"
                )
              )
    )
  )


# Change variable data types ----------------------------------------------

lb_2020 <- lapply(lb_2020, as.numeric)
lb_2018 <- lapply(lb_2018, as.numeric)
lb_2017 <- lapply(lb_2017, as.numeric)
lb_2016 <- lapply(lb_2016, as.numeric)
lb_2015 <- lapply(lb_2015, as.numeric)
lb_2013 <- lapply(lb_2013, as.numeric)
lb_2011 <- lapply(lb_2011, as.numeric)
lb_2010 <- lapply(lb_2010, as.numeric)
lb_2009 <- lapply(lb_2009, as.numeric)
lb_2008 <- lapply(lb_2008, as.numeric)


# Binding data frames -----------------------------------------------------

# Combine yearly data frames into one large data frame
lb <-
  bind_rows(
    lb_2018,
    lb_2017,
    lb_2016,
    lb_2015,
    lb_2013,
    lb_2011,
    lb_2010,
    lb_2009,
    lb_2008
  )

# Remove/reset row names
rownames(lb) <- NULL

# Reorder columns
lb <- lb %>%
  select(
    year,
    id,
    trust_police,
    victim,
    country,
    region,
    gender,
    age,
    race,
    marital_status,
    religion,
    education,
    employment,
    trust,
    satisfaction_life,
    satisfaction_democracy,
    trust_government,
    trust_judiciary,
    trust_parties,
    income
  )

# Drop null values
lb <- drop_na(lb)

summary(lb)

# Save "lb" data frame as RData -------------------------------------------

save(lb, file = "../data/latinobarometro.RData")
write.csv(lb, file= "../data/latinobarometro.csv")
