## Data processing

# Load packages 
# Read in and prep data 
# Visualize normality of data
# Correlation matrix
# Factor Analysis (FA)
# Global Moran's I of Factor Scores
# Spatial Factor Analysis (sFA)
# Weighted Quantile Sums? (WQS)
# Compare model fit (Z-score, FA, sFA, WQS?)


# Load packages

I'm not sure if we will use all of these packages, but these were all the packages within the setup syntax

library(RODBC)
library(tidyverse)
library(dplyr)
library(psych)
library(Hmisc)
library(ggrepel)
library(corrplot)
library(car)
library(tidyquant)
library(MASS)
library(stringr)
library(janitor)
library(pastecs)
library(ggpmisc)
library(stats)
library(psy)
library(ltm)
library(lavaan)
library(sem)
library(foreign)


library(naniar)

# Pulling Philadelphia 09 & 17 ACS 5-Year data sets and merging

philly_2009 <- read_csv("Philadelphia_2009_ACS.csv")
philly_2017 <- read_csv("Philadelphia_2017_ACS.csv")


philly_2008a <- read.spss("hs08ar1.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
#philly_2008a %>% sapply(levels)

philly_2008a <- dplyr::select(philly_2008a, TRACT, GETSSDI, GETSSI, GETSTAMP, GETTANF, GETWIC, INCOME, MAINEMPL, NPOV100, NPOV150, NPOV200, RESPEMPL, BELONG, NUM60, NUMADULT, NUMKIDS, NUMLIVHH, ORIGIN, OTHENG, PARTICIP, FINDFRUT, GROCERY, USUALGOA, RENTOWN, BMI, HOSPERA, INSUREDA) %>% 
mutate(TRACT = TRACT*100,
GETSSDI = case_when(GETSSDI == "Yes" ~ 0, 
GETSSDI == "No" ~ 1,
GETSSDI == "Not heard of" ~ 99,
GETSSDI == "Don't Know" ~ 99,
GETSSDI == "Refused" ~ 99),
GETSSI = case_when(GETSSI == "Yes" ~ 0, 
GETSSI == "No" ~ 1,
GETSSI == "Not heard of" ~ 99,
GETSSI == "Don't Know" ~ 99,
GETSSI == "Refused" ~ 99),
GETSTAMP = case_when(GETSTAMP == "Yes" ~ 0, 
GETSTAMP == "No" ~ 1,
GETSTAMP == "Not heard of" ~ 99,
GETSTAMP == "Don't Know" ~ 99,
GETSTAMP == "Refused" ~ 99),
GETTANF = case_when(GETTANF == "Yes" ~ 0, 
GETTANF == "No" ~ 1,
GETTANF == "Not heard of" ~ 99,
GETTANF == "Don't Know" ~ 99,
GETTANF == "Refused" ~ 99),
GETWIC = case_when(GETWIC == "Yes" ~ 0, 
GETWIC == "No" ~ 1,
GETWIC == "Not heard of" ~ 99,
GETWIC == "Don't Know" ~ 99,
GETWIC == "Refused" ~ 99),
MAINEMPL = case_when(MAINEMPL == "Employed full-time" ~ 1, 
MAINEMPL == "Employed part-time" ~ 1,
MAINEMPL == "Unemployed but looking for work" ~ 0,
MAINEMPL == "Unemployed and not looking for work" ~ 0,
MAINEMPL == "Retired" ~ 1,
MAINEMPL == "Unable to work-disabled" ~ 0,
MAINEMPL == "Homemaker" ~ 1,
MAINEMPL == "Full-time student/Job training" ~ 1,
MAINEMPL == "Don't Know" ~ 99,
MAINEMPL == "Refused" ~ 99),
NPOV100 = case_when(NPOV100 == "poor" ~ 0, 
NPOV100 == "nonpoor" ~ 1),
NPOV150 = case_when(NPOV150 == "poor" ~ 0, 
NPOV150 == "nonpoor" ~ 1),
NPOV200 = case_when(NPOV200 == "poor" ~ 0, 
NPOV200 == "nonpoor" ~ 1),
RESPEMPL = case_when(RESPEMPL == "Employed full-time" ~ 1, 
RESPEMPL == "Employed part-time" ~ 1,
RESPEMPL == "Unemployed but looking for work" ~ 0,
RESPEMPL == "Unemployed and not looking for work" ~ 0,
RESPEMPL == "Retired" ~ 1,
RESPEMPL == "Unable to work-disabled" ~ 0,
RESPEMPL == "Homemaker" ~ 1,
RESPEMPL == "Full-time student/Job training" ~ 1,
RESPEMPL == "Don't Know" ~ 99,
RESPEMPL == "Refused" ~ 99),
BELONG = case_when(BELONG == "Strongly Agree" ~ 3, 
BELONG == "Agree" ~ 2,
BELONG == "Disagree" ~ 1,
BELONG == "Strongly Disagree" ~ 0,
BELONG == "Don't Know" ~ 99,
BELONG == "Refused" ~ 99),
NUM60 = case_when(NUM60 == "None" ~ 0, 
NUM60 == "1" ~ 1,
NUM60 == "2" ~ 2,
NUM60 == "3" ~ 3,
NUM60 == "4" ~ 4,
NUM60 == "5" ~ 5,
NUM60 == "6" ~ 6,
NUM60 == "7" ~ 7,
NUM60 == "8" ~ 8,
NUM60 == "9" ~ 9,
NUM60 == "10" ~ 10,
NUM60 == "11" ~ 11,
NUM60 == "12" ~ 12,
NUM60 == "13" ~ 13,
NUM60 == "14" ~ 14,
NUM60 == "15 or more" ~ 15),
NUMADULT = case_when(NUMADULT == "None" ~ 0, 
NUMADULT == "1" ~ 1,
NUMADULT == "2" ~ 2,
NUMADULT == "3" ~ 3,
NUMADULT == "4" ~ 4,
NUMADULT == "5" ~ 5,
NUMADULT == "6" ~ 6,
NUMADULT == "7" ~ 7,
NUMADULT == "8" ~ 8,
NUMADULT == "9" ~ 9,
NUMADULT == "10" ~ 10,
NUMADULT == "11" ~ 11,
NUMADULT == "12" ~ 12,
NUMADULT == "13" ~ 13,
NUMADULT == "14" ~ 14,
NUMADULT == "15 or more" ~ 15),
NUMKIDS = case_when(NUMKIDS == "None" ~ 0, 
NUMKIDS == "1" ~ 1,
NUMKIDS == "2" ~ 2,
NUMKIDS == "3" ~ 3,
NUMKIDS == "4" ~ 4,
NUMKIDS == "5" ~ 5,
NUMKIDS == "6" ~ 6,
NUMKIDS == "7" ~ 7,
NUMKIDS == "8" ~ 8,
NUMKIDS == "9" ~ 9,
NUMKIDS == "10" ~ 10,
NUMKIDS == "11" ~ 11,
NUMKIDS == "12" ~ 12,
NUMKIDS == "13" ~ 13,
NUMKIDS == "14" ~ 14,
NUMKIDS == "15 or more" ~ 15),
NUMLIVHH = case_when(NUMLIVHH == "0" ~ 0, 
NUMLIVHH == "1" ~ 1,
NUMLIVHH == "2" ~ 2,
NUMLIVHH == "3" ~ 3,
NUMLIVHH == "4" ~ 4,
NUMLIVHH == "5" ~ 5,
NUMLIVHH == "6" ~ 6,
NUMLIVHH == "7" ~ 7,
NUMLIVHH == "8" ~ 8,
NUMLIVHH == "9" ~ 9,
NUMLIVHH == "10" ~ 10,
NUMLIVHH == "11" ~ 11,
NUMLIVHH == "12" ~ 12,
NUMLIVHH == "13" ~ 13,
NUMLIVHH == "14" ~ 14,
NUMLIVHH == "15" ~ 15,
NUMLIVHH == "16" ~ 16,
NUMLIVHH == "17" ~ 17,
NUMLIVHH == "18" ~ 18,
NUMLIVHH == "19" ~ 19,
NUMLIVHH == "20" ~ 20,
NUMLIVHH == "21" ~ 21,
NUMLIVHH == "22" ~ 22,
NUMLIVHH == "23" ~ 23,
NUMLIVHH == "24" ~ 24,
NUMLIVHH == "25" ~ 25),
ORIGIN = case_when(ORIGIN != "United States of America" ~ 1,
ORIGIN == "United States of America" ~ 0),
OTHENG = case_when(OTHENG != "Don't Know" | OTHENG != "Refused" ~ 1,
OTHENG == "Don't Know" | OTHENG == "Refused" ~ 0),
PARTICIP = case_when(PARTICIP == "0" ~ 0, 
PARTICIP == "1" ~ 1,
PARTICIP == "2" ~ 2,
PARTICIP == "3" ~ 3,
PARTICIP == "4" ~ 4,
PARTICIP == "5" ~ 5,
PARTICIP == "6" ~ 6,
PARTICIP == "7" ~ 7,
PARTICIP == "8" ~ 8,
PARTICIP == "9" ~ 9,
PARTICIP == "10" ~ 10,
PARTICIP == "11" ~ 11,
PARTICIP == "12" ~ 12),
FINDFRUT = case_when(FINDFRUT == "Very easy" ~ 3, 
FINDFRUT == "Easy" ~ 2,
FINDFRUT == "Difficult" ~ 1,
FINDFRUT == "Very Difficult" ~ 0,
FINDFRUT == "Don't Know" ~ 99,
FINDFRUT == "Refused" ~ 99),
GROCERY = case_when(GROCERY == "Excellent" ~ 3, 
GROCERY == "Good" ~ 2,
GROCERY == "Fair" ~ 1,
GROCERY == "Poor" ~ 0,
GROCERY == "Don't Know" ~ 99,
GROCERY == "Refused" ~ 99),
USUALGOA = case_when(USUALGOA == "Private doctor's office" ~ 4, 
USUALGOA == "Community health center or public clinic" ~ 3,
USUALGOA == "Hospital outpatient clinic" ~ 2,
USUALGOA == "Hospital emergency room" ~ 1,
USUALGOA == "Other place" ~ 0),
RENTOWN = case_when(RENTOWN == "Rent" ~ 0, 
RENTOWN == "Own" ~ 1,
RENTOWN == "Other arrangement" ~ 0,
RENTOWN == "Don't Know" ~ 99,
RENTOWN == "Refused" ~ 99),
HOSPERA = case_when(HOSPERA == "0" ~ 0, 
HOSPERA == "1" ~ 1,
HOSPERA == "2" ~ 2,
HOSPERA == "3" ~ 3,
HOSPERA == "4" ~ 4,
HOSPERA == "5" ~ 5,
HOSPERA == "6" ~ 6,
HOSPERA == "7" ~ 7,
HOSPERA == "8" ~ 8,
HOSPERA == "9" ~ 9,
HOSPERA == "10" ~ 10,
HOSPERA == "11" ~ 11,
HOSPERA == "12+ visits" ~ 12),
INSUREDA = case_when(INSUREDA == "Yes" ~ 1, 
INSUREDA == "No" ~ 0)
) %>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))

#str(philly_2008a, list.len=ncol(philly_2008a))


philly_2008aa <- as_tibble(philly_2008a) %>% 
group_by(TRACT) %>% 
summarise(GETSSDI_08_m = mean(GETSSDI), 
GETSSI_08_m = mean(GETSSI), 
GETSTAMP_08_m = mean(GETSTAMP), 
GETTANF_08_m = mean(GETTANF), 
GETWIC_08_m = mean(GETWIC), 
MAINEMPL_08_m = mean(MAINEMPL), 
NPOV100_08_m = mean(NPOV100), 
NPOV150_08_m = mean(NPOV150), 
NPOV200_08_m = mean(NPOV200),
RESPEMPL_08_m = mean(RESPEMPL), 
BELONG_08_m = mean(BELONG), 
NUM60_08_m = mean(NUM60), 
NUMADULT_08_m = mean(NUMADULT), 
NUMKIDS_08_m = mean(NUMKIDS), 
NUMLIVHH_08_m = mean(NUMLIVHH), 
ORIGIN_08_m = mean(ORIGIN), 
OTHENG_08_m = mean(OTHENG), 
PARTICIP_08_m = mean(PARTICIP), 
FINDFRUT_08_m = mean(FINDFRUT), 
GROCERY_08_m = mean(GROCERY), 
USUALGOA_08_m = mean(USUALGOA), 
RENTOWN_08_m = mean(RENTOWN), 
BMI_08_m = mean(BMI), 
HOSPERA_08_m = mean(HOSPERA), 
INSUREDA_08_m = mean(INSUREDA),
n_08_a = n()) 




philly_2008b <- read.spss("hs08chd1.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
philly_2008b <- dplyr::select(philly_2008b, TRACT, BMIPCT, CHIP, INSUREDC, EVRASTHC) %>% 
mutate(TRACT = TRACT*100,
CHIP = case_when(CHIP == "Yes" ~ 0, 
CHIP == "No" ~ 1),
INSUREDC = case_when(INSUREDC == "Yes" ~ 0, 
INSUREDC == "No" ~ 1),
EVRASTHC = case_when(EVRASTHC == "Yes" ~ 0, 
EVRASTHC == "No" ~ 1)) %>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))


#str(philly_2008b, list.len=ncol(philly_2008b))
#philly_2008b %>% sapply(levels)


philly_2008bb <- as_tibble(philly_2008b) %>% 
group_by(TRACT) %>% 
summarise(BMIPCT_08_m = mean(BMIPCT), 
CHIP_08_m = mean(CHIP), 
INSUREDC_08_m = mean(INSUREDC), 
EVRASTHC_08_m = mean(EVRASTHC),
n_08_b = n())





philly_2008c <- read.spss("hs0860s1.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
philly_2008c <- dplyr::select(philly_2008c, TRACT, NTRAN, DEPRESS) %>% 
mutate(TRACT = TRACT*100,
NTRAN = case_when(NTRAN == "Yes" ~ 0, 
NTRAN == "No" ~ 1),
DEPRESS = case_when(DEPRESS == "Yes" ~ 0, 
DEPRESS == "No" ~ 1))%>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))

#str(philly_2008c, list.len=ncol(philly_2008c))
#philly_2008c %>% sapply(levels)

philly_2008cc <- as_tibble(philly_2008c) %>% 
group_by(TRACT) %>% 
summarise(NTRAN_08_m = mean(NTRAN), 
DEPRESS_08_m = mean(DEPRESS),
n_08_c = n())

philly_2008ab  <- inner_join(philly_2008aa, philly_2008bb, by = "TRACT")
philly_2008    <- inner_join(philly_2008ab, philly_2008cc, by = "TRACT") %>% 
mutate(n = n_08_a + n_08_b + n_08_c) %>% 
dplyr::select(-n_08_a,-n_08_b,-n_08_c)











#NPOV50

philly_2015a <- read.spss("HS15AR1b.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
philly_2015a <- dplyr::select(philly_2015a, TRACT10, GETSSDI, GETSSI, GETSTAMP, GETTANF, GETWIC, INCOME, MAINEMPL, NPOV100, NPOV150, NPOV200, RESPEMPL, BELONG, NUM60, NUMADULT, NUMKIDS, NUMLIVHH, ORIGIN, OTHENG, PARTICIP, FINDFRUT, GROCERY, USUALGOA, RENTOWN, BMI, HOSPERA, INSUREDA) %>% 
mutate(TRACT10 = TRACT10*100,
GETSSDI = case_when(GETSSDI == "Yes" ~ 0, 
GETSSDI == "No" ~ 1,
GETSSDI == "Not heard of" ~ 99,
GETSSDI == "Don't Know" ~ 99,
GETSSDI == "Refused" ~ 99),
GETSSI = case_when(GETSSI == "Yes" ~ 0, 
GETSSI == "No" ~ 1,
GETSSI == "Not heard of" ~ 99,
GETSSI == "Don't Know" ~ 99,
GETSSI == "Refused" ~ 99),
GETSTAMP = case_when(GETSTAMP == "Yes" ~ 0, 
GETSTAMP == "No" ~ 1,
GETSTAMP == "Not heard of" ~ 99,
GETSTAMP == "Don't Know" ~ 99,
GETSTAMP == "Refused" ~ 99),
GETTANF = case_when(GETTANF == "Yes" ~ 0, 
GETTANF == "No" ~ 1,
GETTANF == "Not heard of" ~ 99,
GETTANF == "Don't Know" ~ 99,
GETTANF == "Refused" ~ 99),
GETWIC = case_when(GETWIC == "Yes" ~ 0, 
GETWIC == "No" ~ 1,
GETWIC == "Not heard of" ~ 99,
GETWIC == "Don't Know" ~ 99,
GETWIC == "Refused" ~ 99),
MAINEMPL = case_when(MAINEMPL == "Employed full-time" ~ 1, 
MAINEMPL == "Employed part-time" ~ 1,
MAINEMPL == "Unemployed but looking for work" ~ 0,
MAINEMPL == "Unemployed and not looking for work" ~ 0,
MAINEMPL == "Retired" ~ 1,
MAINEMPL == "Unable to work-disabled" ~ 0,
MAINEMPL == "Homemaker" ~ 1,
MAINEMPL == "Full-time student/Job training" ~ 1,
MAINEMPL == "Don't Know" ~ 99,
MAINEMPL == "Refused" ~ 99),
#NPOV50 = case_when(NPOV50 == "poor" ~ 0, 
#                   NPOV50 == "nonpoor" ~ 1),
NPOV100 = case_when(NPOV100 == "poor" ~ 0, 
NPOV100 == "nonpoor" ~ 1),
NPOV150 = case_when(NPOV150 == "poor" ~ 0, 
NPOV150 == "nonpoor" ~ 1),
NPOV200 = case_when(NPOV200 == "poor" ~ 0, 
NPOV200 == "nonpoor" ~ 1),
RESPEMPL = case_when(RESPEMPL == "Employed full-time" ~ 1, 
RESPEMPL == "Employed part-time" ~ 1,
RESPEMPL == "Unemployed but looking for work" ~ 0,
RESPEMPL == "Unemployed and not looking for work" ~ 0,
RESPEMPL == "Retired" ~ 1,
RESPEMPL == "Unable to work-disabled" ~ 0,
RESPEMPL == "Homemaker" ~ 1,
RESPEMPL == "Full-time student/Job training" ~ 1,
RESPEMPL == "Don't Know" ~ 99,
RESPEMPL == "Refused" ~ 99),
BELONG = case_when(BELONG == "Strongly Agree" ~ 3, 
BELONG == "Agree" ~ 2,
BELONG == "Disagree" ~ 1,
BELONG == "Strongly Disagree" ~ 0),
NUM60 = case_when(NUM60 == "None" ~ 0, 
NUM60 == "1" ~ 1,
NUM60 == "2" ~ 2,
NUM60 == "3" ~ 3,
NUM60 == "4" ~ 4,
NUM60 == "5" ~ 5,
NUM60 == "6" ~ 6,
NUM60 == "7" ~ 7,
NUM60 == "8" ~ 8,
NUM60 == "9" ~ 9,
NUM60 == "10" ~ 10,
NUM60 == "11" ~ 11,
NUM60 == "12" ~ 12,
NUM60 == "13" ~ 13,
NUM60 == "14" ~ 14,
NUM60 == "15 or more" ~ 15),
NUMADULT = case_when(NUMADULT == "None" ~ 0, 
NUMADULT == "1" ~ 1,
NUMADULT == "2" ~ 2,
NUMADULT == "3" ~ 3,
NUMADULT == "4" ~ 4,
NUMADULT == "5" ~ 5,
NUMADULT == "6" ~ 6,
NUMADULT == "7" ~ 7,
NUMADULT == "8" ~ 8,
NUMADULT == "9" ~ 9,
NUMADULT == "10" ~ 10,
NUMADULT == "11" ~ 11,
NUMADULT == "12" ~ 12,
NUMADULT == "13" ~ 13,
NUMADULT == "14" ~ 14,
NUMADULT == "15 or more" ~ 15),
NUMKIDS = case_when(NUMKIDS == "None" ~ 0, 
NUMKIDS == "1" ~ 1,
NUMKIDS == "2" ~ 2,
NUMKIDS == "3" ~ 3,
NUMKIDS == "4" ~ 4,
NUMKIDS == "5" ~ 5,
NUMKIDS == "6" ~ 6,
NUMKIDS == "7" ~ 7,
NUMKIDS == "8" ~ 8,
NUMKIDS == "9" ~ 9,
NUMKIDS == "10" ~ 10,
NUMKIDS == "11" ~ 11,
NUMKIDS == "12" ~ 12,
NUMKIDS == "13" ~ 13,
NUMKIDS == "14" ~ 14,
NUMKIDS == "15 or more" ~ 15),
NUMLIVHH = case_when(NUMLIVHH == "0" ~ 0, 
NUMLIVHH == "1" ~ 1,
NUMLIVHH == "2" ~ 2,
NUMLIVHH == "3" ~ 3,
NUMLIVHH == "4" ~ 4,
NUMLIVHH == "5" ~ 5,
NUMLIVHH == "6" ~ 6,
NUMLIVHH == "7" ~ 7,
NUMLIVHH == "8" ~ 8,
NUMLIVHH == "9" ~ 9,
NUMLIVHH == "10" ~ 10,
NUMLIVHH == "11" ~ 11,
NUMLIVHH == "12" ~ 12,
NUMLIVHH == "13" ~ 13,
NUMLIVHH == "14" ~ 14,
NUMLIVHH == "15" ~ 15,
NUMLIVHH == "16" ~ 16,
NUMLIVHH == "17" ~ 17,
NUMLIVHH == "18" ~ 18,
NUMLIVHH == "19" ~ 19,
NUMLIVHH == "20" ~ 20,
NUMLIVHH == "21" ~ 21,
NUMLIVHH == "22" ~ 22,
NUMLIVHH == "23" ~ 23,
NUMLIVHH == "24" ~ 24,
NUMLIVHH == "25" ~ 25),
ORIGIN = case_when(ORIGIN != "United States of America" ~ 1,
ORIGIN == "United States of America" ~ 0),
OTHENG = case_when(OTHENG != "Don't Know" | OTHENG != "Refused" ~ 1,
OTHENG == "Don't Know" | OTHENG == "Refused" ~ 0),
PARTICIP = case_when(PARTICIP == "0" ~ 0, 
PARTICIP == "1" ~ 1,
PARTICIP == "2" ~ 2,
PARTICIP == "3" ~ 3,
PARTICIP == "4" ~ 4,
PARTICIP == "5" ~ 5,
PARTICIP == "6" ~ 6,
PARTICIP == "7" ~ 7,
PARTICIP == "8" ~ 8,
PARTICIP == "9" ~ 9,
PARTICIP == "10" ~ 10,
PARTICIP == "11" ~ 11,
PARTICIP == "12" ~ 12),
FINDFRUT = case_when(FINDFRUT == "Very easy" ~ 3, 
FINDFRUT == "Easy" ~ 2,
FINDFRUT == "Difficult" ~ 1,
FINDFRUT == "Very Difficult" ~ 0,
FINDFRUT == "Don't Know" ~ 99,
FINDFRUT == "Refused" ~ 99),
GROCERY = case_when(GROCERY == "Excellent" ~ 3, 
GROCERY == "Good" ~ 2,
GROCERY == "Fair" ~ 1,
GROCERY == "Poor" ~ 0,
GROCERY == "Don't Know" ~ 99,
GROCERY == "Refused" ~ 99),
USUALGOA = case_when(USUALGOA == "Private doctor's office" ~ 4, 
USUALGOA == "Community health center or public clinic" ~ 3,
USUALGOA == "Hospital outpatient clinic" ~ 2,
USUALGOA == "Hospital emergency room" ~ 1,
USUALGOA == "Other place" ~ 0),
RENTOWN = case_when(RENTOWN == "Rent" ~ 0, 
RENTOWN == "Own" ~ 1,
RENTOWN == "Other arrangement" ~ 0,
RENTOWN == "Don't Know" ~ 99,
RENTOWN == "Refused" ~ 99),
HOSPERA = case_when(HOSPERA == "0" ~ 0, 
HOSPERA == "1" ~ 1,
HOSPERA == "2" ~ 2,
HOSPERA == "3" ~ 3,
HOSPERA == "4" ~ 4,
HOSPERA == "5" ~ 5,
HOSPERA == "6" ~ 6,
HOSPERA == "7" ~ 7,
HOSPERA == "8" ~ 8,
HOSPERA == "9" ~ 9,
HOSPERA == "10" ~ 10,
HOSPERA == "11" ~ 11,
HOSPERA == "12+ visits" ~ 12),
INSUREDA = case_when(INSUREDA == "Yes" ~ 1, 
INSUREDA == "No" ~ 0)
) %>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))

#head(philly_2015a)



# Deciding not to use income since we already have median hh income and it would be time consuming to recode

#str(philly_2015a, list.len=ncol(philly_2015a))

#philly_2015a %>% sapply(levels)

# Make thematic map of count within census tracts to see if systematic areas have low numbers

philly_2015aa <- as_tibble(philly_2015a) %>% 
group_by(TRACT10) %>% 
summarise(GETSSDI_15_m = mean(GETSSDI), 
GETSSI_15_m = mean(GETSSI), 
GETSTAMP_15_m = mean(GETSTAMP), 
GETTANF_15_m = mean(GETTANF), 
GETWIC_15_m = mean(GETWIC), 
MAINEMPL_15_m = mean(MAINEMPL), 
NPOV100_15_m = mean(NPOV100), 
NPOV150_15_m = mean(NPOV150), 
NPOV200_15_m = mean(NPOV200), 
#NPOV50_15_m = mean(NPOV50), 
RESPEMPL_15_m = mean(RESPEMPL), 
BELONG_15_m = mean(BELONG), 
NUM60_15_m = mean(NUM60), 
NUMADULT_15_m = mean(NUMADULT), 
NUMKIDS_15_m = mean(NUMKIDS), 
NUMLIVHH_15_m = mean(NUMLIVHH), 
ORIGIN_15_m = mean(ORIGIN), 
OTHENG_15_m = mean(OTHENG), 
PARTICIP_15_m = mean(PARTICIP), 
FINDFRUT_15_m = mean(FINDFRUT), 
GROCERY_15_m = mean(GROCERY), 
USUALGOA_15_m = mean(USUALGOA), 
RENTOWN_15_m = mean(RENTOWN), 
BMI_15_m = mean(BMI), 
HOSPERA_15_m = mean(HOSPERA), 
INSUREDA_15_m = mean(INSUREDA),
n_15_a = n()) 


#head(philly_2015aa)


philly_2015b <- read.spss("HS15CHD1b.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
philly_2015b <- dplyr::select(philly_2015b, TRACT10, BMIPCT, CHIP, INSUREDC, EVRASTHC) %>% 
mutate(TRACT10 = TRACT10*100,
CHIP = case_when(CHIP == "Yes" ~ 0, 
CHIP == "No" ~ 1),
INSUREDC = case_when(INSUREDC == "Yes" ~ 0, 
INSUREDC == "No" ~ 1),
EVRASTHC = case_when(EVRASTHC == "Yes" ~ 0, 
EVRASTHC == "No" ~ 1)) %>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))


#str(philly_2015b, list.len=ncol(philly_2015b))
#philly_2015b %>% sapply(levels)


philly_2015bb <- as_tibble(philly_2015b) %>% 
group_by(TRACT10) %>% 
summarise(BMIPCT_15_m = mean(BMIPCT), 
CHIP_15_m = mean(CHIP), 
INSUREDC_15_m = mean(INSUREDC), 
EVRASTHC_15_m = mean(EVRASTHC),
n_15_b = n())





philly_2015c <- read.spss("HS1560S1b.sav", to.data.frame=TRUE) %>% filter(COUNTY == "Philadelphia")
philly_2015c <- dplyr::select(philly_2015c, TRACT10, NTRAN, DEPRESD) %>% 
mutate(TRACT10 = TRACT10*100,
NTRAN = case_when(NTRAN == "Yes" ~ 0, 
NTRAN == "No" ~ 1),
DEPRESD = case_when(DEPRESD == "Yes" ~ 0, 
DEPRESD == "No" ~ 1))%>% 
replace_with_na_all(condition = ~.x == c(99)) %>% 
replace_with_na_all(condition = ~.x == c(98)) %>% 
replace_with_na_all(condition = ~.x == c(97))

#str(philly_2015c, list.len=ncol(philly_2015c))
#philly_2015c %>% sapply(levels)

philly_2015cc <- as_tibble(philly_2015c) %>% 
group_by(TRACT10) %>% 
summarise(NTRAN_15_m = mean(NTRAN), 
DEPRESD_15_m = mean(DEPRESD),
n_15_c = n())

philly_2015ab  <- inner_join(philly_2015aa, philly_2015bb, by = "TRACT10")
philly_2015    <- inner_join(philly_2015ab, philly_2015cc, by = "TRACT10") %>% 
mutate(n = n_15_a + n_15_b + n_15_c) %>% 
dplyr::select(-n_15_a,-n_15_b,-n_15_c)


philly_supp   <- read_csv("supplemental_variables.csv")
str(philly_supp)



philly_le   <- read_csv("phl_life_expectancy.csv")
philly_xy   <- read_csv("centroids.csv")
philly_xy$GEOID <- as.numeric(as.character(philly_xy$GEOID))
philly_xy=philly_xy[complete.cases(philly_xy), ]

philly_tbl  <- inner_join(philly_2017, philly_2009, by = c("Geo_TRACT_2017" = "Geo_TRACT_2009"))
philly_tbl  <- left_join(philly_tbl, philly_le, by = c("Geo_FIPS_2017" = "tract_id"))
philly_tbl  <- left_join(philly_tbl, philly_xy, by = c("Geo_FIPS_2017" = "GEOID"))
philly_census <- philly_tbl

philly_tbl  <- left_join(philly_tbl, philly_2015, by = c("Geo_TRACT_2017" = "TRACT10"))
philly_tbl  <- left_join(philly_tbl, philly_2008, by = c("Geo_TRACT_2017" = "TRACT"))
philly_tbl  <- left_join(philly_tbl, philly_supp, by = c("Geo_FIPS_2017" = "Census_Tract"))

#str(philly_tbl)
#head(philly_tbl)


# Converting data frame to tibble and renaming tract variable

philly_tbl <- as_tibble(philly_tbl) %>%
mutate(tract = Geo_TRACT_2017) 

#str(philly_tbl)

philly <- philly_tbl # %>% dplyr::select(4:251)



# Wes, I commented out this part because we want to standardize later on after we created our change variables

# Standardizing entire data set

#z_philly <- philly %>%
#  psycho::standardize()

#z_philly_tbl <- z_philly %>%
#  cbind(philly_tbl$Geo_TRACT_2017)

#z_philly_tbl <- z_philly_tbl %>%
#  mutate(tract = philly_tbl$Geo_TRACT_2017) 

#?clean_names

#z_philly_tbl %>% clean_names(case = "old_janitor")

#View(z_philly_tbl)
#str(z_philly_tbl)



# Creating Factor Analysis variables for 2009 and 2017
# Reverse code variables so indicators all have positive relationship with life expectancy

philly2 <- mutate(philly,
Pct_black_2009 = (Population_Black_2009 / total_population_2009),
Pct_married_house_2009 = Households_Married_2009 / Households_2009,
Pct_single_2009 = ((Population_Never_Married_2009 + Population_Separated_2009 + Population_Widowed_2009 + Population_Divorced_2009) / Population_15_and_Over_2009),
Pct_HS_2009 = (Population_25_Years_and_Over_Diploma_2009 / Population_25_Years_and_Over_2009),
Pct_Bach_2009 = Population_25_Years_and_Bachelors_2009 / Population_25_Years_and_Over_2009,
Pct_enrolled_School_2009 = (Population_3_Years_and_Over_Enrolled_In_school_2009 / Population_3_Years_and_Over_2009),
Pct_unemployed_2009 = (Population_16_Years_and_Over_in_labor_force_civilian_unemployed_2009 / (Population_16_Years_and_Over_2009 - Population_16_Years_and_Over_not_in_labor_force_2009)),
Pct_Own_2009 = Owner_Occupied_Housing_Units_2009/ Occupied_Housing_2009,
Pct_vacant_2009 = (Vacant_Housing_Units_2009 / Housing_Units_2009),
Pct_fam_blw_pov_2009 = (Families_below_poverty_line_2009 / Families_2009),
Pct_car_access_2009 = ((Housing_units_No_Vehicle_Available_2009 + Renter_units_No_Vehicle_Available_2009) / (Occupied_Housing_Units_2009 + Renter_Housing_Units_2009)),
Stability_2009 = Same_house_year_ago_2009,
Pct_crowded_2009 = (Renter_Occupied_Housing_1.51_2_Occupants_per_room_2009 + Occupied_Housing_1.51_2_Occupants_per_room_2009 + Occupied_Housing_2_or_more_Occupants_per_room_2009 + Renter_Occupied_Housing_2_or_more_Occupants_per_room_2009)/(Renter_Occupied_Housing_2009 + Occupied_Housing_2009),
Pct_black_2017 = (Population_Black_2017 / Total_Population_2017),
Pct_married_house_2017 = Households_Married_2017 / Households_2017,
Pct_single_2017 = ((Population_Never_Married_2017 + Population_Separated_2017 + Population_Widowed_2017 + Population_Divorced_2017) / Population_15_and_Over_2017),
Pct_HS_2017 = (Population_25_Years_and_Over_Diploma_2017 / Population_25_Years_and_Over_2017),
Pct_Bach_2017 = Population_25_Years_and_Bachelors_2017 / Population_25_Years_and_Over_2017,
Pct_enrolled_School_2017 = (Population_3_Years_and_Over_Enrolled_In_school_2017 / Population_3_Years_and_Over_2017),
Pct_unemployed_2017 = (Population_16_Years_and_Over_in_labor_force_civilian_unemployed_2017 / (Population_16_Years_and_Over_2017 - Population_16_Years_and_Over_not_in_labor_force_2017)),
Pct_Own_2017 = Owner_Occupied_Housing_Units_2017/ Occupied_Housing_Units_2017,
Pct_vacant_2017 = (Vacant_Housing_Units_2017 / Housing_Units_2017),
Pct_fam_blw_pov_2017 = (Families_below_poverty_line_2017 / Families_2017),
Pct_car_access_2017 = ((Housing_units_No_Vehicle_Available_2017 + Renter_units_No_Vehicle_Available_2017) / (Occupied_Housing_Units_2017 + Renter_Housing_Units_2017)),
Stability_2017 = Same_house_year_ago_2017,
Pct_crowded_2017 = (Renter_Occupied_Housing_1.51_2_Occupants_per_room_2017 + Occupied_Housing_1.51_2_Occupants_per_room_2017 + Occupied_Housing_2_or_more_Occupants_per_room_2017 + Renter_Occupied_Housing_2_or_more_Occupants_per_room_2017)/(Renter_Occupied_Housing_2017 + Occupied_Housing_2017)
)



# Creating '09 -> '17 change variables

philly_change <- mutate(philly2,
Pct_black_09_17 = Pct_black_2017-Pct_black_2009,
Pct_married_house_09_17 = Pct_married_house_2017-Pct_married_house_2009,
Pct_single_09_17 = Pct_single_2017-Pct_single_2009,
Pct_HS_09_17 = Pct_HS_2017-Pct_HS_2009,
Pct_Bach_09_17 = Pct_Bach_2017-Pct_Bach_2009,
Pct_enrolled_School_09_17 = Pct_enrolled_School_2017-Pct_enrolled_School_2009,
Pct_unemployed_09_17 = Pct_unemployed_2017-Pct_unemployed_2009,
Pct_Own_09_17 = Pct_Own_2017-Pct_Own_2009,
Pct_vacant_09_17 = Pct_vacant_2017-Pct_vacant_2009,
Pct_fam_blw_pov_09_17 = Pct_fam_blw_pov_2017-Pct_fam_blw_pov_2009,
Pct_car_access_09_17 = Pct_car_access_2017-Pct_car_access_2009,
Stability_09_17 = Stability_2017-Stability_2009,
Median_Household_Income_09_17 = Median_Household_Income_2017-Median_Household_Income_2009,
Pct_crowded_09_17 = Pct_crowded_2017 - Pct_crowded_2009,
Gini_Index_09_17 = Gini_Index_2017 - Gini_Index_2009
)

# Standardizing all '09, '17, and change variables

philly_change_z <- philly_change %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("life_expectancy", "Pct_black_2017", "Pct_single_2017", "Pct_HS_2017", "Pct_Bach_2017", "Pct_unemployed_2017", "Pct_vacant_2017", "Pct_fam_blw_pov_2017", "Median_Household_Income_2017",  "Pct_Own_2017", "Stability_2017", "Pct_crowded_2017", "Pct_car_access_2017", "Gini_Index_2017", "Pct_black_2009", "Pct_single_2009", "Pct_HS_2009", "Pct_unemployed_2009", "Pct_vacant_2009", "Pct_fam_blw_pov_2009", "Median_Household_Income_2009",  "Pct_Own_2009", "Stability_2009", "Pct_crowded_2009", "Pct_car_access_2009", "Gini_Index_2009", "Pct_black_09_17", "Pct_single_09_17", "Pct_HS_09_17", "Pct_unemployed_09_17", "Pct_vacant_09_17", "Pct_fam_blw_pov_09_17", "Median_Household_Income_09_17",  "Pct_Own_09_17", "Stability_09_17", "Pct_crowded_09_17", "Pct_car_access_09_17", "Gini_Index_09_17", "GETSSDI_15_m", "GETSSI_15_m", "GETSTAMP_15_m", "GETTANF_15_m", "GETWIC_15_m", "MAINEMPL_15_m", "NPOV100_15_m", "NPOV150_15_m", "NPOV200_15_m", "RESPEMPL_15_m", "BELONG_15_m", "NTRAN_15_m", "NUM60_15_m", "NUMADULT_15_m", "NUMKIDS_15_m", "NUMLIVHH_15_m", "ORIGIN_15_m", "OTHENG_15_m", "PARTICIP_15_m", "FINDFRUT_15_m", "GROCERY_15_m", "USUALGOA_15_m", "RENTOWN_15_m", "BMI_15_m", "BMIPCT_15_m", "CHIP_15_m", "DEPRESD_15_m", "EVRASTHC_15_m", "HOSPERA_15_m", "INSUREDA_15_m", "INSUREDC_15_m", "GETSSDI_08_m", "GETSSI_08_m", "GETSTAMP_08_m", "GETTANF_08_m", "GETWIC_08_m", "MAINEMPL_08_m", "NPOV100_08_m", "NPOV150_08_m", "NPOV200_08_m", "RESPEMPL_08_m", "BELONG_08_m", "NUM60_08_m", "NUMADULT_08_m", "NUMKIDS_08_m", "NUMLIVHH_08_m", "ORIGIN_08_m", "OTHENG_08_m", "PARTICIP_08_m", "FINDFRUT_08_m", "GROCERY_08_m", "USUALGOA_08_m", "RENTOWN_08_m", "BMI_08_m", "HOSPERA_08_m", "INSUREDA_08_m", "NTRAN_08_m", "BMIPCT_08_m", "CHIP_08_m", "DEPRESS_08_m", "EVRASTHC_08_m", "job_density_13", "avg_job_growth_04_13", "Low_Response_Score", "Total_Crime_Index_2019", "pct_Sngl_Prns_HHD_ACSMOE_13_17", "pct_Crowd_Occp_U_ACS_13_17"))



#%>% psycho::standardize()



#  Removing all unnecessary variables, observations with missing data, and outputting analysis data sets for Factor Analysis

philly_analysis <- dplyr::select(philly_change_z, Geo_TRACT_2017, life_expectancy, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_crowded_2017, Pct_car_access_2017, Gini_Index_2017, Pct_black_2009, Pct_single_2009, Pct_HS_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Median_Household_Income_2009,  Pct_Own_2009, Stability_2009, Pct_crowded_2009, Pct_car_access_2009, Gini_Index_2009, Pct_black_09_17, Pct_single_09_17, Pct_HS_09_17, Pct_unemployed_09_17, Pct_vacant_09_17, Pct_fam_blw_pov_09_17, Median_Household_Income_09_17,  Pct_Own_09_17, Stability_09_17, Pct_crowded_09_17, Pct_car_access_09_17, Gini_Index_09_17, GETSSDI_15_m, GETSSI_15_m, GETSTAMP_15_m, GETTANF_15_m, GETWIC_15_m, MAINEMPL_15_m, NPOV100_15_m, NPOV150_15_m, NPOV200_15_m, RESPEMPL_15_m, BELONG_15_m, NTRAN_15_m, NUM60_15_m, NUMADULT_15_m, NUMKIDS_15_m, NUMLIVHH_15_m, ORIGIN_15_m, OTHENG_15_m, PARTICIP_15_m, FINDFRUT_15_m, GROCERY_15_m, USUALGOA_15_m, RENTOWN_15_m, BMI_15_m, BMIPCT_15_m, CHIP_15_m, DEPRESD_15_m, EVRASTHC_15_m, HOSPERA_15_m, INSUREDA_15_m, INSUREDC_15_m, GETSSDI_08_m, GETSSI_08_m, GETSTAMP_08_m, GETTANF_08_m, GETWIC_08_m, MAINEMPL_08_m, NPOV100_08_m, NPOV150_08_m, NPOV200_08_m, RESPEMPL_08_m, BELONG_08_m, NUM60_08_m, NUMADULT_08_m, NUMKIDS_08_m, NUMLIVHH_08_m, ORIGIN_08_m, OTHENG_08_m, PARTICIP_08_m, FINDFRUT_08_m, GROCERY_08_m, USUALGOA_08_m, RENTOWN_08_m, BMI_08_m, HOSPERA_08_m, INSUREDA_08_m, NTRAN_08_m, BMIPCT_08_m, CHIP_08_m, DEPRESS_08_m, EVRASTHC_08_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)
# philly_analysis=philly_analysis[complete.cases(philly_analysis),]


philly_analysis_ <- dplyr::select(philly_change, Geo_TRACT_2017, life_expectancy, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_crowded_2017, Pct_car_access_2017, Gini_Index_2017, Pct_black_2009, Pct_Bach_2009, Pct_single_2009, Pct_HS_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Median_Household_Income_2009,  Pct_Own_2009, Stability_2009, Pct_crowded_2009, Pct_car_access_2009, Gini_Index_2009, Pct_black_09_17, Pct_single_09_17, Pct_HS_09_17, Pct_unemployed_09_17, Pct_vacant_09_17, Pct_fam_blw_pov_09_17, Median_Household_Income_09_17,  Pct_Own_09_17, Stability_09_17, Pct_crowded_09_17, Pct_car_access_09_17, Gini_Index_09_17, GETSSDI_15_m, GETSSI_15_m, GETSTAMP_15_m, GETTANF_15_m, GETWIC_15_m, MAINEMPL_15_m, NPOV100_15_m, NPOV150_15_m, NPOV200_15_m, RESPEMPL_15_m, BELONG_15_m, NTRAN_15_m, NUM60_15_m, NUMADULT_15_m, NUMKIDS_15_m, NUMLIVHH_15_m, ORIGIN_15_m, OTHENG_15_m, PARTICIP_15_m, FINDFRUT_15_m, GROCERY_15_m, USUALGOA_15_m, RENTOWN_15_m, BMI_15_m, BMIPCT_15_m, CHIP_15_m, DEPRESD_15_m, EVRASTHC_15_m, HOSPERA_15_m, INSUREDA_15_m, INSUREDC_15_m, GETSSDI_08_m, GETSSI_08_m, GETSTAMP_08_m, GETTANF_08_m, GETWIC_08_m, MAINEMPL_08_m, NPOV100_08_m, NPOV150_08_m, NPOV200_08_m, RESPEMPL_08_m, BELONG_08_m, NUM60_08_m, NUMADULT_08_m, NUMKIDS_08_m, NUMLIVHH_08_m, ORIGIN_08_m, OTHENG_08_m, PARTICIP_08_m, FINDFRUT_08_m, GROCERY_08_m, USUALGOA_08_m, RENTOWN_08_m, BMI_08_m, HOSPERA_08_m, INSUREDA_08_m, NTRAN_08_m, BMIPCT_08_m, CHIP_08_m, DEPRESS_08_m, EVRASTHC_08_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)
# philly_analysis=philly_analysis[complete.cases(philly_analysis),]


philly_analysis2 <- dplyr::select(philly_change, life_expectancy, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_crowded_2017, Pct_car_access_2017, Gini_Index_2017, Pct_black_2009, Pct_single_2009, Pct_HS_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Median_Household_Income_2009,  Pct_Own_2009, Stability_2009, Pct_crowded_2009, Pct_car_access_2009, Gini_Index_2009, Pct_black_09_17, Pct_single_09_17, Pct_HS_09_17, Pct_unemployed_09_17, Pct_vacant_09_17, Pct_fam_blw_pov_09_17, Median_Household_Income_09_17,  Pct_Own_09_17, Stability_09_17, Pct_crowded_09_17, Pct_car_access_09_17, Gini_Index_09_17)


philly_analysis3 <- dplyr::select(philly_change, GETSSDI_15_m, GETSSI_15_m, GETSTAMP_15_m, GETTANF_15_m, GETWIC_15_m, MAINEMPL_15_m, NPOV100_15_m, NPOV150_15_m, NPOV200_15_m, RESPEMPL_15_m, BELONG_15_m, NTRAN_15_m, NUM60_15_m, NUMADULT_15_m, NUMKIDS_15_m, NUMLIVHH_15_m, ORIGIN_15_m, OTHENG_15_m, PARTICIP_15_m, FINDFRUT_15_m, GROCERY_15_m, USUALGOA_15_m, RENTOWN_15_m, BMI_15_m, BMIPCT_15_m, CHIP_15_m, DEPRESD_15_m, EVRASTHC_15_m, HOSPERA_15_m, INSUREDA_15_m, INSUREDC_15_m, GETSSDI_08_m, GETSSI_08_m, GETSTAMP_08_m, GETTANF_08_m, GETWIC_08_m, MAINEMPL_08_m, NPOV100_08_m, NPOV150_08_m, NPOV200_08_m, RESPEMPL_08_m, BELONG_08_m, NUM60_08_m, NUMADULT_08_m, NUMKIDS_08_m, NUMLIVHH_08_m, ORIGIN_08_m, OTHENG_08_m, PARTICIP_08_m, FINDFRUT_08_m, GROCERY_08_m, USUALGOA_08_m, RENTOWN_08_m, BMI_08_m, HOSPERA_08_m, INSUREDA_08_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)

philly_analysis3z <- dplyr::select(philly_change_z, GETSSDI_15_m, GETSSI_15_m, GETSTAMP_15_m, GETTANF_15_m, GETWIC_15_m, MAINEMPL_15_m, NPOV100_15_m, NPOV150_15_m, NPOV200_15_m, RESPEMPL_15_m, BELONG_15_m, NTRAN_15_m, NUM60_15_m, NUMADULT_15_m, NUMKIDS_15_m, NUMLIVHH_15_m, ORIGIN_15_m, OTHENG_15_m, PARTICIP_15_m, FINDFRUT_15_m, GROCERY_15_m, USUALGOA_15_m, RENTOWN_15_m, BMI_15_m, BMIPCT_15_m, CHIP_15_m, DEPRESD_15_m, EVRASTHC_15_m, HOSPERA_15_m, INSUREDA_15_m, INSUREDC_15_m, GETSSDI_08_m, GETSSI_08_m, GETSTAMP_08_m, GETTANF_08_m, GETWIC_08_m, MAINEMPL_08_m, NPOV100_08_m, NPOV150_08_m, NPOV200_08_m, RESPEMPL_08_m, BELONG_08_m, NUM60_08_m, NUMADULT_08_m, NUMKIDS_08_m, NUMLIVHH_08_m, ORIGIN_08_m, OTHENG_08_m, PARTICIP_08_m, FINDFRUT_08_m, GROCERY_08_m, USUALGOA_08_m, RENTOWN_08_m, BMI_08_m, HOSPERA_08_m, INSUREDA_08_m, NTRAN_08_m, BMIPCT_08_m, CHIP_08_m, DEPRESS_08_m, EVRASTHC_08_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)


philly_analysis4z_08 <- dplyr::select(philly_change_z, life_expectancy, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_crowded_2017, Pct_car_access_2017, Gini_Index_2017, GETSSDI_08_m, GETSSI_08_m, GETSTAMP_08_m, GETTANF_08_m, GETWIC_08_m, MAINEMPL_08_m, NPOV100_08_m, NPOV150_08_m, NPOV200_08_m, RESPEMPL_08_m, BELONG_08_m, NUM60_08_m, NUMADULT_08_m, NUMKIDS_08_m, NUMLIVHH_08_m, ORIGIN_08_m, OTHENG_08_m, PARTICIP_08_m, FINDFRUT_08_m, GROCERY_08_m, USUALGOA_08_m, RENTOWN_08_m, BMI_08_m, HOSPERA_08_m, INSUREDA_08_m, NTRAN_08_m, BMIPCT_08_m, CHIP_08_m, DEPRESS_08_m, EVRASTHC_08_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)

philly_analysis4z_15 <- dplyr::select(philly_change_z, life_expectancy, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_crowded_2017, Pct_car_access_2017, Gini_Index_2017, GETSSDI_15_m, GETSSI_15_m, GETSTAMP_15_m, GETTANF_15_m, GETWIC_15_m, MAINEMPL_15_m, NPOV100_15_m, NPOV150_15_m, NPOV200_15_m, RESPEMPL_15_m, BELONG_15_m, NTRAN_15_m, NUM60_15_m, NUMADULT_15_m, NUMKIDS_15_m, NUMLIVHH_15_m, ORIGIN_15_m, OTHENG_15_m, PARTICIP_15_m, FINDFRUT_15_m, GROCERY_15_m, USUALGOA_15_m, RENTOWN_15_m, BMI_15_m, BMIPCT_15_m, CHIP_15_m, DEPRESD_15_m, EVRASTHC_15_m, HOSPERA_15_m, INSUREDA_15_m, INSUREDC_15_m, job_density_13, avg_job_growth_04_13, Low_Response_Score, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)








#https://www.socialprogress.org/assets/downloads/resources/2018/2018-Social-Progress-Index-Methodology.pdf

#1. Double check missingness
#2. Fix the upper and lower bounds of Z-scores for each year based on theoretical or best/worst known values

# Convert factor score within specified range? Factor score = (Score - [Worst all scores])/ ([Best all scores]-[Worst all scores]) x 100

#If multiple dimensions then create index score by multiplying each score times (1/# of dimensions) and add together


# Min and Max values for each variable



library(summarytools)
descr(philly_analysis_)

#life expectancy: 64.4-86.10, 75.25
#Pct_black_2009 | Pct_black_2017: 0-100, 5
#Pct_single_2009 | Pct_single_2017: 0-96, 48
#Pct_HS_2009 | Pct_HS_2017: 40-99, 69.5
#Pct_unemployed_2009 | Pct_unemployed_2017: 0-45, 22.5
#Pct_vacant_2009 | Pct_vacant_2017: 0-41, 20.5
#Pct_fam_blw_pov_2009 | Pct_fam_blw_pov_2017: 0-72, 36
#Median_Household_Income_2009 | Median_Household_Income_2017: 11825-149211, 80518
#Pct_Own_2009 | Pct_Own_2017: 1-94, 47.5
#Stability_2009 | Stability_2017: 0-10139, 5068.5
#Pct_crowded_2009 | Pct_crowded_2017: 0-.03, .015
#Pct_car_access_2009 | Pct_car_access_2017: 0-78, 39
#Gini_Index_2009 | Gini_Index_2017: .27-.71, .49
#GETSSDI_15_m | GETSSDI_08_m : .29-1, .875
#GETSSI_15_m | GETSSI_08_m : .85-1, .925
#GETSTAMP_15_m | GETSTAMP_08_m : .50-1, .75
#GETTANF_15_m | GETTANF_08_m : .88-1, .94
#GETWIC_15_m | GETWIC_08_m : .74-1, .87
#MAINEMPL_15_m | MAINEMPL_08_m : bad
#NPOV100_15_m | NPOV100_08_m : bad
#NPOV150_15_m | NPOV150_08_m : bad
#NPOV200_15_m | NPOV200_08_m : bad
#NPOV50_15_m | NPOV50_08_m : bad
#RESPEMPL_15_m | RESPEMPL_08_m : bad
#BELONG_15_m | BELONG_08_m : 1.80-3.00, 2.4
#NTRAN_15_m | NTRAN_08_m : bad
#NUM60_15_m | NUM60_08_m : bad
#NUMADULT_15_m | NUMADULT_08_m : 1.37-2.42, 1.89
#NUMKIDS_15_m | NUMKIDS_08_m : bad
#NUMLIVHH_15_m | NUMLIVHH_08_m : 1.88-3.42, 5.3
#ORIGIN_15_m | ORIGIN_08_m : bad
#OTHENG_15_m | OTHENG_08_m : bad
#PARTICIP_15_m | PARTICIP_08_m : 0-.74, .37
#FINDFRUT_15_m | FINDFRUT_08_m : 1.67-3
#GROCERY_15_m | GROCERY_08_m : 1.11-3, 2.05
#USUALGOA_15_m | USUALGOA_08_m : bad
#RENTOWN_15_m | RENTOWN_08_m : .44-.85, .64
#BMI_15_m | BMI_08_m : 22.88-39.51, 31.195
#BMIPCT_15_m | BMIPCT_08_m : 0-100, 50
#CHIP_15_m | CHIP_08_m : 0-1, .91
#DEPRESD_15_m | DEPRESS_08_m : 0-1, .81
#EVRASTHC_15_m | EVRASTHC_08_m : 0-1, .79
#HOSPERA_15_m | HOSPERA_08_m : 0-2.75, .7
#INSUREDA_15_m | INSUREDA_08_m : bad
#INSUREDC_15_m | INSUREDC_08_m : bad


philly_analysis_c <- mutate(philly_analysis_, Geo_TRACT_2017,
life_expectancy_c = (life_expectancy-75.25)/1.51,
Pct_black_2009_c = (Pct_black_2009-.50)/.225, Pct_black_2017_c = (Pct_black_2017-.50)/.225,
Pct_Bach_2009_c = (Pct_Bach_2009-.50)/.225, Pct_Bach_2017_c = (Pct_Bach_2017-.50)/.225,
Pct_single_2009_c = (Pct_single_2009-.48)/.06, Pct_single_2017_c = (Pct_single_2017-.48)/.06,
Pct_HS_2009_c = (Pct_HS_2009-.695)/.19, Pct_HS_2017_c = (Pct_HS_2017-.695)/.19,
Pct_unemployed_2009_c = (Pct_unemployed_2009-.225)/.025, Pct_unemployed_2017_c = (Pct_unemployed_2017-.225)/.025,
Pct_vacant_2009_c = (Pct_vacant_2009-.205)/.015, Pct_vacant_2017_c = (Pct_vacant_2017-.205)/.015,
Pct_fam_blw_pov_2009_c = (Pct_fam_blw_pov_2009-.36)/.085, Pct_fam_blw_pov_2017_c = (Pct_fam_blw_pov_2017-.36)/.085,
Median_Household_Income_2009_c = (Median_Household_Income_2009-80518)/19604.375, Median_Household_Income_2017_c = (Median_Household_Income_2017-80518)/19604.375,
Pct_Own_2009_c = (Pct_Own_2009-.475)/.055, Pct_Own_2017_c = (Pct_Own_2017-.475)/.055,
Stability_2009_c = (Stability_2009-5068.5)/513, Stability_2017_c = (Stability_2017-5068.5)/513,
Pct_crowded_2009_c = (Pct_crowded_2009-.015)/.015, Pct_crowded_2017_c = (Pct_crowded_2017-.015)/.015,
Pct_car_access_2009_c = (Pct_car_access_2009-.39)/.135, Pct_car_access_2017_c = (Pct_car_access_2017-.39)/.135,
Gini_Index_2009_c = (Gini_Index_2009-.49)/.1, Gini_Index_2017_c = (Gini_Index_2017-.49)/.1,
GETSSI_15_m_c = (GETSSI_15_m-.845)/.13, GETSSI_08_m_c = (GETSSI_08_m-.845)/.13,
GETSTAMP_15_m_c = (GETSTAMP_15_m-.75)/.14, GETSTAMP_08_m_c = (GETSTAMP_08_m-.75)/.14,
NUMADULT_15_m_c = (NUMADULT_15_m-1.89)/.58, NUMADULT_08_m_c = (NUMADULT_08_m-1.89)/.58,
NUMLIVHH_15_m_c = (NUMLIVHH_15_m-2.65)/.525, NUMLIVHH_08_m_c = (NUMLIVHH_08_m-2.65)/.525,
PARTICIP_15_m_c = (PARTICIP_15_m-.37)/.225, PARTICIP_08_m_c = (PARTICIP_08_m-.37)/.225,
GROCERY_15_m_c = (GROCERY_15_m-2.05)/.35, GROCERY_08_m_c = (GROCERY_08_m-2.05)/.35,
FINDFRUT_15_m_c = (FINDFRUT_15_m-2.33)/.24, FINDFRUT_08_m_c = (FINDFRUT_08_m-2.33)/.24,
RENTOWN_15_m_c = (RENTOWN_15_m-.64)/.13, RENTOWN_08_m = (RENTOWN_08_m-.64)/.13,
BMI_15_m_c = (BMI_15_m-31.195)/2.41, BMI_08_m_c = (BMI_08_m-31.195)/2.41,
BMIPCT_15_m_c = (BMIPCT_15_m-50)/29.04, BMIPCT_08_m_c = (BMIPCT_08_m-50)/29.04,
BELONG_15_m_c = (BELONG_15_m-2.4)/.16, BELONG_08_m_c = (BELONG_08_m-2.4)/.16,
CHIP_15_m_c = (CHIP_15_m-.91)/.195, CHIP_08_m_c = (CHIP_08_m-.91)/.195,
EVRASTHC_15_m_c = (EVRASTHC_15_m-.79)/.26, EVRASTHC_08_m_c = (EVRASTHC_08_m-.79)/.26,
DEPRESD_15_m_c = (DEPRESD_15_m-.81)/.25, DEPRESS_08_m_c = (DEPRESS_08_m-.81)/.25,
HOSPERA_15_m_c = (HOSPERA_15_m-.7)/.52, HOSPERA_08_m_c = (HOSPERA_08_m-.7)/.52)



philly_analysis_cb <- mutate(philly2, Geo_TRACT_2017,
life_expectancy_c = (life_expectancy-75.25)/1.51,
Pct_black_2009_c = (Pct_black_2009-.50)/.225, Pct_black_2017_c = (Pct_black_2017-.50)/.225,
Pct_Bach_2009_c = (Pct_Bach_2009-.50)/.225, Pct_Bach_2017_c = (Pct_Bach_2017-.50)/.225,
Pct_single_2009_c = (Pct_single_2009-.48)/.06, Pct_single_2017_c = (Pct_single_2017-.48)/.06,
Pct_HS_2009_c = (Pct_HS_2009-.695)/.19, Pct_HS_2017_c = (Pct_HS_2017-.695)/.19,
Pct_unemployed_2009_c = (Pct_unemployed_2009-.225)/.025, Pct_unemployed_2017_c = (Pct_unemployed_2017-.225)/.025,
Pct_vacant_2009_c = (Pct_vacant_2009-.205)/.015, Pct_vacant_2017_c = (Pct_vacant_2017-.205)/.015,
Pct_fam_blw_pov_2009_c = (Pct_fam_blw_pov_2009-.36)/.085, Pct_fam_blw_pov_2017_c = (Pct_fam_blw_pov_2017-.36)/.085,
Median_Household_Income_2009_c = (Median_Household_Income_2009-80518)/19604.375, Median_Household_Income_2017_c = (Median_Household_Income_2017-80518)/19604.375,
Pct_Own_2009_c = (Pct_Own_2009-.475)/.055, Pct_Own_2017_c = (Pct_Own_2017-.475)/.055,
Stability_2009_c = (Stability_2009-5068.5)/513, Stability_2017_c = (Stability_2017-5068.5)/513,
Pct_crowded_2009_c = (Pct_crowded_2009-.015)/.015, Pct_crowded_2017_c = (Pct_crowded_2017-.015)/.015,
Pct_car_access_2009_c = (Pct_car_access_2009-.39)/.135, Pct_car_access_2017_c = (Pct_car_access_2017-.39)/.135,
Gini_Index_2009_c = (Gini_Index_2009-.49)/.1, Gini_Index_2017_c = (Gini_Index_2017-.49)/.1)









# Visualize distributions


library(purrr)
library(tidyr)
library(ggplot2)

philly_analysis_ %>%
keep(is.numeric) %>% 
gather() %>% 
ggplot(aes(value)) +
facet_wrap(~ key, scales = "free") +
geom_histogram()

# Looks good. Since the 'Change' data set has a normal distribution after standardizing. Will use Maximum Likelihood as estimator for factor analysis.






# Correlation Matrix



# How do variables correlate with life expectancy?

cor.test(~ Pct_black_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_single_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_HS_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_unemployed_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_vacant_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_fam_blw_pov_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_car_access_2017+ life_expectancy, philly_analysis)
cor.test(~ Median_Household_Income_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_Own_2017+ life_expectancy, philly_analysis)
cor.test(~ Stability_2017+ life_expectancy, philly_analysis)
cor.test(~ Pct_crowded_2017 + life_expectancy, philly_analysis)
cor.test(~ Gini_Index_2017 + life_expectancy, philly_analysis)


# Correlation matrix of change scores

cor(philly_analysis_09_17)




#Stability and Pct_crowded did not have a statistically significant relationship with life expectancy. #Therefore, they do not move on to the next stage of developing the index measure.




# Subsetting data sets for Factor Analysis with variables only statistically significantly (p < .05) correlated with life expectancy. Reverse (Invert) coding negative associations so all standardized variables contribute positively to index.




philly_final_09_17 <- dplyr::select(philly_analysis_09_17, Pct_black_09_17, Pct_single_09_17, Pct_HS_09_17, Pct_unemployed_09_17, Pct_vacant_09_17, Pct_fam_blw_pov_09_17, Pct_car_access_09_17, Median_Household_Income_09_17, Pct_Own_09_17, Gini_Index_09_17)%>% 
mutate(Pct_black_09_17 = 1 - Pct_black_09_17,
Pct_single_09_17 = 1- Pct_single_09_17,
Pct_unemployed_09_17 = 1 - Pct_unemployed_09_17,
Pct_vacant_09_17 = 1 - Pct_vacant_09_17,
Pct_fam_blw_pov_09_17 = 1 - Pct_fam_blw_pov_09_17,
Pct_car_access_09_17 = 1 - Pct_car_access_09_17,
Gini_Index_09_17 = 1 - Gini_Index_09_17
)



philly_final_2017 <- dplyr::select(philly_analysis, Pct_black_2017, Pct_single_2017, Pct_HS_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Pct_car_access_2017, Median_Household_Income_2017, Pct_Own_2017, Gini_Index_2017) %>% 
mutate(Pct_black_2017 = 1 - Pct_black_2017,
Pct_single_2017 = 1- Pct_single_2017,
Pct_unemployed_2017 = 1 - Pct_unemployed_2017,
Pct_vacant_2017 = 1 - Pct_vacant_2017,
Pct_fam_blw_pov_2017 = 1 - Pct_fam_blw_pov_2017,
Pct_car_access_2017 = 1 - Pct_car_access_2017,
Gini_Index_2017 = 1 - Gini_Index_2017
)



philly_final_2009 <- dplyr::select(philly_analysis, Pct_black_2009, Pct_single_2009, Pct_HS_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Pct_car_access_2009, Median_Household_Income_2009, Pct_Own_2009, Gini_Index_2009)%>% 
mutate(Pct_black_2009 = 1 - Pct_black_2009,
Pct_single_2009 = 1- Pct_single_2009,
Pct_unemployed_2009 = 1 - Pct_unemployed_2009,
Pct_vacant_2009 = 1 - Pct_vacant_2009,
Pct_fam_blw_pov_2009 = 1 - Pct_fam_blw_pov_2009,
Pct_car_access_2009 = 1 - Pct_car_access_2009,
Gini_Index_2009 = 1 - Gini_Index_2009
)










# Exploratory Factor Analysis



philly_analysis4z_15 <- dplyr::select(philly_change_z, life_expectancy, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_car_access_2017, Gini_Index_2017, NUMLIVHH_15_m, PARTICIP_15_m, GROCERY_15_m, BMI_15_m, FINDFRUT_15_m, PARTICIP_15_m, MAINEMPL_15_m, NPOV200_15_m, BELONG_15_m, NTRAN_15_m, ORIGIN_15_m, FINDFRUT_15_m, DEPRESD_15_m, EVRASTHC_15_m, INSUREDA_15_m, job_density_13, avg_job_growth_04_13, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17)

FCI_EFA_15_full <- factanal(na.omit(philly_analysis4z_15), 1)
FCI_EFA_15_full


philly_analysis4z_15 <- dplyr::select(philly_change_z, life_expectancy, Pct_Bach_2017, Median_Household_Income_2017,  Pct_Own_2017, NUMLIVHH_15_m, PARTICIP_15_m, GROCERY_15_m, FINDFRUT_15_m, PARTICIP_15_m, MAINEMPL_15_m, NPOV200_15_m, BELONG_15_m, ORIGIN_15_m, FINDFRUT_15_m, INSUREDA_15_m, job_density_13)
cronbach.alpha(na.omit(philly_analysis4z_15), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
# Alpha - .865 NTRAN_15_m

philly_analysis4z_15 <- dplyr::select(philly_change_z, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Median_Household_Income_2017,  Pct_Own_2017, Stability_2017, Pct_car_access_2017, Gini_Index_2017, NUMLIVHH_15_m, PARTICIP_15_m, GROCERY_15_m, BMI_15_m, FINDFRUT_15_m, PARTICIP_15_m)
# MAINEMPL_15_m_c NPOV200_15_m_c BELONG_15_m_c NTRAN_15_m_c ORIGIN_15_m_c FINDFRUT_15_m_c DEPRESD_15_m_c EVRASTHC_15_m_c INSUREDA_15_m_c job_density_13, avg_job_growth_04_13, Total_Crime_Index_2019, pct_Sngl_Prns_HHD_ACSMOE_13_17, pct_Crowd_Occp_U_ACS_13_17

library(psych)

FCI_EFA_15_1 <- factanal(na.omit(philly_analysis4z_15), 1)
FCI_EFA_15_1


philly_analysis4z_15 <- dplyr::select(philly_change_z, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Pct_car_access_2017, Gini_Index_2017, BMI_15_m)
cronbach.alpha(na.omit(philly_analysis4z_15), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
# Alpha - .775

philly_analysis4z_15 <- dplyr::select(philly_change_z, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Pct_car_access_2017, Gini_Index_2017)
cronbach.alpha(na.omit(philly_analysis4z_15), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
# Alpha - .826


philly_analysis4a_15 <- dplyr::select(philly_change_z, Pct_Bach_2017, Median_Household_Income_2017, NPOV200_15_m, Pct_Own_2017, BELONG_15_m, FINDFRUT_15_m, PARTICIP_15_m, GROCERY_15_m, MAINEMPL_15_m, NTRAN_15_m, INSUREDA_15_m)
cronbach.alpha(na.omit(philly_analysis4a_15), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
#Alpha is .793









philly_analysis4z_08 <- dplyr::select(philly_analysis_c, Pct_Bach_2009_c, Pct_unemployed_2009_c, Pct_vacant_2009_c, Pct_fam_blw_pov_2009_c, Median_Household_Income_2009_c,  Pct_Own_2009_c, Stability_2009_c, Pct_car_access_2009_c, Gini_Index_2009_c, NUMLIVHH_08_m_c, PARTICIP_08_m_c, GROCERY_08_m_c, BMI_08_m_c, FINDFRUT_08_m_c, PARTICIP_08_m_c)

philly_analysis4z_08 <- dplyr::select(philly_change_z, Pct_Bach_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Median_Household_Income_2009,  Pct_Own_2009, Stability_2009, Pct_car_access_2009, Gini_Index_2009, NUMLIVHH_08_m, PARTICIP_08_m, GROCERY_08_m, BMI_08_m, FINDFRUT_08_m, PARTICIP_08_m)
# MAINEMPL_08_m_c NPOV200_08_m_c BELONG_08_m_c NTRAN_08_m_c ORIGIN_08_m_c FINDFRUT_08_m_c DEPRESS_08_m_c EVRASTHC_08_m_c INSUREDA_08_m_c HOSPERA_08_m_c

#str(na.omit(philly_analysis4z_08))

FCI_EFA_08_1 <- factanal(na.omit(philly_analysis4z_08), 1)
FCI_EFA_08_1

philly_analysis4a_08 <- dplyr::select(philly_change_z, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Pct_car_access_2009, Gini_Index_2009, BMI_08_m)
cronbach.alpha(na.omit(philly_analysis4a_08), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
#Alpha is .821
philly_analysis4a_08 <- dplyr::select(philly_change_z, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Pct_car_access_2009, Gini_Index_2009)
cronbach.alpha(na.omit(philly_analysis4a_08), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
#Alpha is .849



philly_analysis4b_08 <- dplyr::select(philly_change_z, Pct_Bach_2009, Median_Household_Income_2009, Pct_Own_2009, PARTICIP_08_m, GROCERY_08_m, FINDFRUT_08_m)
cronbach.alpha(na.omit(philly_analysis4b), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
#^^^These varibles negatively loaded in factor analysis

philly_analysis4a_15 <- dplyr::select(philly_change_z, Pct_Bach_2017, Median_Household_Income_2017, NPOV200_08_m, Pct_Own_2009, BELONG_08_m, FINDFRUT_08_m, PARTICIP_08_m, GROCERY_08_m, MAINEMPL_08_m, NTRAN_08_m, INSUREDA_08_m)
cronbach.alpha(na.omit(philly_analysis4a_15), CI = TRUE, probs = c(0.025, 0.975), B = 1000)
#Alpha is .793









finalds <- dplyr::select(philly_analysis_c, Geo_TRACT_2017, Pct_unemployed_2017_c, Pct_vacant_2017_c, Pct_fam_blw_pov_2017_c, Pct_car_access_2017_c, Gini_Index_2017_c, BMI_15_m_c, Pct_unemployed_2009_c, Pct_vacant_2009_c, Pct_fam_blw_pov_2009_c, Pct_car_access_2009_c, Gini_Index_2009_c, BMI_08_m_c) %>% 
mutate(FCI_13_17 = Pct_unemployed_2017_c + Pct_vacant_2017_c + Pct_fam_blw_pov_2017_c + Pct_car_access_2017_c + Gini_Index_2017_c + BMI_15_m_c, FCI_05_09 = Pct_unemployed_2009_c + Pct_vacant_2009_c + Pct_fam_blw_pov_2009_c + Pct_car_access_2009_c + Gini_Index_2009_c + BMI_08_m_c, FCI = FCI_13_17 - FCI_05_09)
finalds_  <- dplyr::select(finalds, Geo_TRACT_2017, BMI_15_m_c, BMI_08_m_c, FCI, FCI_13_17, FCI_05_09)

#write_csv(finalds, 'C:/Users/Rsnea/Documents/Wes Contract/PHL_FCI/finalds.csv')

finalds_cens <- dplyr::select(philly_analysis_c, Geo_TRACT_2017, Pct_unemployed_2017_c, Pct_vacant_2017_c, Pct_fam_blw_pov_2017_c, Pct_car_access_2017_c, Gini_Index_2017_c, Pct_unemployed_2009_c, Pct_vacant_2009_c, Pct_fam_blw_pov_2009_c, Pct_car_access_2009_c, Gini_Index_2009_c) %>% 
mutate(FCI_13_17_cens = Pct_unemployed_2017_c + Pct_vacant_2017_c + Pct_fam_blw_pov_2017_c + Pct_car_access_2017_c + Gini_Index_2017_c, FCI_05_09_cens = Pct_unemployed_2009_c + Pct_vacant_2009_c + Pct_fam_blw_pov_2009_c + Pct_car_access_2009_c + Gini_Index_2009_c, FCI_cens = FCI_13_17_cens - FCI_05_09_cens)

#write_csv(finalds, 'C:/Users/Rsnea/Documents/Wes Contract/PHL_FCI/finalds.csv')

final1  <- full_join(finalds_cens, finalds_, by = c("Geo_TRACT_2017"))



finalds_17 <- dplyr::select(philly_change_z, Geo_TRACT_2017, life_expectancy, Pct_Bach_2017, Median_Household_Income_2017,  Pct_Own_2017, NUMLIVHH_15_m, PARTICIP_15_m, GROCERY_15_m, FINDFRUT_15_m, PARTICIP_15_m, MAINEMPL_15_m, NPOV200_15_m, BELONG_15_m, ORIGIN_15_m, FINDFRUT_15_m, INSUREDA_15_m, job_density_13) %>% 
mutate(FCI_17 = life_expectancy + Pct_Bach_2017 + Median_Household_Income_2017 + Pct_Own_2017 + NUMLIVHH_15_m + PARTICIP_15_m + GROCERY_15_m + FINDFRUT_15_m + PARTICIP_15_m + MAINEMPL_15_m + NPOV200_15_m + BELONG_15_m + ORIGIN_15_m + FINDFRUT_15_m + INSUREDA_15_m + job_density_13)

#write_csv(finalds, 'C:/Users/Rsnea/Documents/Wes Contract/PHL_FCI/finalds.csv')

FCI_Index  <- left_join(final1, finalds_17, by = c("Geo_TRACT_2017"))

write_csv(FCI_Index, 'C:/Users/Rsnea/Documents/Wes Contract/PHL_FCI/FCI_Index.csv')





# Weighted Quantile Sums

FCI_Matrix_09 <- dplyr::select(philly_change_z, life_expectancy, Pct_single_2009, Pct_Bach_2009, Pct_unemployed_2009, Pct_vacant_2009, Pct_fam_blw_pov_2009, Pct_car_access_2009, Gini_Index_2009, Median_Household_Income_2009)

FCI_Matrix_09 <- data.matrix(na.omit(FCI_Matrix_09))

x <- FCI_Matrix_09[,2:9]
y <- FCI_Matrix_09[,'life_expectancy']

FCI_WQS_09 <- wqs.est(y, x, B = 100)
print(FCI_WQS_09[["weights"]])
print(FCI_WQS_09[["fit"]])
print(FCI_WQS_09[["WQS"]])


# 2017

FCI_Matrix_17 <- dplyr::select(philly_change_z, life_expectancy, Pct_single_2017, Pct_Bach_2017, Pct_unemployed_2017, Pct_vacant_2017, Pct_fam_blw_pov_2017, Pct_car_access_2017, Gini_Index_2017, Median_Household_Income_2017)

FCI_Matrix_17 <- data.matrix(na.omit(FCI_Matrix_17))

x <- FCI_Matrix_17[,2:9]
y <- FCI_Matrix_17[,'life_expectancy']

FCI_WQS_17 <- wqs.est(y, x, B = 100)
print(FCI_WQS_17[["weights"]])
print(FCI_WQS_17[["fit"]])
print(FCI_WQS_17[["WQS"]])







