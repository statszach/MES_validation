######################
## Loading Packages ##
######################

library(tidyverse)
library(readxl)
library(lavaan)
library(mirt)
library(psych)

####################
## Importing Data ##
####################

MoneyExpenditure_Complete <- read_excel("~/Consulting/2020-McNamara/MES Validation/MoneyExpenditure_Complete.xlsx", na = "9999")

###############
## Tidy Data ##
###############

##First, let's rename the variables to be a bit easier to work with.

Tidy1 <- MoneyExpenditure_Complete %>% rename(.,SocialEvents = `Social events (such as gifts for friends or significant others, hanging out)`,
                                                 Family = `Family (such as child care, pets, other family members)`,
                                                 Tech = `Technology (such as computer, camera, phone, video games)`,
                                                 Entertainment = `Entertainment (such as concert, travel, Greek life)`,
                                                 Drugs = `Alcohol, Tobacco (such as cigarettes, e-cigs, snuff), and recreational drugs`,
                                                 Coffee = `Coffee and Energy drinks`,
                                                 Fashion = `Fashion (such as clothing, handbags, shoes, tattoos)`,
                                                 HealthPromotion1 = `Health Promotion (such as gym membership, nutrient supplements, camping)`,
                                                 HealthPromotion2 = `Health Promotion (such as doctor or dentist bills or co-pays, medications)`,
                                                 PersonalCare = `Personal Care (such as make up, shaving supplies, nail polish, toiletries)`,
                                                 Housing = `Housing (such as rent, utilities)`,
                                                 Transportation = `Transportation (such as parking, tickets, car or bike repairs, gas, subway or bus fare)`,
                                                 CredCard = `Credit Card bills and Loans`,
                                                 School = `School (such as books, tuition, fees, supplies)`,
                                                 Charity = `Charity (such as donations to needy, church)`)

## Now, filter for only participants who actually ran out of food.

Tidy2 <- Tidy1 %>% filter(`Since you have been in college, have you ever run out of money and were unable to buy food?` == 1)

# Making a list of the MES items to use in later analyses

MES_items <- c("SocialEvents", "Family", "Tech", "Entertainment", "Drugs", "Coffee", "Fashion", "HealthPromotion1",
               "HealthPromotion2", "PersonalCare", "Housing", "Transportation", "CredCard", "School", "Charity")

####################
## Transform Data ##
####################

MissingDiagnosis <- Tidy2[MES_items]

table(is.na(MissingDiagnosis))

# FALSE  TRUE 
# 10932   768 

#(768 / (768 + 10932)) * 100 = 6.56%

# Going to want to impute missing data ##

describe(MissingDiagnosis)

#                   vars n  mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 768 2.54 1.11      3    2.50 1.48   1   5     4  0.23    -0.76 0.04
# Family              2 765 2.41 1.14      2    2.34 1.48   1   5     4  0.30    -0.82 0.04
# Tech                3 763 1.94 1.13      2    1.76 1.48   1   5     4  1.00     0.04 0.04
# Entertainment       4 765 2.08 1.15      2    1.94 1.48   1   5     4  0.69    -0.62 0.04
# Drugs               5 764 2.02 1.18      2    1.86 1.48   1   5     4  0.84    -0.47 0.04
# Coffee              6 761 2.27 1.24      2    2.15 1.48   1   5     4  0.55    -0.84 0.04
# Fashion             7 762 2.06 1.12      2    1.90 1.48   1   5     4  0.85    -0.09 0.04
# HealthPromotion1    8 225 2.46 1.33      2    2.33 1.48   1   5     4  0.45    -0.96 0.09
# HealthPromotion2    9 766 2.47 1.17      3    2.41 1.48   1   5     4  0.21    -0.91 0.04
# PersonalCare       10 766 2.79 1.16      3    2.77 1.48   1   5     4  0.04    -0.75 0.04
# Housing            11 763 3.30 1.39      4    3.37 1.48   1   5     4 -0.43    -1.05 0.05
# Transportation     12 766 2.93 1.29      3    2.91 1.48   1   5     4 -0.09    -1.08 0.05
# CredCard           13 766 2.42 1.39      2    2.28 1.48   1   5     4  0.45    -1.17 0.05
# School             14 766 3.41 1.18      4    3.50 1.48   1   5     4 -0.44    -0.58 0.04
# Charity            15 766 1.63 0.90      1    1.47 0.00   1   5     4  1.42     1.57 0.03
