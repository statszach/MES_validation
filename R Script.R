######################
## Loading Packages ##
######################

library(tidyverse)
library(readxl)
library(lavaan)
library(mirt)
library(psych)
library(rsample)
library(gtsummary)

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
# Health promotion 1 was dropped from the analyses since it was not asked at all universities. See email from JM on 24 Mar 20

MES_items <- c("SocialEvents", "Family", "Tech", "Entertainment", "Drugs", "Coffee", "Fashion", 
               "HealthPromotion2", "PersonalCare", "Housing", "Transportation", "CredCard", "School", "Charity",
               "University")

####################
## Transform Data ##
####################

MissingDiagnosis <- Tidy2[MES_items]

table(is.na(MissingDiagnosis))

# FALSE  TRUE 
# 10707   213 

#(213 / (213 + 10707)) * 100 = 1.95%

## Complete Case!##

Tidy3 <- drop_na(MissingDiagnosis)

##################
## Descriptives ##
##################

describe(Tidy3)

#                   vars n  mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 754 2.54 1.10      3    2.50 1.48   1   5     4  0.22    -0.76 0.04
# Family              2 754 2.40 1.14      2    2.33 1.48   1   5     4  0.29    -0.82 0.04
# Tech                3 754 1.93 1.12      2    1.75 1.48   1   5     4  1.00     0.06 0.04
# Entertainment       4 754 2.07 1.14      2    1.93 1.48   1   5     4  0.71    -0.60 0.04
# Drugs               5 754 2.00 1.17      2    1.84 1.48   1   5     4  0.85    -0.46 0.04
# Coffee              6 754 2.26 1.23      2    2.14 1.48   1   5     4  0.55    -0.84 0.04
# Fashion             7 754 2.05 1.11      2    1.89 1.48   1   5     4  0.86    -0.09 0.04
# HealthPromotion2    8 754 2.47 1.17      3    2.40 1.48   1   5     4  0.22    -0.90 0.04
# PersonalCare        9 754 2.78 1.15      3    2.76 1.48   1   5     4  0.03    -0.75 0.04
# Housing            10 754 3.29 1.39      4    3.36 1.48   1   5     4 -0.42    -1.06 0.05
# Transportation     11 754 2.94 1.29      3    2.92 1.48   1   5     4 -0.10    -1.09 0.05
# CredCard           12 754 2.41 1.39      2    2.27 1.48   1   5     4  0.46    -1.17 0.05
# School             13 754 3.41 1.18      4    3.50 1.48   1   5     4 -0.45    -0.56 0.04
# Charity            14 754 1.61 0.89      1    1.45 0.00   1   5     4  1.48     1.81 0.03

table(Tidy3$University)

# 1   2   3   4 
# 49 111 538  56 


items <- c("SocialEvents"    ,
"Family"          ,
"Tech"            ,
"Entertainment"   ,
"Drugs"           ,
"Coffee"          ,
"Fashion"         ,
"HealthPromotion2",
"PersonalCare"    ,
"Housing"         ,
"Transportation"  ,
"CredCard"        ,
"School"          ,
"Charity"         )

############
## MANOVA ##
############

manova1 <- manova(cbind(SocialEvents    ,
                        Family          ,
                        Tech            ,
                        Entertainment   ,
                        Drugs           ,
                        Coffee          ,
                        Fashion         ,
                        HealthPromotion2,
                        PersonalCare    ,
                        Housing         ,
                        Transportation  ,
                        CredCard        ,
                        School          ,
                        Charity         ) ~ University, data = Tidy3)

summary.aov(manova1)

describeBy(Tidy3, group = "University")

# Sig different, cannot claim similar

# Descriptive statistics by group 
# University: 1
# vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 49 2.53 1.10      3    2.51 1.48   1   5     4  0.06    -1.04 0.16
# Family              2 49 2.18 1.15      2    2.10 1.48   1   5     4  0.45    -0.99 0.16
# Tech                3 49 1.84 1.05      1    1.71 0.00   1   5     4  0.96    -0.01 0.15
# Entertainment       4 49 2.02 0.97      2    1.93 1.48   1   5     4  0.77     0.21 0.14
# Drugs               5 49 2.08 1.38      1    1.93 0.00   1   5     4  0.78    -0.95 0.20
# Coffee              6 49 2.16 1.26      2    2.00 1.48   1   5     4  0.79    -0.39 0.18
# Fashion             7 49 2.16 1.14      2    2.07 1.48   1   5     4  0.59    -0.83 0.16
# HealthPromotion2    8 49 1.96 1.21      1    1.80 0.00   1   5     4  0.91    -0.36 0.17
# PersonalCare        9 49 2.90 1.19      3    2.88 1.48   1   5     4  0.12    -0.75 0.17
# Housing            10 49 2.59 1.55      3    2.51 2.97   1   5     4  0.22    -1.56 0.22
# Transportation     11 49 3.31 1.26      3    3.37 1.48   1   5     4 -0.39    -0.87 0.18
# CredCard           12 49 2.18 1.48      1    2.02 0.00   1   5     4  0.78    -0.94 0.21
# School             13 49 3.47 1.31      4    3.56 1.48   1   5     4 -0.56    -0.81 0.19
# Charity            14 49 1.45 0.77      1    1.32 0.00   1   5     4  2.35     7.30 0.11
# --------------------------------------------------------------------- 
#   University: 2
# vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 111 3.64 1.01      4    3.73 1.48   1   5     4 -0.67     0.25 0.10
# Family              2 111 3.24 1.08      3    3.27 1.48   1   5     4 -0.23    -0.46 0.10
# Tech                3 111 3.37 1.14      3    3.40 1.48   1   5     4 -0.16    -0.76 0.11
# Entertainment       4 111 3.28 1.10      3    3.31 1.48   1   5     4 -0.36    -0.58 0.10
# Drugs               5 111 2.76 1.21      3    2.74 1.48   1   5     4 -0.05    -1.00 0.11
# Coffee              6 111 3.03 1.30      3    3.03 1.48   1   5     4 -0.17    -1.09 0.12
# Fashion             7 111 3.14 1.20      3    3.18 1.48   1   5     4 -0.06    -0.88 0.11
# HealthPromotion2    8 111 2.80 1.13      3    2.75 1.48   1   5     4  0.24    -0.53 0.11
# PersonalCare        9 111 3.50 1.04      3    3.54 1.48   1   5     4 -0.27    -0.47 0.10
# Housing            10 111 2.91 1.28      3    2.89 1.48   1   5     4  0.01    -0.98 0.12
# Transportation     11 111 2.98 1.19      3    2.98 1.48   1   5     4  0.03    -0.90 0.11
# CredCard           12 111 2.76 1.36      3    2.70 1.48   1   5     4  0.20    -1.18 0.13
# School             13 111 3.77 1.00      4    3.85 1.48   1   5     4 -0.36    -0.73 0.09
# Charity            14 111 2.50 1.12      3    2.44 1.48   1   5     4  0.22    -0.69 0.11
# --------------------------------------------------------------------- 
#   University: 3
# vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 538 2.33 0.98      2    2.28 1.48   1   5     4  0.23    -0.77 0.04
# Family              2 538 2.23 1.04      2    2.14 1.48   1   5     4  0.32    -0.85 0.05
# Tech                3 538 1.66 0.88      1    1.53 0.00   1   5     4  1.11     0.25 0.04
# Entertainment       4 538 1.83 1.00      1    1.69 0.00   1   5     4  0.89    -0.32 0.04
# Drugs               5 538 1.89 1.10      1    1.71 0.00   1   5     4  1.02    -0.01 0.05
# Coffee              6 538 2.12 1.15      2    2.00 1.48   1   5     4  0.64    -0.72 0.05
# Fashion             7 538 1.83 0.96      2    1.70 1.48   1   5     4  1.00     0.37 0.04
# HealthPromotion2    8 538 2.50 1.12      3    2.47 1.48   1   5     4  0.08    -0.96 0.05
# PersonalCare        9 538 2.62 1.11      3    2.61 1.48   1   5     4  0.06    -0.80 0.05
# Housing            10 538 3.48 1.33      4    3.60 1.48   1   5     4 -0.64    -0.72 0.06
# Transportation     11 538 2.85 1.30      3    2.81 1.48   1   5     4 -0.05    -1.16 0.06
# CredCard           12 538 2.33 1.38      2    2.19 1.48   1   5     4  0.52    -1.14 0.06
# School             13 538 3.32 1.18      3    3.40 1.48   1   5     4 -0.40    -0.58 0.05
# Charity            14 538 1.44 0.73      1    1.28 0.00   1   5     4  1.70     2.80 0.03
# --------------------------------------------------------------------- 
#   University: 4
# vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 56 2.43 1.09      2    2.37 1.48   1   5     4  0.34    -0.67 0.15
# Family              2 56 2.62 1.32      3    2.54 1.48   1   5     4  0.23    -1.18 0.18
# Tech                3 56 1.75 1.08      1    1.57 0.00   1   5     4  1.17     0.21 0.14
# Entertainment       4 56 1.96 1.17      1    1.80 0.00   1   5     4  0.79    -0.51 0.16
# Drugs               5 56 1.52 0.91      1    1.35 0.00   1   4     3  1.49     0.85 0.12
# Coffee              6 56 2.18 1.32      2    2.02 1.48   1   5     4  0.70    -0.81 0.18
# Fashion             7 56 1.89 0.98      2    1.78 1.48   1   5     4  0.88     0.16 0.13
# HealthPromotion2    8 56 1.91 1.35      1    1.67 0.00   1   5     4  1.16    -0.07 0.18
# PersonalCare        9 56 2.71 1.20      3    2.65 1.48   1   5     4  0.12    -0.76 0.16
# Housing            10 56 2.82 1.61      3    2.78 2.97   1   5     4  0.16    -1.56 0.21
# Transportation     11 56 3.34 1.25      4    3.41 1.48   1   5     4 -0.48    -0.77 0.17
# CredCard           12 56 2.71 1.42      3    2.65 1.48   1   5     4  0.17    -1.30 0.19
# School             13 56 3.50 1.35      4    3.61 1.48   1   5     4 -0.55    -0.91 0.18
# Charity            14 56 1.55 0.85      1    1.41 0.00   1   5     4  1.66     2.97 0.11


Tidy4 <- Tidy3 %>% filter(University != 2)

manova2<- manova(cbind(SocialEvents    ,
                       Family          ,
                       Tech            ,
                       Entertainment   ,
                       Drugs           ,
                       Coffee          ,
                       Fashion         ,
                       HealthPromotion2,
                       PersonalCare    ,
                       Housing         ,
                       Transportation  ,
                       CredCard        ,
                       School          ,
                       Charity         ) ~ University, data = Tidy4)

summary(manova2)
summary.aov(manova2)

## Still different ##
## Hmmm ##

## Decided to use just university 3 for a random split, then fit the models into the other three samples afterwards. ##

#########################
## DATA TRANSFORMATION ##
#########################

## Selecting Just Uni 3

Uni3 <- Tidy3 %>% filter(University == 3) %>% select(-University, -MES_scored)

## Splitting into random parts

Uni3_Split <- initial_split(Uni3)

EFA_Sample <- testing(Uni3_Split)
IRT_Sample <- training(Uni3_Split)

#########
## EFA ##
#########

VSS(EFA_Sample, plot = F, fm = "wls")

# Very Simple Structure
# Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
#           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
# VSS complexity 1 achieves a maximimum of 0.69  with  2  factors
# VSS complexity 2 achieves a maximimum of 0.81  with  5  factors
# 
# The Velicer MAP achieves a minimum of 0.02  with  2  factors 

fa(EFA_Sample, nfactors = 2, fm = "wls", rotate = "Promax")
#Drop Personal Care and Charity (loadings below < |.40|)

EFA_Sample2 <- EFA_Sample %>% select(-PersonalCare, -Charity)

fa(EFA_Sample2, nfactors = 2, fm = "wls", rotate = "Promax")
#Retain remaining items

# Factor Analysis using method =  wls
# Call: fa(r = EFA_Sample2, nfactors = 2, rotate = "Promax", fm = "wls")
# Standardized loadings (pattern matrix) based upon correlation matrix
# WLS1  WLS2   h2   u2 com
# SocialEvents      0.75 -0.20 0.54 0.46 1.1
# Family           -0.13  0.48 0.23 0.77 1.1
# Tech              0.52  0.13 0.31 0.69 1.1
# Entertainment     0.76  0.00 0.57 0.43 1.0
# Drugs             0.67 -0.13 0.43 0.57 1.1
# Coffee            0.58  0.09 0.36 0.64 1.0
# Fashion           0.64  0.18 0.48 0.52 1.2
# HealthPromotion2 -0.05  0.56 0.30 0.70 1.0
# Housing          -0.15  0.70 0.48 0.52 1.1
# Transportation    0.13  0.50 0.29 0.71 1.1
# CredCard          0.06  0.72 0.54 0.46 1.0
# School            0.04  0.56 0.33 0.67 1.0
# 
# WLS1 WLS2
# SS loadings           2.65 2.23
# Proportion Var        0.22 0.19
# Cumulative Var        0.22 0.41
# Proportion Explained  0.54 0.46
# Cumulative Proportion 0.54 1.00
# 
# With factor correlations of 
# WLS1 WLS2
# WLS1 1.00 0.19
# WLS2 0.19 1.00
# 
# Mean item complexity =  1.1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  66  and the objective function was  3.57 with Chi Square of  458.13
# The degrees of freedom for the model are 43  and the objective function was  0.48 
# 
# The root mean square of the residuals (RMSR) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# The harmonic number of observations is  134 with the empirical chi square  44.11  with prob <  0.42 
# The total number of observations was  134  with Likelihood Chi Square =  60.39  with prob <  0.041 
# 
# Tucker Lewis Index of factoring reliability =  0.931
# RMSEA index =  0.06  and the 90 % confidence intervals are  0.012 0.086
# BIC =  -150.21
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# WLS1 WLS2
# Correlation of (regression) scores with factors   0.92 0.89
# Multiple R square of scores with factors          0.84 0.80
# Minimum correlation of possible factor scores     0.68 0.59

#######################
## CFA in other Unis ##
#######################

Uni1 <- Tidy3 %>% filter(University == 1) 
Uni2 <- Tidy3 %>% filter(University == 2) 
Uni4 <- Tidy3 %>% filter(University == 4) 

CFA_Model <- 'f1 =~ SocialEvents + Tech + Entertainment + Drugs + Coffee + Fashion
              f2 =~ Family + HealthPromotion2 + Housing + Transportation + CredCard + School
              f1 ~~ f2'

Uni1CFA <- cfa(model = CFA_Model, data = Uni1)

summary(Uni1CFA, fit.measures = T)

Uni2CFA <- cfa(model = CFA_Model, data = Uni2)

summary(Uni2CFA, fit.measures = T)

Uni3CFA <- cfa(model = CFA_Model, data = IRT_Sample)

summary(Uni3CFA, fit.measures = T)

Uni4CFA <- cfa(model = CFA_Model, data = Uni4)

summary(Uni4CFA, fit.measures = T)

#########
## IRT ##
#########

Uni1IRT <- cfa(model = CFA_Model, data = Uni1,  std.lv=TRUE, 
               ordered =c("SocialEvents"    ,
                          "Family"          ,
                          "Tech"            ,
                          "Entertainment"   ,
                          "Drugs"           ,
                          "Coffee"          ,
                          "Fashion"         ,
                          "HealthPromotion2",
                          "Housing"         ,
                          "Transportation"  ,
                          "CredCard"        ,
                          "School"          ),
               parameterization = "theta")

summary(Uni1IRT, fit.measures = T)

Uni2IRT <- cfa(model = CFA_Model, data = Uni2,  std.lv=TRUE, 
               ordered =c("SocialEvents"    ,
                          "Family"          ,
                          "Tech"            ,
                          "Entertainment"   ,
                          "Drugs"           ,
                          "Coffee"          ,
                          "Fashion"         ,
                          "HealthPromotion2",
                          "Housing"         ,
                          "Transportation"  ,
                          "CredCard"        ,
                          "School"          ),
               parameterization = "theta")

summary(Uni2IRT, fit.measures = T)



Uni3IRT <- cfa(model = CFA_Model, data = IRT_Sample,  std.lv=TRUE, 
               ordered =c("SocialEvents"    ,
                          "Family"          ,
                          "Tech"            ,
                          "Entertainment"   ,
                          "Drugs"           ,
                          "Coffee"          ,
                          "Fashion"         ,
                          "HealthPromotion2",
                          "Housing"         ,
                          "Transportation"  ,
                          "CredCard"        ,
                          "School"          ),
               parameterization = "theta")

summary(Uni3IRT, fit.measures = T)

Uni4IRT <- cfa(model = CFA_Model, data = Uni4,  std.lv=TRUE, 
               ordered =c("SocialEvents"    ,
                          "Family"          ,
                          "Tech"            ,
                          "Entertainment"   ,
                          "Drugs"           ,
                          "Coffee"          ,
                          "Fashion"         ,
                          "HealthPromotion2",
                          "Housing"         ,
                          "Transportation"  ,
                          "CredCard"        ,
                          "School"          ),
               parameterization = "theta")

summary(Uni4IRT, fit.measures = T)


IRT_Sample <- IRT_Sample %>% select(-PersonalCare, -Charity)

IRT_model <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion
              F2 = Family, HealthPromotion2, Housing, Transportation, CredCard, School
              COV = F1*F2'

mirt_model <- mirt.model(IRT_model, itemnames = IRT_Sample)

Uni3IRT_mirt <- mirt(IRT_Sample, IRT_model, itemtype = "graded")

M2(Uni3IRT_mirt)

coef(Uni3IRT_mirt)


IRT_model_f1 <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion'
IRT_model_f2 <- 'F2 = Family, HealthPromotion2, Housing, Transportation, CredCard, School'

mirt_model_f1 <- mirt.model(IRT_model_f1, itemnames = IRT_Sample)

Uni3IRT_mirt_f1 <- mirt(IRT_Sample, IRT_model_f1, itemtype = "graded")

M2(Uni3IRT_mirt_f1)

coef(Uni3IRT_mirt_f1)

mirt_model_f2 <- mirt.model(IRT_model_f2, itemnames = IRT_Sample)

Uni3IRT_mirt_f2 <- mirt(IRT_Sample, IRT_model_f2, itemtype = "graded")

M2(Uni3IRT_mirt_f2)

coef(Uni3IRT_mirt_f2)

####

Uni1 <- Uni1 %>% select(-University)

mirt_model_f1 <- mirt.model(IRT_model_f1, itemnames = Uni1)

Uni1IRT_mirt_f1 <- mirt(Uni1, IRT_model_f1, itemtype = "graded")

M2(Uni1IRT_mirt_f1)

coef(Uni1IRT_mirt_f1)

mirt_model_f2 <- mirt.model(IRT_model_f2, itemnames = Uni1)

Uni1IRT_mirt_f2 <- mirt(Uni1, IRT_model_f2, itemtype = "graded")

M2(Uni1IRT_mirt_f2)

coef(Uni1IRT_mirt_f2)

###

Uni2 <- Uni2 %>% select(-University)

mirt_model_f1 <- mirt.model(IRT_model_f1, itemnames = Uni2)

Uni2IRT_mirt_f1 <- mirt(Uni2, IRT_model_f1, itemtype = "graded")

M2(Uni2IRT_mirt_f1)

coef(Uni2IRT_mirt_f1)

mirt_model_f2 <- mirt.model(IRT_model_f2, itemnames = Uni2)

Uni2IRT_mirt_f2 <- mirt(Uni2, IRT_model_f2, itemtype = "graded")

M2(Uni2IRT_mirt_f2)

coef(Uni2IRT_mirt_f2)

###

Uni4 <- Uni4 %>% select(-University)
Uni4F1 <- Uni4 %>% select(SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion)
Uni4F2 <- Uni4 %>% select(Family, HealthPromotion2, Housing, Transportation, CredCard, School)

mirt_model_f1 <- mirt.model(IRT_model_f1, itemnames = Uni4F1)

Uni4IRT_mirt_f1 <- mirt(Uni4F1, IRT_model_f1, itemtype = "graded")

M2(Uni4IRT_mirt_f1, type = "C2")

coef(Uni4IRT_mirt_f1, simplify = T, IRTParam = T)

mirt_model_f2 <- mirt.model(IRT_model_f2, itemnames = Uni4F2)

Uni4IRT_mirt_f2 <- mirt(Uni4F2, IRT_model_f2, itemtype = "graded")

M2(Uni4IRT_mirt_f2, type = "C2")

coef(Uni4IRT_mirt_f2, simplify = T, IRTParam = T)

###

AllUniF1 <- Tidy4 %>% select(SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion)
AllUniF2 <- Tidy4 %>% select(Family, HealthPromotion2, Housing, Transportation, CredCard, School)


mirt_model_f1 <- mirt.model(IRT_model_f1, itemnames = AllUniF1)
mirt_model_f2 <- mirt.model(IRT_model_f2, itemnames = AllUniF2)

mirt_allunif1 <- mirt(AllUniF1, IRT_model_f1, itemtype = "graded")

M2(mirt_allunif1, type = "C2")

coef(mirt_allunif2, simplify = T)

mirt_allunif2 <- mirt(AllUniF2, IRT_model_f2, itemtype = "graded")

M2(mirt_allunif2, type = "C2")

coef(mirt_allunif2, simplify = T)


####

AllUniF1DIF <- Tidy4 %>% select(SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion, University)
IRT_model_f1 <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion'
IRT_model_f2 <- 'F2 = Family, HealthPromotion2, Housing, Transportation, CredCard, School'

groupf1 <- as.factor(AllUniF1DIF$University)
F1DIF <- multipleGroup(AllUniF1DIF, IRT_model_f1, groupf1)