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

## Rename validation variables

Tidy3 <- Tidy2 %>% rename(., WorriedRunOut = `I worried whether my food would run out before I got money to buy more.`,
                          DidntLast = `The food that I bought just didn't last, and I didn't have money to get more.`)

## Recode validation variables

Transform1 <- Tidy3 %>% mutate(RunOut2Cat = dplyr::if_else(WorriedRunOut >=4, 1, 0),
                               DidntLast2Cat = dplyr::if_else(DidntLast >=4, 1, 0))


## After discussing with JM, decided to do EFA/IRT on Uni 3, and then use Uni 2 as an independent
## sample. 

## Filtering by Unis

Uni3Only <- Transform1 %>% filter(University == 3)
Uni2Only <- Transform1 %>% filter(University == 2)



# Making a list of the MES items to use in later analyses
# Health promotion 1 was dropped from the analyses since it was not asked at all universities. See email from JM on 24 Mar 20

MES_items <- c("SocialEvents", "Family", "Tech", "Entertainment", "Drugs", "Coffee", "Fashion", 
               "HealthPromotion2", "PersonalCare", "Housing", "Transportation", "CredCard", "School", "Charity",
               "University", "RunOut2Cat", "DidntLast2Cat")

####################
## Transform Data ##
####################

MissingDiagnosisUni3 <- Uni3Only[MES_items]

table(is.na(MissingDiagnosisUni3))

# FALSE  TRUE 
# 9208   108 

#(108 / (108 + 9208)) * 100 = 1.16%

MissingDiagnosisUni2 <- Uni2Only[MES_items]

table(is.na(MissingDiagnosisUni2))

# FALSE  TRUE 
# 2049   93 

#(93 / (93 + 2049)) * 100 = 4.34%

## Complete Cases##

Uni3CC <- drop_na(MissingDiagnosisUni3)
Uni2CC <- drop_na(MissingDiagnosisUni2)

##################
## Descriptives ##
##################

describe(Uni3CC)

#                  vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
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
# University         15 538 3.00 0.00      3    3.00 0.00   3   3     0   NaN      NaN 0.00
# RunOut2Cat         16 538 0.33 0.47      0    0.29 0.00   0   1     1  0.72    -1.49 0.02
# DidntLast2Cat      17 538 0.24 0.43      0    0.18 0.00   0   1     1  1.20    -0.55 0.02

describe(Uni2CC)


#                  vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# SocialEvents        1 110 3.63 1.00    4.0    3.72 1.48   1   5     4 -0.67     0.27 0.10
# Family              2 110 3.24 1.08    3.0    3.26 1.48   1   5     4 -0.22    -0.47 0.10
# Tech                3 110 3.36 1.14    3.0    3.40 1.48   1   5     4 -0.15    -0.78 0.11
# Entertainment       4 110 3.28 1.10    3.0    3.32 1.48   1   5     4 -0.36    -0.60 0.11
# Drugs               5 110 2.74 1.19    3.0    2.73 1.48   1   5     4 -0.06    -1.01 0.11
# Coffee              6 110 3.05 1.29    3.0    3.06 1.48   1   5     4 -0.18    -1.07 0.12
# Fashion             7 110 3.15 1.21    3.0    3.19 1.48   1   5     4 -0.08    -0.87 0.11
# HealthPromotion2    8 110 2.82 1.13    3.0    2.77 1.48   1   5     4  0.24    -0.51 0.11
# PersonalCare        9 110 3.51 1.04    3.5    3.56 0.74   1   5     4 -0.29    -0.42 0.10
# Housing            10 110 2.90 1.28    3.0    2.88 1.48   1   5     4  0.03    -0.98 0.12
# Transportation     11 110 2.97 1.19    3.0    2.97 1.48   1   5     4  0.05    -0.90 0.11
# CredCard           12 110 2.77 1.35    3.0    2.72 1.48   1   5     4  0.19    -1.18 0.13
# School             13 110 3.77 1.00    4.0    3.85 1.48   1   5     4 -0.35    -0.75 0.10
# Charity            14 110 2.51 1.11    3.0    2.45 1.48   1   5     4  0.21    -0.67 0.11
# University         15 110 2.00 0.00    2.0    2.00 0.00   2   2     0   NaN      NaN 0.00
# RunOut2Cat         16 110 0.47 0.50    0.0    0.47 0.00   0   1     1  0.11    -2.01 0.05
# DidntLast2Cat      17 110 0.42 0.50    0.0    0.40 0.00   0   1     1  0.33    -1.91 0.05

#########################
## DATA TRANSFORMATION ##
#########################

## Selecting Just Uni 3

Uni3 <- Uni3CC %>% select(-University, -RunOut2Cat, -DidntLast2Cat)

## Splitting into random parts
set.seed(77)
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
# VSS complexity 1 achieves a maximimum of 0.62  with  2  factors
# VSS complexity 2 achieves a maximimum of 0.71  with  7  factors
# 
# The Velicer MAP achieves a minimum of 0.03  with  2  factors 
# BIC achieves a minimum of  -212.32  with  2  factors

fa(EFA_Sample, nfactors = 2, fm = "wls", rotate = "varimax")

# Factor Analysis using method =  wls
# Call: fa(r = EFA_Sample, nfactors = 2, rotate = "Promax", fm = "wls")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                   WLS1  WLS2    h2   u2 com
# SocialEvents      0.71 -0.29 0.505 0.50 1.3
# Family           -0.01  0.43 0.181 0.82 1.0
# Tech              0.43  0.11 0.214 0.79 1.1
# Entertainment     0.74 -0.03 0.546 0.45 1.0
# Drugs             0.65 -0.11 0.409 0.59 1.1
# Coffee            0.45  0.02 0.206 0.79 1.0
# Fashion           0.58  0.01 0.342 0.66 1.0
# HealthPromotion2 -0.06  0.43 0.183 0.82 1.0
# PersonalCare      0.38  0.20 0.211 0.79 1.5
# Housing          -0.16  0.76 0.562 0.44 1.1
# Transportation    0.13  0.41 0.201 0.80 1.2
# CredCard         -0.04  0.53 0.270 0.73 1.0
# School           -0.07  0.69 0.466 0.53 1.0
# Charity           0.23  0.14 0.085 0.91 1.7

#Drop Personal Care and Charity (loadings below < |.40|)

EFA_Sample2 <- EFA_Sample %>% select(-PersonalCare, -Charity)

fa(EFA_Sample2, nfactors = 2, fm = "wls", rotate = "Promax")

# Factor Analysis using method =  wls
# Call: fa(r = EFA_Sample2, nfactors = 2, rotate = "Promax", fm = "wls")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                   WLS1  WLS2   h2   u2 com
# SocialEvents      0.69 -0.23 0.51 0.49 1.2
# Family            0.00  0.41 0.17 0.83 1.0
# Tech              0.43  0.13 0.21 0.79 1.2
# Entertainment     0.73  0.02 0.54 0.46 1.0
# Drugs             0.69 -0.05 0.47 0.53 1.0
# Coffee            0.44  0.05 0.20 0.80 1.0
# Fashion           0.53  0.02 0.29 0.71 1.0
# HealthPromotion2 -0.09  0.39 0.15 0.85 1.1
# Housing          -0.11  0.76 0.58 0.42 1.0
# Transportation    0.16  0.43 0.22 0.78 1.3
# CredCard         -0.02  0.52 0.27 0.73 1.0
# School           -0.03  0.70 0.49 0.51 1.0

# Drop HealthPromotion2, loading < |.40|

EFA_Sample3 <- EFA_Sample2 %>% select(-HealthPromotion2)

fa(EFA_Sample3, nfactors = 2, fm = "wls", rotate = "Promax")

# Factor Analysis using method =  wls
# Call: fa(r = EFA_Sample3, nfactors = 2, rotate = "Promax", fm = "wls")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                 WLS1  WLS2    h2   u2 com
# SocialEvents    0.69 -0.24 0.473 0.53 1.2
# Family          0.00  0.39 0.155 0.85 1.0
# Tech            0.43  0.11 0.217 0.78 1.1
# Entertainment   0.75 -0.04 0.554 0.45 1.0
# Drugs           0.64 -0.08 0.401 0.60 1.0
# Coffee          0.45  0.01 0.206 0.79 1.0
# Fashion         0.59 -0.02 0.342 0.66 1.0
# PersonalCare    0.39  0.16 0.196 0.80 1.3
# Housing        -0.16  0.81 0.636 0.36 1.1
# Transportation  0.13  0.42 0.216 0.78 1.2
# CredCard       -0.04  0.53 0.278 0.72 1.0
# School         -0.07  0.69 0.467 0.53 1.0
# Charity         0.24  0.09 0.074 0.93 1.3

# Drop Family, loading < |.40|

EFA_Sample4 <- EFA_Sample3 %>% select(-Family)

f <- fa(EFA_Sample4, nfactors = 2, fm = "wls", rotate = "Promax")

## Retaing Remaining Items

# Factor Analysis using method =  wls
# Call: fa(r = EFA_Sample4, nfactors = 2, rotate = "Promax", fm = "wls")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                 WLS1  WLS2   h2   u2 com
# SocialEvents    0.68 -0.20 0.48 0.52 1.2
# Tech            0.43  0.13 0.21 0.79 1.2
# Entertainment   0.74 -0.02 0.54 0.46 1.0
# Drugs           0.68 -0.03 0.46 0.54 1.0
# Coffee          0.45  0.06 0.21 0.79 1.0
# Fashion         0.54 -0.01 0.29 0.71 1.0
# Housing        -0.13  0.81 0.65 0.35 1.1
# Transportation  0.15  0.42 0.21 0.79 1.3
# CredCard       -0.03  0.52 0.27 0.73 1.0
# School         -0.04  0.70 0.48 0.52 1.0
# 
# WLS1 WLS2
# SS loadings           2.17 1.63
# Proportion Var        0.22 0.16
# Cumulative Var        0.22 0.38
# Proportion Explained  0.57 0.43
# Cumulative Proportion 0.57 1.00
# 
# With factor correlations of 
# WLS1 WLS2
# WLS1  1.0  0.1
# WLS2  0.1  1.0
# 
# Mean item complexity =  1.1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  2.27 with Chi Square of  292.1
# The degrees of freedom for the model are 26  and the objective function was  0.23 
# 
# The root mean square of the residuals (RMSR) is  0.04 
# The df corrected root mean square of the residuals is  0.05 
# 
# The harmonic number of observations is  134 with the empirical chi square  20.65  with prob <  0.76 
# The total number of observations was  134  with Likelihood Chi Square =  28.85  with prob <  0.32 
# 
# Tucker Lewis Index of factoring reliability =  0.98
# RMSEA index =  0.034  and the 90 % confidence intervals are  0 0.076
# BIC =  -98.49
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# WLS1 WLS2
# Correlation of (regression) scores with factors   0.89 0.88
# Multiple R square of scores with factors          0.79 0.78
# Minimum correlation of possible factor scores     0.59 0.56

## Checking if factor scores are correlated

fscores <- factor.scores(EFA_Sample4, f, method = "Thurstone")

fscores2 <- as.data.frame(fscores$scores)

cor.test(fscores2$WLS1, fscores2$WLS2)

# Pearson's product-moment correlation
# 
# data:  fscores2$WLS1 and fscores2$WLS2
# t = 1.1172, df = 132, p-value = 0.2659
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.07401786  0.26207228
# sample estimates:
#        cor 
# 0.09678545 

#########
## IRT ##
#########

## Uni 3 Sample ##

IRT_Sample <- IRT_Sample %>% select(-PersonalCare, -Charity, -Family, -HealthPromotion2)

IRT_model <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion      
              F2 = Housing, Transportation, CredCard, School
              COV = F1*F2'

mirt_model <- mirt.model(IRT_model, itemnames = IRT_Sample)

Uni3IRT_mirt <- mirt(IRT_Sample, IRT_model, itemtype = "graded")

M2(Uni3IRT_mirt)

#             M2 df         p      RMSEA RMSEA_5   RMSEA_95     SRMSR       TLI       CFI
# stats 6.970547  4 0.1374529 0.04292751       0 0.09466609 0.0788873 0.9805367 0.9948098

coef(Uni3IRT_mirt, simplify = T, IRTParam = T)

# $items
#                   a1    a2     d1     d2     d3     d4
# SocialEvents   2.070 0.000  1.854 -0.611 -3.133 -6.330
# Tech           1.311 0.000 -0.388 -1.822 -3.699 -6.806
# Entertainment  2.340 0.000 -0.105 -1.925 -4.246 -7.675
# Drugs          1.612 0.000 -0.119 -1.510 -2.945 -4.826
# Coffee         1.158 0.000  0.399 -0.672 -2.070 -4.263
# Fashion        1.582 0.000  0.116 -1.707 -3.626 -5.729
# Housing        0.000 1.779  2.707  1.940  0.676 -1.467
# Transportation 0.000 1.519  1.835  0.729 -0.698 -2.880
# CredCard       0.000 1.485  0.522 -0.343 -1.361 -3.105
# School         0.000 1.834  3.045  1.823 -0.125 -2.327
# 
# $means
# F1 F2 
# 0  0 
# 
# $cov
# F1 F2
# F1 1.000 NA
# F2 0.103  1

## Uni 2 Sample ##

IRT_Sample2 <- Uni2CC %>% select(-PersonalCare, -Charity, -Family, -HealthPromotion2, -University)

IRT_model <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion      
              F2 = Housing, Transportation, CredCard, School
              COV = F1*F2'

mirt_model2 <- mirt.model(IRT_model, itemnames = IRT_Sample2)

Uni2IRT_mirt <- mirt(IRT_Sample2, mirt_model2, itemtype = "graded")

M2(Uni2IRT_mirt)

#             M2 df            p     RMSEA    RMSEA_5  RMSEA_95     SRMSR      TLI       CFI
# stats 68.43522 25 6.553452e-06 0.1262519 0.09057863 0.1617659 0.1280741 0.570689 0.7018674

## Bad fit -- let's try modeling separately! ##

IRT_Uni3_F1 <- IRT_Sample %>% select(SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion)

Uni3_F1_model <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion'

Uni3_F1_mirt_model <- mirt.model(Uni3_F1_model, itemnames = IRT_Uni3_F1)

Uni3_F1 <- mirt(IRT_Uni3_F1, Uni3_F1_mirt_model, itemtype = "graded")

M2(Uni3_F1, type = "C2")

#             M2 df            p      RMSEA    RMSEA_5 RMSEA_95      SRMSR       TLI       CFI
# stats 32.70004  9 0.0001505976 0.08083525 0.05211155 0.111369 0.05002262 0.9548928 0.9729357

coef(Uni3_F1, simplify=T, IRTParam = T)

# $items
#                  a1     d1     d2     d3     d4
# SocialEvents  2.105  1.875 -0.617 -3.163 -6.384
# Tech          1.293 -0.386 -1.813 -3.681 -6.780
# Entertainment 2.346 -0.105 -1.927 -4.251 -7.672
# Drugs         1.634 -0.120 -1.520 -2.963 -4.851
# Coffee        1.143  0.398 -0.668 -2.061 -4.247
# Fashion       1.559  0.117 -1.694 -3.604 -5.694

IRT_Uni3_F2 <- IRT_Sample %>% select(Housing, Transportation, CredCard, School)

Uni3_F2_model <- 'F2 = Housing, Transportation, CredCard, School'

Uni3_F2_mirt_model <- mirt.model(Uni3_F2_model, itemnames = IRT_Uni3_F2)

Uni3_F2 <- mirt(IRT_Uni3_F2, Uni3_F2_mirt_model, itemtype = "graded")

M2(Uni3_F2, type = "C2")

#            M2 df        p RMSEA RMSEA_5   RMSEA_95    SRMSR      TLI CFI
# stats 1.02628  2 0.598613     0       0 0.08109317 0.044167 1.008858   1

coef(Uni3_F2, simplify = T, IRTParam = T)

# $items
#                   a1    d1     d2     d3     d4
# Housing        1.820 2.740  1.964  0.685 -1.485
# Transportation 1.507 1.829  0.728 -0.693 -2.871
# CredCard       1.458 0.520 -0.338 -1.349 -3.081
# School         1.842 3.051  1.828 -0.123 -2.332

IRT_Uni2_F1 <- Uni2CC %>% select(SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion)

Uni2_F1_model <- 'F1 = SocialEvents, Tech, Entertainment, Drugs, Coffee, Fashion'

Uni2_F1_mirt_model <- mirt.model(Uni2_F1_model, itemnames = IRT_Uni2_F1)

Uni2_F1 <- mirt(IRT_Uni2_F1, Uni2_F1_mirt_model, itemtype = "graded")

M2(Uni2_F1, type = "C2")

#             M2 df          p      RMSEA RMSEA_5  RMSEA_95      SRMSR       TLI       CFI
# stats 16.80047  9 0.05193391 0.08917144       0 0.1537994 0.09067469 0.9083203 0.9449922

coef(Uni2_F1, simplify = T, IRTParam = T)
# $items
#                  a1    d1    d2     d3     d4
# SocialEvents  2.878 5.425 3.911  0.952 -2.952
# Tech          2.051 4.198 2.058 -0.239 -2.154
# Entertainment 2.518 4.294 2.200  0.063 -3.593
# Drugs         0.939 1.469 0.591 -1.041 -3.241
# Coffee        0.474 1.649 0.787 -0.342 -1.901
# Fashion       0.744 2.370 0.982 -0.488 -1.765

IRT_Uni2_F2 <- Uni2CC %>% select(Housing, Transportation, CredCard, School)

Uni2_F2_model <- 'F2 = Housing, Transportation, CredCard, School'

Uni2_F2_mirt_model <- mirt.model(Uni2_F2_model, itemnames = IRT_Uni2_F2)

Uni2_F2 <- mirt(IRT_Uni2_F2, Uni2_F2_mirt_model, itemtype = "graded")

M2(Uni2_F2, type = "C2")

#             M2 df          p     RMSEA RMSEA_5  RMSEA_95      SRMSR       TLI       CFI
# stats 4.938819  2 0.08463481 0.1161069       0 0.2488121 0.07823532 0.8931167 0.9643722

coef(Uni2_F2, simplify = T, IRTParam = T)
# $items
#                   a1    d1    d2     d3     d4
# Housing        2.593 2.667 1.149 -1.369 -3.316
# Transportation 1.552 2.660 0.700 -0.957 -2.764
# CredCard       2.589 2.253 0.373 -1.308 -3.333
# School         0.559 4.816 2.193  0.469 -1.001

#################
## Reliability ##
#################

marginal_rxx(Uni3_F1)
# 0.78653

marginal_rxx(Uni3_F2)
# 0.7504776

marginal_rxx(Uni2_F1)
# 0.8311664

marginal_rxx(Uni2_F2)
# 0.7986639

Uni3F1Omega <- omega(IRT_Uni3_F1, plot = F, fm = "wls")
# Omega 
# Call: omega(m = IRT_Uni3_F1, fm = "wls", plot = F)
# Alpha:                 0.78 
# G.6:                   0.76 
# Omega Hierarchical:    0.64 
# Omega H asymptotic:    0.76 
# Omega Total            0.85 

Uni3F2Omega <- omega(IRT_Uni3_F2, plot = F, fm = "wls")

# Omega 
# Call: omega(m = IRT_Uni3_F2, fm = "wls", plot = F)
# Alpha:                 0.7 
# G.6:                   0.64 
# Omega Hierarchical:    0.61 
# Omega H asymptotic:    0.74 
# Omega Total            0.83 

Uni2F1Omega <- omega(IRT_Uni2_F1, plot = F, fm = "wls")
# Omega 
# Call: omega(m = IRT_Uni2_F1, fm = "wls", plot = F)
# Alpha:                 0.69 
# G.6:                   0.69 
# Omega Hierarchical:    0.34 
# Omega H asymptotic:    0.42 
# Omega Total            0.82 

Uni2F2Omega <- omega(IRT_Uni2_F2, plot = F, fm = "wls")

# Omega 
# Call: omega(m = IRT_Uni2_F2, fm = "wls", plot = F)
# Alpha:                 0.67 
# G.6:                   0.64 
# Omega Hierarchical:    0.59 
# Omega H asymptotic:    0.69 
# Omega Total            0.85 

##############
## Validity ##
##############

#First, score scales

Uni3_Validity <- Uni3CC %>% mutate(MES_F1 = SocialEvents + Tech + Entertainment + Drugs + Coffee + Fashion,
                                   MES_F2 = Housing + Transportation + CredCard + School)

describe(Uni3_Validity$MES_F1)
#    vars   n  mean   sd median trimmed  mad min max range skew kurtosis   se
# X1    1 538 11.66 4.18     11   11.34 4.45   6  26    20 0.59    -0.37 0.18
describe(Uni3_Validity$MES_F2)
#    vars   n  mean   sd median trimmed  mad min max range skew kurtosis   se
# X1    1 538 11.98 3.77     12   11.95 4.45   4  20    16 0.06    -0.51 0.16

Uni2_Validity <- Uni2CC %>% mutate(MES_F1 = SocialEvents + Tech + Entertainment + Drugs + Coffee + Fashion,
                                   MES_F2 = Housing + Transportation + CredCard + School)

#Now, build LRs

F1U3_Validity_RO <- glm(RunOut2Cat ~ MES_F1, data = Uni3_Validity, family = binomial(link = "logit"))
summary(F1U3_Validity_RO) #OR = 1.01, p = 0.52

F2U3_Validity_RO <- glm(RunOut2Cat ~ MES_F2, data = Uni3_Validity, family = binomial(link = "logit"))
summary(F2U3_Validity_RO) #OR = 1.14, p < .001

F1U2_Validity_RO <- glm(RunOut2Cat ~ MES_F1, data = Uni2_Validity, family = binomial(link = "logit"))
summary(F1U2_Validity_RO) #OR = 0.90, p = 0.02

F2U2_Validity_RO <- glm(RunOut2Cat ~ MES_F2, data = Uni2_Validity, family = binomial(link = "logit"))
summary(F2U2_Validity_RO) #OR = 1.15, p = 0.02

F1U3_Validity_DL <- glm(DidntLast2Cat ~ MES_F1, data = Uni3_Validity, family = binomial(link = "logit"))
summary(F1U3_Validity_DL) #OR = 1.03, p = 0.19

F2U3_Validity_DL <- glm(DidntLast2Cat ~ MES_F2, data = Uni3_Validity, family = binomial(link = "logit"))
summary(F2U3_Validity_DL) #OR = 1.17, p < .001

F1U2_Validity_DL <- glm(DidntLast2Cat ~ MES_F1, data = Uni2_Validity, family = binomial(link = "logit"))
summary(F1U2_Validity_DL) #OR = 0.93, p = 0.09

F2U2_Validity_DL <- glm(DidntLast2Cat ~ MES_F2, data = Uni2_Validity, family = binomial(link = "logit"))
summary(F2U2_Validity_DL) #OR = 1.08, p = 0.15
