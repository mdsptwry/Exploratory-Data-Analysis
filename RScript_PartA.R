#* Part A
getwd()
#Task 1
#***first task: sort the two files by subject ID
PartA_IV_sorted <- PartA_IV[order(PartA_IV$ID),]
PartA_DV_sorted <- PartA_DV[order(PartA_DV$ID),]

#checking sorted data
View(PartA_IV_sorted)
View(PartA_DV_sorted)

#checking number of observations of IV and DV
# number of observations should be the same regardless of sorting
str(PartA_IV_sorted) # 644 observations of 2 variables
str(PartA_DV_sorted) # 644 observations of 2 variables

#***second task: merge the data
PartA <- merge(PartA_IV_sorted, PartA_DV_sorted, by='ID')
View(PartA)
str(PartA)
# 644 observations of 3 variables (ID, IV, DV)

#account for missing values
any(is.na(PartA[, 2])==TRUE)
any(is.nan(PartA[, 2])==TRUE)

#number of IDs with IV
Count_IV <- nrow(PartA[!is.na(PartA$IV),])
Count_IV # 548 IVs

#number of IDs with DV
Count_DV <- nrow(PartA[!is.na(PartA$DV),])
Count_DV # 553 DVs

#number of IDs with both IV and DV
Count_Both <- nrow(PartA[!is.na(PartA$IV) & !is.na(PartA$DV),])
Count_Both # 467 IDs with both IV and DV
#and we know that number of ID with at least one IV or DV is 644

#Task 2: impute missing variables
#***install mice package
install.packages('mice')
library(mice)

#inspecting pattern of missing data
md.pattern(PartA)
# 10 observations have neither IV nor DV
# drop the 10 observations
PartA_imp <- PartA[!is.na(PartA$IV)==TRUE | !is.na(PartA$DV)==TRUE,]

#impute missing value
imp <- mice(PartA_imp, method = "norm.boot", printFlag = FALSE)
PartA_complete <- complete(imp)

#check if data is imputed
md.pattern(PartA_complete)
# gives us 634 IDs with both IV and DV

#regression model
RegM <- lm(DV ~ IV, data = PartA_complete)
summary(RegM)

# Call:
# lm(formula = DV ~ IV, data = PartA_complete)
#
# Residuals:
#  Min      1Q  Median      3Q     Max 
# -8.9790 -1.9741 -0.1377  2.0041 11.4102 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  16.7536     0.6267   26.73   <2e-16 ***
#  IV            2.3505     0.1234   19.04   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3.117 on 632 degrees of freedom
# Multiple R-squared:  0.3646,	Adjusted R-squared:  0.3636 
# F-statistic: 362.6 on 1 and 632 DF,  p-value: < 2.2e-16

#** we notice that adjusted R-

# knitr package already installed
library(knitr)
kable(anova(RegM), caption = 'ANOVA Table')

#Table: ANOVA Table

#|          |  Df|   Sum Sq|     Mean Sq| F value| Pr(>F)|
#  |:---------|---:|--------:|-----------:|-------:|------:|
#  |IV        |   1| 3523.505| 3523.505248| 362.628|      0|
#  |Residuals | 632| 6140.880|    9.716582|      NA|     NA|

plot(PartA_complete$DV ~ PartA_complete$IV, main='Scatter: DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(RegM, col='red', lty=1, lwd=2)

# 95% confidence interval
confint(RegM, level = 0.95)
#                2.5 %    97.5 %
#  (Intercept) 15.522984 17.984282
# IV           2.108076  2.592842



