#* Part B
#find insight on the data
str(PartB)
# 565 observations of 3 variables

#lets check for transformation
RM <- lm(y ~ x, data = PartB)
summary(RM)
#Call:
#  lm(formula = y ~ x, data = PartB)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.12637 -0.26449  0.01148  0.26029  1.31491 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.18129    0.03806   31.04   <2e-16 ***
#  x            0.58088    0.02532   22.94   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3911 on 563 degrees of freedom
#Multiple R-squared:  0.4832,	Adjusted R-squared:  0.4823 
#F-statistic: 526.4 on 1 and 563 DF,  p-value: < 2.2e-16

plot(PartB$y ~ PartB$x, main='Scatter: y ~ x', xlab='x', ylab='y', pch=20)
lines(lowess(PartB$x, PartB$y), col='red')
# the data seems to follow a logarithmic growth
# the relation is increasing at a decreasing rate 
# we use logarithmic tansofrmation 
xlog <- log(PartB$x)
ylog <- log(PartB$y)
transformed_data <- data.frame(xtrans=xlog, ytrans= ylog)
linear_model <- lm(ytrans ~ xtrans, data = transformed_data)
summary(linear_model)

## output shown below 

#Call:
#  lm(formula = y ~ x, data = transformed_data)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.60464 -0.12503  0.01381  0.15039  0.59465 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.568731   0.009779   58.16   <2e-16 ***
#  x           0.404272   0.015744   25.68   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2254 on 563 degrees of freedom
#Multiple R-squared:  0.5394,	Adjusted R-squared:  0.5386 
#F-statistic: 659.3 on 1 and 563 DF,  p-value: < 2.2e-16

#using logarithmic transformation we were able to get a higher 
# r - squared value 0.5386

#** time to bin the data
# first group the data
groups <- cut(transformed_data$xtrans,breaks=c(-Inf,seq(min(transformed_data$xtrans)+0.05, 
max(transformed_data$xtrans)-0.05,by=0.05),Inf))
table(groups)

x <- ave(transformed_data$xtrans, groups)
data_bin <- data.frame(x=x, y=transformed_data$ytrans)

#installing packages
install.packages('remotes')
install_github("cran/alr3")
library(alr3)
fit_b <- lm(y ~ x, data = data_bin)
pureErrorAnova(fit_b)

#output
# Analysis of Variance Table

# Response: y
# Df              Sum Sq Mean Sq  F value    Pr(>F)    
# x              1 33.557  33.557 723.3047 < 2.2e-16 ***
#  Residuals    563 28.530   0.051                       
# Lack of fit  47  4.591   0.098   2.1055 5.061e-05 ***
#  Pure Error  516 23.939   0.046                       
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## The high p-value for the LOF component indicates that upon transforming 
## our data, there is no significant Lack of fit in our regression model

summary(fit_b)
#output 

#Call:
#  lm(formula = y ~ x, data = data_bin)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.60568 -0.12338  0.01476  0.14990  0.60179 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.568652   0.009768   58.21   <2e-16 ***
#  x           0.404789   0.015730   25.73   <2e-16 ***
#  ---
#  Signif. codes:  
 # 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2251 on 563 degrees of freedom
#Multiple R-squared:  0.5405,	Adjusted R-squared:  0.5397 
#F-statistic: 662.2 on 1 and 563 DF,  p-value: < 2.2e-16

confint(fit_b, level = 0.95)



plot(data_bin$y ~ data_bin$x, main='Scatter: y ~ x', xlab='x', ylab='y', pch=20)
lines(lowess(data_bin$x, data_bin$y), col='red')




