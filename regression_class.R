library(data.table)
library(ggplot2)
library(dplyr)
#To implement OLS on R, we use 'lm'=linear model command that performs linear modelling
##“YVAR ~ XVAR” where YVAR is the dependent, or predicted, variable and XVAR
#is the independent, or predictor, variable
#DV=intercept+coffe.(IV),A formula has an implied intercept term.  To remove this use
   #either ‘y ~ x - 1’ or ‘y ~ 0 + x
Usage:
  lm(formula, data, subset, weights, na.action,
     method = 'qr', model = TRUE, x = False, y = FALSE, qr = TRUE,
     singular.ok = TRUE, contrasts = NULL, offset,...)
help(lm)
#weights=weighted least squre or numric col and weight col, NA = na.omit or what action should be
#method to be used; for fitting,or model.frame=true
#We are specifically considering OLS regressions here
lm(formula,data)

#Using data frames
datar=data.frame(size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3),
                 weights=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3))
datar
summary(datar)
reg0=lm(weights~size,datar)

summary(reg0)
########coefficient Standard Error measures the average amount that
#the coefficient estimates vary from the actual average value of our response variable
 #ideally lower then its coff, use to comput confidence interval
#########The coefficient t-value is a measure of how many
#standard deviations our coefficient estimate is far
#away from 0. more far away from zero indicate we could
#reject the null hypothesis - means a relationship between y and X exist
##########the probability of observing any value equal or larger than t.
#A small p-value indicates that it is unlikely we will observe a relationship between
#the predictor and response  variables due to chance.
#Typically, a p-value of 5% or less is a good cut-off point
#######multipal/adjusted r squard-takes form of proportion of variance, limit 0( a regression
###that does not explain the variance in the response variable well)to 1
##########for F statistics h0 isno relation b/w y and x.how much larger the
#F-statistic needs to be depends on both the number of data points and the number of predictors

                                        #Regression Line
P=ggplot(reg0,aes(x=size,y=weights))+
  geom_point() +
  labs(x='size in feet',y='weights per kg',title='Model1')+
  geom_smooth(method='lm',formula=y~x)
P
help(geom_smooth)
ggsave('P.png')
##se is confidence interval present or not, by default TRUE
#syntax ## Default  method:Quantile Quantile plots, by defaule Q1 and Q3
qqnorm(y, ylim, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE, ...)

qqline(y, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, ...)
#probs=probability,
qqplot(x, y, plot.it = TRUE,
       xlab = deparse1(substitute(x)),
       ylab = deparse1(substitute(y)), ...)
#Residual plot

resr=resid(reg0)
resr
fortify(reg0)
#Convert a curves and points object to a data frame for ggplot2

P1=ggplot(aes(x=.fitted,y=.resid),data=reg0)+
  geom_point()+
  geom_hline(yintercept=0)+labs(x="fitted values",y="residuals")
P1
qqnorm(resr);qqline(resr, col = 3)
help(qqline)
#####qqline adds a line to a “theoretical”, by default normal,
##quantile-quantile plot which passes through the probs quantiles

#Using data sets
uredata = readxl::read_excel('~/Downloads/data for R.xlsx')
uredata = as.data.table(uredata)
uredata
summary(uredata)

#Multivariate regression with numeric variables
reg1 = lm(URE ~ exped + Man , data = uredata)
summary(reg1)

#Multivariate regressions with one categorical variables
uredata$Dnag = as.factor(uredata$Dnag)
summary(uredata)
reg3=lm(URE~exped+Agri+Man+Dnag,data=uredata)
summary(reg3)
#ActualVsPredicted
P2=ggplot(reg3,aes(x=8.30-0.66*exped-0.15*Agri-0.46*Man,y=URE))+
  geom_point() +
  labs(x='predicted URE',y='URE',title='Model2')+
  geom_smooth(method='lm',formula=y~x,se=FALSE)
P2
#Residual plot
resr=resid(reg3)
resr
fortify(reg3)
P3=ggplot(aes(x=.fitted,y=.resid),data=reg3)+
  geom_point()+
  geom_hline(yintercept=0)+labs(x="fitted values",y="residuals")
P3
qqnorm(resr);qqline(resr, col = 5)



library(lmtest)
#bptest(formula, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)
#Breusch-Pagan Test is used to determine if heteroscedasticity is present in a regression analysis
bptest(reg3)
#p value is not less then 0.05,so fail to reject H0/ accept H0 or say no hetroscadicity present.


######################################
library(car)
library(geepack)
library(lfe)
library(plm)#data and balanced or not
##extra for time
data("Grunfeld", package = "plm")
Grunfeld %>%
  select(year, firm) %>%
    table()

# examined with function is.pbalanced()
Grunfeld %>%
    is.pbalanced()

library(ggplot2)
##graph
ggplot(data = Grunfeld, aes(x = year, y = inv)) +
  geom_line() +
  labs(x = "Year",  y = "Gross Investment") +
    theme(legend.position = "none")

#to pool observations of the same individual recorded at different time points
pooled_ols_lm <- lm(inv ~ capital, data = Grunfeld )
summary(pooled_ols_lm)


##FE model can be estimated by including dummy variables for all firms
fe_model_lm <- lm(inv ~ capital + factor(firm), data = Grunfeld)

summary(fe_model_lm)


##rgument model= is now set to "within". This is the within estimator with n entity-specific intercepts
fe_model_plm <- plm(inv ~ capital, data = Grunfeld,
                    index = c("firm", "year"),
                    effect = "individual", model = "within")

summary(fe_model_plm)
