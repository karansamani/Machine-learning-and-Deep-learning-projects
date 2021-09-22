setwd("E:/NU/Engineering Probability and Statistics IE 6200 Sec 02/Project/winequality")

library(dplyr)
library(pastecs) # library to compute descriptive statistics 
library(kableExtra) #for building(plotting) nice tables
library(modeest)
library(e1071)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(lubridate)
library(TeachingDemos)
library(viridis)
library(fitdistrplus)

install.packages("TeachingDemos")
library(TeachingDemos)

raw_data = read.table('winequality-white.csv',
                      header = TRUE,
                      sep = ';')

# Updating column names(Default names have spaces b/w words)
colnames(raw_data) = c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "chlorides", "free_sulfer_dioxide", "total_sulpher_dioxide", "density", "pH", "sulphates", "alcohol", "quality")

# summary of each column
summary(raw_data)

#-----------Descriptive Statistics----------------------------------------------

# Computing Basic Descriptive Statistics for all columns
x = raw_data
x
descriptive_statistics = sapply(x, function(x) c( "sample_size" = length(x),
                                                  "missing_values" = sum(is.na(x)),
                                                  "min" = min(x),
                                                  "max" = max(x),
                                                  "range" = max(x)-min(x),
                                                  "median" = median(x),
                                                  "mode" = mfv(x),
                                                  "mean"= mean(x,na.rm=TRUE),
                                                  "var" = var(x),
                                                  "std dev" = sd(x),
                                                  "cv" = sd(x)/mean(x,na.rm=TRUE),
                                                  "skewness" = skewness(x),
                                                  "kurtosis" = kurtosis(x)
)
)

# rounding all values to 2 decimal places
descriptive_statistics = round(descriptive_statistics, 2)

#Making table beautiful with themes and colors
descriptive_statistics %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, html_font = "Cambria",fixed_thead = list(enabled = T, background = "#ff9900")) %>% 
  row_spec(8, bold = F, color = "white", background = "#00cc44") %>% 
  row_spec(10, bold = F, color = "white", background = "#00cc44") %>%
  row_spec(6, bold = F, color = "white", background = "#006633")

# Adding Categorical columns
data_categorical = raw_data

# Adding categorical column for quality 
data_categorical$quality_category = cut(data_categorical$quality,
                                        breaks=c(0, 5, 7, 9),
                                        labels=c("low","medium","high"))

# Adding categorical column for sweetness  
data_categorical$sweetness_category = cut(data_categorical$residual_sugar,
                                          breaks=c(0, 1, 10, 35, 120, 220),
                                          labels=c("bone_dry","dry","off_dry","sweet", "very_sweet"), right = FALSE)

# Finding counts samples of each quality
ggplot(data = data_categorical)+
  geom_bar(mapping = aes(x=quality),fill="steelblue",color="black")+
  theme_bw() +
  xlab('Wine Quality')+
  scale_x_continuous("Wine Quality", labels = as.character(data_categorical$quality),
                     breaks = data_categorical$quality)

# Finding counts samples of each sweetness_category
c = c('bone dry', 'dry','off dry', 'sweet', 'very sweet')
ggplot(data = data_categorical)+
  geom_bar(mapping = aes(x=sweetness_category),fill="steelblue",color="black")+
  theme_bw() +
  xlab('Sweetness Category')+
  scale_x_discrete("Sweetness Category", labels = as.character(data_categorical$sweetness_category),
                   breaks = data_categorical$sweetness_category)

##----- PMF and CDF ---------------------------------------------------

#Defining the R.V
# X: The R.V of the quality of a dry wine

#selecting required columns
req_col=dplyr::select(data_categorical, quality, quality_category, sweetness_category)
head(req_col)

#selecting required rows
req_row=dplyr::filter(req_col, sweetness_category=='dry')
head(req_row)

#Frequency Table (with grouping and summarizing)
group_df=group_by(req_row, quality)
group_df=summarise(group_df, count=n())
head(group_df)

#Calculating PMF
dry_pmf=round(group_df$count/sum(group_df$count),3)
dry_pmf

dry_cdf=round(cumsum(dry_pmf),3)
dry_cdf

#bringing altogether in a single table
dry_freq=cbind(group_df, dry_pmf=dry_pmf, dry_cdf=dry_cdf)
dry_freq


##----- Joint Probability ---------------------------------------------

#X=sweetness_category Y=quality
#Y: 3,4,5,6,7,8,9

# Joint Frequency
sweetness_quality_freq = 
  data_categorical %>% 
  group_by(sweetness_category, quality) %>% 
  summarize(n = n())

sweetness_quality_freq
sweetness_quality_freq %>% 
  dcast(sweetness_category ~ quality, value.var = "n", fun.aggregate = sum)

# Joint Probability Distribution
sweetness_quality_pmf =
  sweetness_quality_freq %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n))

sweetness_quality_pmf$prop = round(sweetness_quality_pmf$prop,3)

sweetness_quality_pmf

# Joint Probability
sweetness_quality_pmf %>%   
  dcast(sweetness_category ~ quality, fill = 0, value.var = "prop")

# Storing the output in a matrix
sweetness_quality_pmf %>%   
  dcast(sweetness_category ~ quality, fill = 0, value.var = "prop") %>% 
  {. ->>sweetness_quality_matrix}
sweetness_quality_matrix

#------------ Correlation b/w sweetness and quality-------------------
x <- data_categorical$residual_sugar
y <- data_categorical$quality
# correlation coefficient
cor(x, y)
# Our data suggests that there is a weak or no correlation b/w
# Sweetness and quality

# Heat Map
# Converting wide format to long format
sweetness_quality_matrix_long = melt(sweetness_quality_matrix)
sweetness_quality_matrix_long

ggplot(data = sweetness_quality_matrix_long, aes(x=sweetness_category, y=variable, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu")+
  geom_label(aes(label = value))+
  labs(x = 'Sweetness Category', y = 'Quality')+
  scale_x_discrete("Sweetness Category", labels = as.character(sweetness_quality_freq$sweetness_category),
                   breaks = sweetness_quality_freq$sweetness_category)+ 
  scale_y_discrete("Quality", labels = as.character(sweetness_quality_freq$quality),
                     breaks = sweetness_quality_freq$quality)


# frequency scatter plot 
ggplot(data = sweetness_quality_freq,aes(x=sweetness_category, y=quality)) +
  geom_point(aes(size = n, color = n)) +
  labs(x = 'Sweetness Category', y = 'Quality')+
  theme_bw()+
  scale_x_discrete("Sweetness Category", labels = as.character(sweetness_quality_freq$sweetness_category),
                   breaks = sweetness_quality_freq$sweetness_category)+ 
  scale_y_continuous("Quality", labels = as.character(sweetness_quality_freq$quality),
                     breaks = sweetness_quality_freq$quality)

#----------- Goodness of Fit Tests ------------------------------------
#fitting test (continuous)

#plotting of the alcohol column distribution which tends to be normally distributed
ggplot(data_categorical,aes(x=alcohol))+
  geom_histogram(bins = 50, color ='Black', fill ='steelblue')


descdist(data_categorical$alcohol) 
fit_n <-fitdist(data_categorical$alcohol, "norm")
summary(fit_n)

#The skewness is between (-0.5,0.5) and hence it is symmetrical we can apply the fit test for normal,lognormal and uniform distribution as
#the point observation lies near to these distributions


#Checking the AIC,BIC AND LIKELIHOOD VALUES for lnorm distribution
fit_ln <-fitdist(data_categorical$alcohol, "lnorm")
summary(fit_ln)


#Checking the AIC,BIC AND LIKELIHOOD VALUES for uniform distribution
fit_un <-fitdist(data_categorical$alcohol, "unif")
summary(fit_un)

#As we see the AIC,BIC values for lnorm distribution is lower than that of the uniform distribution and the likehood value is 
#greater for the lognormal and hence we do the goodness of fit test for the lognormal distribution

#goodness of fit test for lognormal
par(mfrow=c(2,2))
plot.legend <-c("lognormal")
denscomp(list(fit_ln), legendtext = plot.legend, xlab ='Alcohol', xlegend ='topleft')
cdfcomp(list(fit_ln), legendtext = plot.legend, xlab ='Alcohol')
qqcomp(list(fit_ln), legendtext = plot.legend, xlab ='Alcohol')
ppcomp(list(fit_ln), legendtext = plot.legend, xlab ='Alcohol')

#goodness of fit test for uniform
par(mfrow=c(2,2))
plot.legend <-c("Uniform")
denscomp(list(fit_un), legendtext = plot.legend, xlab ='Alcohol', xlegend ='topleft')
cdfcomp(list(fit_un), legendtext = plot.legend, xlab ='Alcohol')
qqcomp(list(fit_un), legendtext = plot.legend, xlab ='Alcohol')
ppcomp(list(fit_un), legendtext = plot.legend, xlab ='Alcohol')

#goodness of fit test for normal
par(mfrow=c(2,2))
plot.legend <-c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab ='Alcohol', xlegend ='topleft')
cdfcomp(list(fit_n), legendtext = plot.legend, xlab ='Alcohol')
qqcomp(list(fit_n), legendtext = plot.legend, xlab ='Alcohol')
ppcomp(list(fit_n), legendtext = plot.legend, xlab ='Alcohol')

#we see the the graph shows the lognormal fits better for the continuous alcohol data




#fit test for discrete data=(quality)
ggplot(data_categorical,aes(quality))+
  geom_histogram(bins = 50, color ='Black', fill ='steelblue')


#Checking the AIC,BIC AND LIKELIHOOD VALUES for negative binomial distribution distribution 

fit_nb <-fitdist(data_categorical$quality,'nbinom')
summary(fit_nb)

#Checking the AIC,BIC AND LIKELIHOOD VALUES for poison distribution

fit_p <-fitdist(data_categorical$quality,'pois')
summary(fit_p)

#Checking the AIC,BIC AND LIKELIHOOD VALUES for geometric distribution

fit_g <-fitdist(data_categorical$quality,'geom')
summary(fit_g)

#using the gofstat function we compare the values of all the three distributions and we see poision fits the best amongst the three.

gofstat(list(fit_nb, fit_p,fit_g))

#Hence we go with the poison distribution

#------------------------- Hypothesis Testing ------------------------
#structure of the df
str(data_categorical)

## ------------------------- Scenario-1 ------------------------------------------------- 
## 1. We manufactured a batch of wines(1 Batch = 500 bottles of wine)
## 2. Now we want to check if we successfully manufactured dry wine or not
## 3. For testing, we take a sample of 100 wines from the batch
##    and check if their mean residual sugar content is 
##    equal to 5.5 g/dm3 or not
## 4. Why 5.5 g/dm?
##    For Dry wines, residual sugar content should be in [1 gm/dm3 - 10 gm/dm3]
##    Avg. is (1+10)/2 = 5.5 gm/dm3
## 5. We want to test with a confidence level of 95%

install.packages("devtools")
library(devtools)
install.packages("webr")
library(webr)




## Sampling
set.seed(100) 
data_batch = data_categorical[1:500,] #First Batch of wine 
data_sample = sample_n(data_batch, 100) #Sample for testing

# one-sample two-tailed z-test
# H0: Mu = 5.5 g/dm3
# H1: Mu != 5.5 g/dm3

# one-sample two-tailed z-test (known variance)
sample = data_sample$residual_sugar
pop = data_batch$residual_sugar
n = length(sample)
x_bar = mean(sample)
sigma = sd(pop)
Mu0 = 5.5
z_calc = (x_bar-Mu0)/(sigma/sqrt(n))
z_calc = round(z_calc,3)
z_calc

## Rejection-region method
alpha = 0.05
z0 = qnorm(alpha/2) # qnorm gives z_value corresponding to z_alpha/2 
z0 = round(z0,2)    # rounding
z0
c(z0,-z0) # critical values (acceptance region)

## z_calc = 1.113
## critival values: [-1.96,1.96] i.e, [ z_-alpha/2, z_alpha/2 ]

## Conclusion:
## 1. As z_calc does not lie in rejection region, we fail to reject H0
## 2. With 95% confidence our sample data shows that our batch
##    contains dry wine

## p-value method
z_calc
## p(z>1.11) = p(z<-1.11)
## Table and pnorm() calculates P(Z < z)
p_value = pnorm(-z_calc)
p_value
round(2*p_value, 3) # two-tailed test

## 2*p_value = 0.266
## 0.266 > 0.05 i.e, 2*p_value > alpha 

## Conclusion:
## 1. As 2*p_value > alpha(=0.05) we fail to reject H0
## 2. With 95% confidence our sample data shows that we have
##    successfully manufactured a batch of dry wine


## ------------------------- Scenario-2 ------------------------------------------------- 
## 1. We want to check if all of our batches contain volatile acids
##    with in the legal limits or not
## 2. US legal limit is 1.11 g/dm3    
## 3. For testing, we take a sample of 50 wines from each of the batches.(Stratified Random Sampling)
## 4. We get a total of 500 wines as sample
## 5. We check if their mean volatile acidity is exceeding the legal limit of 1.1 gm/dm3 or not 
## 6. We want to test with a confidence level of 95%


install.packages("dplyr")
library(dplyr)




# step-0: Sampling
stratified_random_sampling <- function(df,group_size,sample_size){
  nr = nrow(df)
  groups = split(df, rep(1:ceiling(nr/group_size), each=group_size, length.out=nr))  
  s = slice(df,0)
  for (i in groups){
    set.seed(100)
    s = rbind(s, sample_n(i,sample_size))
  }    
  return(s)
}

df = data.frame(data_categorical$volatile_acidity)
# stratified_random_sampling with group_size = 500 and sample_size = 50 from each group 
sample = stratified_random_sampling(df,500,50) 

# step-1: Formulating Hypothesis
# H0: Mu <= 1.1 g/dm3
# H1: Mu > 1.1 g/dm3

# step-2: one-sample right-tailed t-test (unknown variance)

pop_mean = mean(data_categorical$volatile_acidity)
pop_mean
mean(sample$data_categorical.volatile_acidity)
sd(sample$data_categorical.volatile_acidity)

t.test(x = sample , mu =1.1, alternative = "greater")

plot(t.test(sample$data_categorical.volatile_acidity , mu =1.1, alternative = "greater"))


# step-3: rejection region and p-value
# Rejection region:  z<0.2714
# p-value = 0.999~1

# step-4: Conclusion
# 1. As P_value > 0.05 we failed to reject the null hypothesis
# 2. We infer that the batch contains the amount of
#  volatile acidity within the legal limits

## ------------------------- Scenario-3 ------------------------------------------------- 
## 1. We want to check if we are producing healthy wine across across all batches 
## 2. For a healthy wine, the pH level should be in [2.9-3.9]. We take a target
##    standard deviation of 0.1
## 3. For testing, we take a sample of 50 wines from each of the batches.(Stratified Random Sampling)
## 4. We get a total of 500 wines as sample
## 5. We check if their mean volatile acidity is exceeding the legal limit of 1.1 gm/dm3 or not 
## 6. We want to test with a confidence level of 95%


#step-0: Sampling 
df = data.frame(data_categorical$pH)
# stratified_random_sampling with group_size = 500 and sample_size = 50 from each group 
pH_sample = stratified_random_sampling(df,500,50) 


#step-1: Formulating Hypothesis

# H0: sigma^2 = 0.01
# H1: sigma^2 != 0.01

#step-2: One-Sample two-tailed Variance test - Chi-Square test
sigma.test(x=pH_sample$data_categorical.pH,sigma = 0.1)



#step-3: Rejection Region

#LB = 0.019
#UB = 0.024
#Chi_squared_calc = 0.021

#step-4: Conclusion
# 1. Since, chi_squared_calc does not lie in the rejection region,
#    we fail to reject H0
# 2. We can infer that we are manufacturing healthy wine

## ------------------------- Scenario-4 ------------------------------------------------- 
## 1. We want to check if there is any variation in the
##    sulphur content of low quality and high-quality wines
## 2. For testing, we take two samples, 100 each from low quality and high quality wines
## 3. We check if ratio of their sample variances is equal to 1 or not 
## 4. We want to test with a confidence level of 95%

# step-0: Sampling

# lq = low quality
# hq = high quality

lq_wines = data_categorical %>% filter(quality_category=="low")
hq_wines = data_categorical %>% filter(quality_category=="high") 

set.seed(100)
lq_sample = sample_n(lq_wines, 100) # sample of 100 wines from low quality wines
set.seed(100)
hq_sample = sample_n(hq_wines, 100) # sample of 100 wines from high quality wines

# Step-1: Formulating Hypothesis
# H0: sigma_1^2 = sigma_2^2
# H1: sigma_1^2 != sigma_2^2 

# Step-2: Performing two-tailed F-Test
var.test(x=lq_sample$total_sulpher_dioxide, y=hq_sample$total_sulpher_dioxide)
plot(var.test(x=lq_sample$total_sulpher_dioxide, y=hq_sample$total_sulpher_dioxide))
# step-3: Rejection Region and p-value
#LB = 1.211
#UB = 2.674
#F_calc = 1.79
#p-value = 0.003781

# step-4: Conclusion
# 1. We observe that LB>1 and UB>1
# 2. p-value is 0.003781 < 0.05(alpha)
# 3. Hence, we reject the null hypothesis
# 4. We infer that there is high variation in the sulphur content of low quality wines

## ------------------------- Scenario-5 ------------------------------------------------- 
## 1. We want to target mainly middle-class customers
## 2. So, we want to ensure that each batch contains atleast 70%
##    of medium quality wines
## 3. For testing, we take a sample of 50 wines from each of the batches.(Stratified Random Sampling)
## 4. We get a total of 500 wines as sample
## 5. We check if the proportion of dry wines is less than 0.7 or not 
## 6. We want to test with a confidence level of 99%

# step-0: Sampling
df = data.frame(data_categorical$quality_category)
# stratified_random_sampling with group_size = 500 and sample_size = 50 from each group 
prop_sample = stratified_random_sampling(df,500,50) 

# step-1: Formulating Hypothesis
# H0: p >= 0.7
# H1: p < 0.7

# step2: one-sample left-tailed proportion-test
x = length(which(prop_sample$data_categorical.quality_category == "medium"))
x
n = length(prop_sample$data_categorical.quality_category) 
n
prop.test(x=x,n=n,p=0.7, correct = TRUE, conf.level=0.99, alternative = "less")

plot(prop.test(x=x,n=n,p=0.7, correct = TRUE, conf.level=0.99, alternative = "less"))

# step-3: Rejection Region

# LB= 0
# UB= 0.67
# p_calc = 0.63

# step-4: Conclusion
# 1. Since, p_calc(calculated using z_calc) does not lie in the rejection region,
#    we fail to reject H0
# 2. With very high confidence (99%) we conclude that we are ensuring
#    every batch contains atleast 70% of medium quality wines