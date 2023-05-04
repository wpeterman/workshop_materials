# Dealing with missing values 
# helpful tutorial: https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
library(tidyverse)

# Generate data set ----
set.seed(777)
N = 40
x = rnorm(N, mean = 0, sd = 2)
z = rnorm(N, mean = 0.5*x, sd = 2)
y = rnorm(N, mean = -0.3*x + 0.5*z, sd = 2)

plot(x,y)
plot(z,y)

d = data.frame(x,z,y)

# Create missing values in y ----
p_missing = 0.2
missing_id = sample(1:N, size = N*p_missing, replace = F)

d[ missing_id, 'y'] = NA

# Make predictions for missing values: two approaches ----
library(brms)

d_cc = d[ complete.cases(d), ]
d_na = d[ is.na(d$y), ]

# model 1
m1 = brm( y ~ x + z, data = d_cc, family = 'gaussian', 
          cores = 2, chains = 2, iter = 2000)

# predictions for complete and NA data frames 
pred_cc = as.data.frame(predict(m1, newdata = d_cc))
pred_cc$miss = 0
pred_na = as.data.frame(predict(m1, newdata = d_na))
pred_na$miss = 1

# model 2 
# model excludes NAs
m2 = brm( y ~ x + z, data = d, family = 'gaussian', 
          cores = 2, chains = 2, iter = 2000)
# but we can get back same predictions as above by using d as new data 
predict(m2, newdata = d)    

# missing values in z ----
d = data.frame(x,z,y)
p_missing = 0.2
missing_id = sample(1:N, size = N*p_missing, replace = F)
d[ missing_id, 'z'] = NA
d

# option 1: imputation before model using mice
library(mice)
imp = mice(d, m = 5, print = F)
imp

# run model on imputed datasets 
m3_imp = brm_multiple(y ~ x + z, data = imp, chains = 2)
m3_imp

# some Rhat issues need to be resolved; but they are all that bad 
plot(m3_imp, variable = "^b", regex = T)
round(m3_imp$rhats, 3)

# option 2: imputation during model 
# here we impute z as a function x, since x was a predictor of z 
# this requires a submodel: z | mi() ~ x
f4 = bf( y ~ x + mi(z)) + bf( z | mi() ~ x) + set_rescor(F)
m4_imp = brm( f4, data = d, chains = 2 )
m4_imp



# missing values in y and z ----
d = data.frame(x,z,y)
p_missing = 0.2
missing_z = sample(1:N, size = N*p_missing, replace = F)
missing_y = sample(1:N, size = N*p_missing, replace = F)

d[ missing_z, 'z'] = NA
d[ missing_y, 'y'] = NA

# impute both simultaneously 
f5 = bf( y | mi() ~ x + mi(z)) + bf( z | mi() ~ x) + set_rescor(F)
m5_imp = brm( f5, data = d, chains = 2)
m5_imp
