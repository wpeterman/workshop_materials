# Site occupancy simulation from Kery et al 2016 
# Models with ubms and brms 
# Shane A. Scaggs 

# Simulation ----

# Choose sample sizes and prepare observed data array y 
set.seed(24)
M = 100 # sites
J = 2   # presence absence measurements 
y = matrix(NA, nrow = M, ncol = J) 

# Parameter values 
psi = 0.8  # probability of occupancy 
p = 0.5    # probability of detection 

# Generate presence absence data (the truth)
z = rbinom(n=M, size=1, prob=psi)

# Generate detection/nondetection data 
for(j in 1:J) y[,j] = rbinom(n=M, size=1, prob=z*p)

# Summarize the data
sum(z)
sum(apply(y,1,max))

dat = cbind(z=z, y1=y[,1], y2=y[,2])

# Model with unmarked ----
require(unmarked)

# make unmarked data frame 
umf = unmarkedFrameOccu(y=y)
summary(umf)
(fm1 = occu(~1 ~1, data=umf)) # fit the model 

# recover parameters psi and p 
backTransform(fm1, 'state')
backTransform(fm1, 'det')

# Model with ubms ----
detach("package:unmarked", unload=TRUE)
library(ubms) 
(fit_stan = stan_occu(~1~1, data=umf, chains=2, iter=1000, cores=2, seed=24))
cbind(unmarked=coef(fm1), stan=coef(fit_stan))

# extract samples 
post = extract(fit_stan)


# plotting probabilities
library(tidyverse)
library(ggdist)
betas = data.frame(det = post$beta_det, 
                   state = post$beta_state) 

inv.logit = function(x) exp(x) / (1 + exp(x))
betas$prob_det = inv.logit(betas$det)
betas$prob_state = inv.logit(betas$state)


# plotting posterior density
betas %>%
    ggplot() + 
    theme_bw() + 
    
    # plot density curve, label, and mean line for state
    stat_density( aes(x=prob_state), fill="#11bb9955", adjust = 0.5 ) + 
    geom_text(aes(0.88, 4, label='occupancy\nprobability'), col="#11a099") + 
    geom_vline(xintercept = mean(betas$prob_state), col="#11a099",lty=2) + 
    
    # plot density curve, label, and mean line for detection
    stat_density( aes(x=prob_det), fill = "#dd00dd55", adjust = 0.5 ) + 
    geom_text(aes(0.42, 4, label='detection\nprobability'), col="#aa00dd") + 
    geom_vline(xintercept = mean(betas$prob_det), col="#aa00dd",lty=2) + 
    labs(x='parameter estimate', y='posterior density') 

# Choose sample sizes and prepare obs. data array y
set.seed(1) # So we all get same data set
M <- 100 # Number of sites
J <- 3 # Number of presence/absence measurements
y <- matrix(NA, nrow = M, ncol = J) # to contain the obs. data
# Create a covariate called vegHt
vegHt <- sort(runif(M, -1, 1)) # Sort for graphical convenience
# Choose parameter values for occupancy model and compute occupancy
beta0 <- 0 # Logit-scale intercept
beta1 <- 3 # Logit-scale slope for vegHt
psi <- plogis(beta0 + beta1 * vegHt) # Occupancy probability
# plot(vegHt, psi, ylim = c(0,1), type = "l", lwd = 3) # Plot psi relationship
# Now visit each site and observe presence/absence perfectly
z <- rbinom(M, 1, psi) # True presence/absence
# Create a covariate called wind
wind <- array(runif(M * J, -1, 1), dim = c(M, J))
# Choose parameter values for measurement error model and compute detectability
alpha0 <- -2 # Logit-scale intercept
alpha1 <- -3 # Logit-scale slope for wind
p <- plogis(alpha0 + alpha1 * wind) # Detection probability
# plot(p ~ wind, ylim = c(0,1)) # Look at relationship
# Take J [ 3 presence/absence measurements at each site
for(j in 1:J) {
    y[,j] <- rbinom(M, z, p[,j])
}


# Create factors
time <- matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
hab <- c(rep("A", 33), rep("B", 33), rep("C", 34)) # Must have M = 100

umf2 = unmarkedFrameOccu(y=y, 
                         siteCovs = data.frame(vegHt = vegHt, hab=hab),
                         obsCovs = list(wind = wind, time = time))

fit_stan2 = stan_occu(~wind ~vegHt, data=umf2, chains=2, iter=1000, cores=2, seed=24)
coef(fit_stan2)

post2 = ubms::extract(fit_stan2)
plot_effects(fit_stan2, submodel = 'det')
