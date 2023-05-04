library(tidyverse)

# plotting theme
plot_design = theme_bw() + 
    theme(text = element_text(size=16, color='black'),
          panel.border = element_rect(fill=NA, color='black'),
          axis.text = element_text(color='black'),
          axis.ticks.length = unit(3, 'mm')) 

# Human height/weight example 
set.seed(777)
N = 15
sex = sample(0:1, size = N, replace = T)

# average and SD height for each sex 
height_0 = 175
height_1 = 161
sd_0 = 6
sd_1 = 5

# simulate heights 
height = rnorm(n = N, 
               mean = ifelse(sex == 0, height_0, height_1), 
               sd = ifelse(sex == 0, sd_0, sd_1))

# set beta parameters 
bH = 0.5 
b_intercept = 66

# simulate weights 
weight = rnorm(n = N, mean = b_intercept + bH * scale(height), sd = 10)
d = data.frame(sex, height, weight)

# plot
ggplot(d, aes(x=height, y=weight)) + 
    plot_design + 
    geom_point( pch=21, size=3 )
#ggsave('HW.png', height = 5, width = 5.5, units = 'in', dpi=300)

# run three models: glm, brm (flat), brm (weakly informed )
# glm
m1 = glm( weight ~ scale(height), data = d, family = gaussian() )
summary(m1)

# flat brm 
m2 = brm( weight ~ scale(height), data = d, family = gaussian(),
          iter = 2000, cores = 2, chains = 2)

# informed brm 
f3 = bf(weight ~ scale(height), family = gaussian())
get_prior(f3, data = d)
p3 = c( prior(normal(0,2), class = b, lb = 0), 
        prior(normal(70,10), class = Intercept), 
        prior(exponential(1), class = sigma) )

m3 = brm( weight ~ scale(height), data = d, family = gaussian(),
          prior = p3,
          iter = 2000, cores = 2, chains = 2)

# summary table 
out = summary(m1)
sum_df = as.data.frame(rbind(out$coefficients[,1:2], fixef(m2)[,1:2], fixef(m3)[,1:2]))
sum_df$Parameter = rep(c('Intercept','height'), 3)
sum_df$Model = c('mle','mle','brm (flat)', 'brm (flat)', 'brm (informed)', 'brm (informed)')

# plot
library(tidyverse)

sum_df %>%
    ggplot( ) + 
    plot_design + 
    geom_vline(xintercept = 0, lty=2) + 
    #geom_vline(xintercept = 0.5, lty = 2, color = 'red') + 
    #geom_text(aes(10,'Intercept', label = 'true slope = 0.5'), color = 'red') + 
    #geom_vline(xintercept = 66, lty = 2, color = 'blue') + 
    #geom_text(aes(55,'height', label = 'true intercept = 66'), color = 'blue') + 
    geom_point( aes(x=Estimate, y = Parameter, color = Model), 
                position = position_dodge2(width=0.3)) + 
    geom_errorbar( aes(x = Estimate, 
                       xmin = Estimate - `Std. Error`, 
                       xmax = Estimate + `Std. Error`, 
                       y = Parameter, 
                       color = Model), 
                   width = 0, 
                   position = position_dodge(width=0.3)) + 
    labs(x='estimate', y='parameter')
#ggsave('estimates.png', height = 2.5, width = 7, units = 'in', dpi=300)

# marginal effects 
library(marginaleffects)

# predictions data frame
preds = rbind(
    plot_predictions(m1, condition = 'height', draw = F)[,c(1,2,4,5,6,7)], 
    plot_predictions(m2, condition = 'height', draw = F),
    plot_predictions(m3, condition = 'height', draw = F) )
preds$Model = rep(c('mle','brm (flat)', 'brm (informed)'), each = 50)

# lines only 
preds %>%
    ggplot() + 
    theme_bw() + 
    geom_line( aes(x=height, y=estimate, color = Model), linewidth = 1) 

# lines and error ribbons
preds %>%
    ggplot() + 
    plot_design + 
    geom_point(data = d, aes(x=height, y=weight), pch=21) + 
    geom_line( aes(x=height, y=estimate, color = Model), linewidth = 1) + 
    geom_ribbon( aes( x = height, ymin = conf.low, ymax = conf.high, fill=Model), alpha = 0.2) + 
    facet_wrap(~Model)
#ggsave('predictions.png', height = 3, width = 8, units = 'in', dpi=300)

# models including sex 
# run three models: glm, brm (flat), brm (weakly informed )
# glm
m4 = glm( weight ~ scale(height) + sex, data = d, family = gaussian() )

# flat brm 
m5 = brm( weight ~ scale(height) + sex, data = d, family = gaussian(),
          iter = 2000, cores = 2, chains = 2)

# informed brm 
f6 = bf(weight ~ scale(height) + sex, family = gaussian())
get_prior(f6, data = d)
p6 = c( prior(normal(0,2), class = b, coef = scaleheight),
        prior(normal(-5,1), class = b, coef = sex), 
        prior(normal(70,10), class = Intercept), 
        prior(exponential(1), class = sigma) )

m6 = brm( weight ~ scale(height) + sex, data = d, family = gaussian(),
          prior = p6,
          iter = 2000, cores = 2, chains = 2)

# summary table 
out2 = summary(m4)
sum_df2 = as.data.frame(rbind(out$coefficients[,1:2], fixef(m5)[,1:2], fixef(m6)[,1:2]))
sum_df2$Parameter = rep(c('Intercept','height'), 3)
sum_df2$Model = c('glm','glm','brm (flat)', 'brm (flat)', 'brm (informed)', 'brm (informed)')
