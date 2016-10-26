rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('6-beta.csv', header=T)
fix(cdata)

install.packages('leaps')
library(leaps)
regfit.full=regsubsets(dadN~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+
                       phi1+Phi+phi2+misorientation+scaled_angle, cdata)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab='Number of Variables',ylab='RSS',type='l')
plot(reg.summary$adjr2, xlab='Number of Variables',ylab='Adjusted RSq',type='l')