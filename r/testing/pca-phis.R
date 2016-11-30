rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('10-distance-to-avg-distance.csv', header=T)

phis = cbind(cdata$phi1, cdata$Phi, cdata$phi2)

pc = prcomp(phis, scale=T)

plot(cumsum(pc$sdev^2/sum(pc$sdev^2))) #cumulative explained variance

phis.transformed = pc$x
my.data = data.frame(cbind(cdata$dadN, phis.transformed[,1]))
colnames(my.data) = c("dadN","phis.transformed")
hist(my.data$phis.transformed)
plot(my.data$phis.transformed, my.data$dadN)
lm = lm(dadN~poly(phis.transformed, 8), data=my.data)
summary(lm)

xs = data.frame(seq(min(my.data$phis.transformed), 
                    max(my.data$phis.transformed), 
                    length.out = 100))

colnames(xs) = c('phis.transformed')

predictions = predict(lm, newdata=xs)

points(xs$phis.transformed, predictions, col='red')
plot(cdata$phi1, cdata$dadN)
plot(cdata$Phi, cdata$dadN)
plot(cdata$phi2, cdata$dadN)
