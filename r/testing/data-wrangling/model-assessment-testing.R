rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('5-crack-surface-normal.csv', header=T)
typeof(cdata)
train = sample(nrow(cdata), nrow(cdata)/2)
lm.fit=lm(dadN3Dline~misorientation, data=cdata, subset=train)
summary(lm.fit)
mean((cdata$dadN3Dline-predict(lm.fit,cdata))[-train]^2)
mean((cdata$dadN3Dline-mean(cdata$dadN3Dline))^2)
normalize = function (data)
{
  return(data - mean(data)) / sd(data)
}
scaled.dadN3Dline = (cdata$dadN3Dline - mean(cdata$dadN3Dline)) / sd(cdata$dadN3Dline)
scaled.misorientation = normalize(cdata$misorientation)

model <- lm(scaled.dadN3Dline ~ scaled.misorientation, subset=train)
summary(model)
new <- data.frame(scaled.misorientation = scaled.misorientation)
mean((scaled.dadN3Dline - predict(model, newdata = new))[-train]^2)
mean((scaled.dadN3Dline - mean(scaled.dadN3Dline))[-train]^2)
library(tree)
tree = tree(scaled.dadN3Dline~scaled.misorientation)
summary(tree)
plot(tree)
text(tree,pretty=0)
mean((scaled.dadN3Dline - predict(tree, newdata = new))[-train]^2)

model <- lm(cdata$dadN3Dline ~ cdata$misorientation, subset=train)
summary(model)
new <- data.frame(cdata$misorientation)
mean((cdata$dadN3Dline - predict(model, newdata = new))[-train]^2)
mean((cdata$dadN3Dline - mean(cdata$dadN3Dline))[-train]^2)
library(tree)
tree = tree(dadN3Dline~., data=cdata)
summary(tree)
?plot
plot(tree)
text(tree,pretty=0)
mean((cdata$dadN3Dline - predict(tree, newdata = cdata))[-train]^2)

library(boot)
cv.error.10=0
glm.fit=glm(cdata$dadN3Dline~cdata$distance_to_grain_boundary)
summary(glm.fit)
cv.error.10=cv.glm(cdata,glm.fit,K=10)$delta[1]
cv.error.10
?cv.glm
