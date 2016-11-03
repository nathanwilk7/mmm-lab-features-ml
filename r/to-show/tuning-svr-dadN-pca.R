# svr tuning test

rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
c.data = read.csv('6-beta.csv', header=T)

my.scale=function(df)
{
  for (i in 1:ncol(df))
  {
    new.col = (df[,i] - mean(df[,i])) / sd(df[,i])
    if (i == 1)
    {
      scaled.df = data.frame(new.col)
    }
    else
    {
      scaled.df = data.frame(scaled.df, new.col)
    }
  }
  colnames(scaled.df) = colnames(df)
  return(scaled.df)
}

set.seed(7) # 3 same results
num_samples = 5000
temp = c.data[sample(nrow(c.data), num_samples), ]

train.b = temp[temp[, 'theta'] < 2.09,]
#train.b = temp[temp[, 'theta'] > 1.05,]

real.data.missing.train = data.frame(train.b$distance_to_grain_boundary, train.b$unit_vector_to_grain_boundary_x, train.b$unit_vector_to_grain_boundary_y, train.b$unit_vector_to_grain_boundary_z, 
                               train.b$phi1, train.b$Phi, train.b$phi2, train.b$misorientation, train.b$scaled_angle)

real.data.b.train = real.data.missing.train[complete.cases(real.data.missing.train),]
real.data.train = my.scale(real.data.b.train)

pc.2 = prcomp(real.data.train, scale=T, tol = .70, retx = T)
summary(pc.2)
plot(pc.2)

library(ggfortify)
autoplot(pc.2)

#train.b['on_crack_front'] = NULL
#train = my.scale(train.b)
train.input = data.frame(pc.2$x)
#colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
train.dadN = (train.b$dadN - mean(train.b$dadN)) / sd(train.b$dadN)
train.beta = (train.b$beta - mean(train.b$beta)) / sd(train.b$beta)
train.cz = (train.b$change_in_z - mean(train.b$change_in_z)) / sd(train.b$change_in_z)

test.b = temp[temp[, 'theta'] >= 2.09,]
#test.b = temp[temp[, 'theta'] <= 1.05,]

real.data.missing.test = data.frame(test.b$distance_to_grain_boundary, test.b$unit_vector_to_grain_boundary_x, test.b$unit_vector_to_grain_boundary_y, test.b$unit_vector_to_grain_boundary_z, 
                                    test.b$phi1, test.b$Phi, test.b$phi2, test.b$misorientation, test.b$scaled_angle)

real.data.b.test = real.data.missing.test[complete.cases(real.data.missing.test),]
real.data.test = my.scale(real.data.b.test)

colnames(real.data.test) = colnames(real.data.train)
pc.2.test = predict(pc.2, real.data.test)
#pc.2.test = prcomp(real.data.test, scale=T, tol = .70, retx = T)
#summary(pc.2)
plot(pc.2.test)

library(ggfortify)
autoplot(pc.2)

#test.b['on_crack_front'] = NULL
#test = my.scale(test.b)
test.input = data.frame(pc.2.test)
#colnames(test.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
test.dadN = (test.b$dadN - mean(test.b$dadN)) / sd(test.b$dadN)
test.beta = (test.b$beta - mean(test.b$beta)) / sd(test.b$beta)
test.cz = (test.b$change_in_z - mean(test.b$change_in_z)) / sd(test.b$change_in_z)


library(e1071)

models <- vector(mode = "list", length = 7*7)
costs <- vector(mode = "list", length = 7*7)
gammas <- vector(mode = "list", length = 7*7)
i = 1

for (my.cost in 10^(-3:3))
{
  for (my.gamma in 10^(-3:3))
  {
    model <- svm(train.dadN~., data=train.input, kernel ="radial", 
                 cost=my.cost, gamma=my.gamma)
    models[[i]] = model
    costs[i] = my.cost
    gammas[i] = my.gamma
    print(i)
    i = i + 1
  }
}

y.mean.dadN = mean(test.dadN)
mean.error.dadN = sum((y.mean.dadN - test.dadN)^2) / nrow(test.dadN)

train.error.dadN <- vector(mode = "list", length = 7*7)
test.error.dadN <- vector(mode = "list", length = 7*7)
minimum.test.error = 999
minimum.index = 1

for (i in 1:(7*7))
{
  y.pred.train = predict(models[[i]], train.input)
  train.error = sum((y.pred.train - train.dadN)^2) / length(train.dadN)
  y.pred.test = predict(models[[i]], test.input)
  test.error = sum((y.pred.test - test.dadN)^2) / length(test.dadN)
  
  train.error.dadN[i] = train.error
  test.error.dadN[i] = test.error
  
  if (test.error < minimum.test.error)
  {
    minimum.test.error = test.error
    minimum.index = i
    print(i)
  }
}
#test.error.dadN[30]

to.plot = matrix(test.error.dadN, nrow=7, ncol=7)
levelplot(to.plot, at=c(.7,.8,.9,1,1.1,1.2), ylab='Costs', xlab='Gammas')
test.error.dadN[minimum.index]
costs[minimum.index]
gammas[minimum.index]

plot(1:49,test.error.dadN)
abline(h=test.error.dadN[minimum.index], col='red', lwd=3)
print(minimum.index)
print(test.error.dadN[minimum.index])
print(costs[minimum.index])
print(gammas[minimum.index])
