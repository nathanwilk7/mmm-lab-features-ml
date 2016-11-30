# svr tuning test for beta

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
num_samples = 1000
temp = c.data[sample(nrow(c.data), num_samples), ]

train.b = temp[temp[, 'theta'] < 2.09,]
#train.b = temp[temp[, 'theta'] > 1.05,]
train.b['on_crack_front'] = NULL
train = my.scale(train.b)
train.input = data.frame(train$distance_to_grain_boundary, train$unit_vector_to_grain_boundary_x, train$unit_vector_to_grain_boundary_y, train$unit_vector_to_grain_boundary_z
                         , train$phi1, train$Phi, train$phi2, train$misorientation, train$scaled_angle)
colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
train.dadN = train$dadN
train.beta = train$beta
train.cz = train$change_in_z

test.b = temp[temp[, 'theta'] >= 2.09,]
#test.b = temp[temp[, 'theta'] <= 1.05,]
test.b['on_crack_front'] = NULL
test = my.scale(test.b)
test.input = data.frame(test$distance_to_grain_boundary, test$unit_vector_to_grain_boundary_x, test$unit_vector_to_grain_boundary_y, test$unit_vector_to_grain_boundary_z
                        , test$phi1, test$Phi, test$phi2, test$misorientation, test$scaled_angle)
colnames(test.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
test.dadN = test$dadN
test.beta = test$beta
test.cz = test$change_in_z

library(e1071)

models <- vector(mode = "list", length = 7*7)
costs <- vector(mode = "list", length = 7*7)
gammas <- vector(mode = "list", length = 7*7)
i = 1

for (my.cost in 10^(-3:3))
{
  for (my.gamma in 10^(-3:3))
  {
    model <- svm(train.cz~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+phi1+phi2+misorientation+scaled_angle, data=train.input, kernel ="radial", 
                 cost=my.cost, gamma=my.gamma)
    models[[i]] = model
    costs[i] = my.cost
    gammas[i] = my.gamma
    i = i + 1
    cat(my.cost)
    print(my.gamma)
  }
}

y.mean.cz = mean(test$change_in_z)
mean.error.cz = sum((y.mean.cz - test$change_in_z)^2) / nrow(test)

train.error.cz <- vector(mode = "list", length = 7*7)
test.error.cz <- vector(mode = "list", length = 7*7)
minimum.test.error = 999
minimum.index = 1

for (i in 1:(7*7))
{
  y.pred.train = predict(models[[i]], train.input)
  train.error = sum((y.pred.train - train$change_in_z)^2) / nrow(train)
  y.pred.test = predict(models[[i]], test.input)
  test.error = sum((y.pred.test - test$change_in_z)^2) / nrow(test)
  
  train.error.cz[i] = train.error
  test.error.cz[i] = test.error
  
  if (test.error < minimum.test.error)
  {
    minimum.test.error = test.error
    minimum.index = i
    print(i)
  }
}
#test.error.dadN[30]

library(lattice)

to.plot = matrix(test.error.cz, nrow=7, ncol=7)
levelplot(to.plot, at=c(.7,.8,.9,1,1.1,1.2), ylab='Costs', xlab='Gammas')
test.error.cz[minimum.index]
costs[minimum.index]
gammas[minimum.index]

plot(1:49,test.error.cz)
abline(h=test.error.cz[minimum.index], col='red', lwd=3)
print(minimum.index)
print(test.error.cz[minimum.index])
print(costs[minimum.index])
print(gammas[minimum.index])

