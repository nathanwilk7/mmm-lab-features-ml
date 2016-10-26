rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
small.data = read.csv('6-beta.csv', header=T)

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
temp = small.data[sample(nrow(small.data), num_samples), ]

train.b = temp[temp[, 'theta'] < 2.09,]
train.b['on_crack_front'] = NULL
train = my.scale(train.b)
train.input = data.frame(train$distance_to_grain_boundary, train$unit_vector_to_grain_boundary_x, train$unit_vector_to_grain_boundary_y, train$unit_vector_to_grain_boundary_z
                         , train$phi1, train$Phi, train$phi2, train$misorientation, train$scaled_angle)
colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
train.dadN = train$dadN
train.beta = train$beta
train.cz = train$change_in_z

test.b = temp[temp[, 'theta'] >= 2.09,]
test.b['on_crack_front'] = NULL
test = my.scale(test.b)
test.input = data.frame(test$distance_to_grain_boundary, test$unit_vector_to_grain_boundary_x, test$unit_vector_to_grain_boundary_y, test$unit_vector_to_grain_boundary_z
                        , test$phi1, test$Phi, test$phi2, test$misorientation, test$scaled_angle)
colnames(test.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
test.dadN = test$dadN
test.beta = test$beta
test.cz = test$change_in_z

library(e1071)

tune.cost = function(train.output, train.input)
{
  i = 1
  models <- vector(mode = "list", length = 7)
  for (cost.val in c(100,10,1,.01,.001,.0001,.00001))
  {
    models[[i]] <- svm(train.output~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+phi1+phi2+misorientation+scaled_angle, data=train.input, kernel ="radial", cost=cost.val, 
                       gamma=.01)
    i = i + 1
  }
  return(models)
}

tune.gamma = function(train.output, train.input)
{
  i = 1
  models <- vector(mode = "list", length = 7)
  for (gamma.val in c(100,10,1,.01,.001,.0001,.00001))
  {
    models[[i]] <- svm(train.output~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+phi1+phi2+misorientation+scaled_angle, data=train.input, kernel ="radial", 
                       cost=1, gamma=gamma.val)
    i = i + 1
  }
  return(models)
}

# dadN

models.gamma.dadN = tune.gamma(train.dadN, train.input)
y.mean.dadN = mean(test$dadN)
mean.error.dadN = sum((y.mean.dadN - test$dadN)^2) / nrow(test)
test.errors.gamma.dadN = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.gamma.dadN[[i]], train.input)
  train.error = sum((y.pred.train - train$dadN)^2) / nrow(train)
  y.pred.test = predict(models.gamma.dadN[[i]], test.input)
  test.error = sum((y.pred.test - test$dadN)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.dadN, '\n\n')
  test.errors.gamma.dadN[i] = test.error
}
plot(test.errors.gamma.dadN, type='l')
abline(h=mean.error.dadN, col='red', lwd=3)

models.cost.dadN = tune.cost(train.dadN, train.input)
test.errors.cost.dadN = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.cost.dadN[[i]], train.input)
  train.error = sum((y.pred.train - train$dadN)^2) / nrow(train)
  y.pred.test = predict(models.cost.dadN[[i]], test.input)
  test.error = sum((y.pred.test - test$dadN)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.dadN, '\n\n')
  test.errors.cost.dadN[i] = test.error
}
plot(test.errors.cost.dadN, type='l')
abline(h=mean.error.dadN, col='red', lwd=3)

# beta

models.gamma.beta = tune.gamma(train.beta, train.input)
y.mean.beta = mean(test$beta)
mean.error.beta = sum((y.mean.beta - test$beta)^2) / nrow(test)
test.errors.gamma.beta = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.gamma.beta[[i]], train.input)
  train.error = sum((y.pred.train - train$beta)^2) / nrow(train)
  y.pred.test = predict(models.gamma.beta[[i]], test.input)
  test.error = sum((y.pred.test - test$beta)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.beta, '\n\n')
  test.errors.gamma.beta[i] = test.error
}
plot(test.errors.gamma.beta, type='l')
abline(h=mean.error.beta, col='red', lwd=3)

models.cost.beta = tune.cost(train.beta, train.input)
test.errors.cost.beta = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.cost.beta[[i]], train.input)
  train.error = sum((y.pred.train - train$beta)^2) / nrow(train)
  y.pred.test = predict(models.cost.beta[[i]], test.input)
  test.error = sum((y.pred.test - test$beta)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.beta, '\n\n')
  test.errors.cost.beta[i] = test.error
}
plot(test.errors.cost.beta, type='l')
abline(h=mean.error.beta, col='red', lwd=3)

# z

models.gamma.z = tune.gamma(train.cz, train.input)
y.mean.z = mean(test$change_in_z)
mean.error.z = sum((y.mean.z - test$change_in_z)^2) / nrow(test)
test.errors.gamma.z = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.gamma.z[[i]], train.input)
  train.error = sum((y.pred.train - train$change_in_z)^2) / nrow(train)
  y.pred.test = predict(models.gamma.z[[i]], test.input)
  test.error = sum((y.pred.test - test$change_in_z)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.z, '\n\n')
  test.errors.gamma.z[i] = test.error
}
plot(test.errors.gamma.z, type='l')
abline(h=mean.error.z, col='red', lwd=3)

models.cost.z = tune.cost(train.cz, train.input)
test.errors.cost.z = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.cost.z[[i]], train.input)
  train.error = sum((y.pred.train - train$change_in_z)^2) / nrow(train)
  y.pred.test = predict(models.cost.z[[i]], test.input)
  test.error = sum((y.pred.test - test$change_in_z)^2) / nrow(test)
  
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error.z, '\n\n')
  test.errors.cost.z[i] = test.error
}
plot(test.errors.cost.z, type='l')
abline(h=mean.error.z, col='red', lwd=3)
