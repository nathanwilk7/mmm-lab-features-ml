rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
#cdata = read.csv('2-FINAL_TEST-nearest-grain-boundary.csv', header=T)
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

set.seed(7)
num_samples = 5000
temp = small.data[sample(nrow(small.data), num_samples), ]

train.b = temp[temp[, 'theta'] < 2.09,]
train = my.scale(train.b)
train.input = data.frame(train$distance_to_grain_boundary, train$unit_vector_to_grain_boundary_x, train$unit_vector_to_grain_boundary_y, train$unit_vector_to_grain_boundary_z
                         , train$phi1, train$Phi, train$phi2, train$misorientation, train$scaled_angle)
colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
train.dadN = train$dadN
train.beta = train$beta
train.cz = train$change_in_z

test.b = temp[temp[, 'theta'] >= 2.09,]
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

models.gamma = tune.gamma(train.dadN, train.input)
test.errors = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.gamma[[i]], train.input)
  train.error = sum((y.pred.train - train$dadN)^2) / nrow(train)
  y.pred.test = predict(models.gamma[[i]], test.input)
  test.error = sum((y.pred.test - test$dadN)^2) / nrow(test)
  y.mean = mean(test$dadN)
  mean.error = sum((y.mean - test$dadN)^2) / nrow(test)
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error, '\n\n')
  test.errors[i] = test.error
}
plot(test.errors, type='l')
abline(h=mean.error, col='red', lwd=3)

models.cost = tune.cost(train.dadN, train.input)
test.errors = c(1,2,3,4,5,6,7)
for (i in 1:7)
{
  y.pred.train = predict(models.cost[[i]], train.input)
  train.error = sum((y.pred.train - train$dadN)^2) / nrow(train)
  y.pred.test = predict(models.cost[[i]], test.input)
  test.error = sum((y.pred.test - test$dadN)^2) / nrow(test)
  y.mean = mean(test$dadN)
  mean.error = sum((y.mean - test$dadN)^2) / nrow(test)
  cat('Model Number: ', i, '\n')
  cat('Train Error: ', train.error, '\n')
  cat('Test Error: ', test.error, '\n')
  cat('Mean Error: ', mean.error, '\n\n')
  test.errors[i] = test.error
}
plot(test.errors, type='l')
abline(h=mean.error, col='red', lwd=3)
