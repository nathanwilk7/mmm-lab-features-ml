# Set up data

rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('9-curvature-distance.csv', header=T)

# User parameters

#set.seed(351734895)
num.samples = 3000
num.folds = 5
costs = c(.1, 1, 10)
gammas = c(.0001, .001, .01)
epsilons = c(.01, .1, 1)

# costs = c(.0001, .001, .01, .1, 1, 10, 100, 1000)
# gammas = c(.0001, .001, .01, .1, 1, 10, 100, 1000)
# epsilons = c(.0001, .001, .01, .1, 1, 10, 100, 1000)

# Calculated parameters

num.test.samples = num.samples %/% num.folds
num.tune.samples = num.test.samples
num.train.samples = num.samples - (num.test.samples + num.tune.samples)

folds = 1:num.folds
theta.interval = pi / num.folds

num.models = length(costs) * length(gammas) * length(epsilons)

# We don't want this column because it's not numeric

cdata['on_crack_front'] = NULL

# Scale function

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

library(e1071)

# Store results

cv.tune.mses <- vector(mode = "list", length = num.folds)
cv.test.mses <- vector(mode = "list", length = num.folds)
cv.best.index <- vector(mode = "list", length = num.folds)
cv.to.plot <- vector(mode = "list", length = num.folds)

# The Cross Validation Loop

for (fold in folds)
{
  print(fold)
  
  # Calculate the beginning and end of the test section and get the test data
  test.theta.begin = fold * theta.interval - theta.interval
  test.theta.end = fold * theta.interval
  test.data = cdata[cdata[,'theta'] > test.theta.begin & cdata[,'theta'] < test.theta.end,]
  
  # Choose the tune section and get the tune data
  tune.section.id = sample(folds[-fold], 1)
  tune.theta.begin = tune.section.id * theta.interval - theta.interval
  tune.theta.end = tune.section.id * theta.interval
  tune.data = cdata[cdata[,'theta'] > tune.theta.begin & cdata[,'theta'] < tune.theta.end,]
  
  # Get the train data, which is everything that's not test/tune
  train.data = cdata[(cdata[,'theta'] < test.theta.begin & cdata[,'theta'] < tune.theta.begin |
                        cdata[,'theta'] < test.theta.begin & cdata[,'theta'] > tune.theta.end |
                        cdata[,'theta'] > test.theta.end & cdata[,'theta'] < tune.theta.begin |
                        cdata[,'theta'] > test.theta.end & cdata[,'theta'] > tune.theta.end),]
  
  # Sample from the data
  test.samples = test.data[sample(nrow(test.data), num.test.samples), ]
  tune.samples = tune.data[sample(nrow(tune.data), num.tune.samples), ]
  train.samples = train.data[sample(nrow(train.data), num.train.samples), ]
  
  # Scale the sampled data
  test.samples.scaled = my.scale(test.samples)
  tune.samples.scaled = my.scale(tune.samples)
  train.samples.scaled = my.scale(train.samples)
  
  # Set up the training data
  train.input = data.frame(train.samples.scaled$distance_to_grain_boundary, train.samples.scaled$unit_vector_to_grain_boundary_x, train.samples.scaled$unit_vector_to_grain_boundary_y, train.samples.scaled$unit_vector_to_grain_boundary_z, 
                           train.samples.scaled$phi1, train.samples.scaled$Phi, train.samples.scaled$phi2, train.samples.scaled$misorientation, train.samples.scaled$scaled_angle, train.samples.scaled$previous_dadN, train.samples.scaled$curvature_distance)
  colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle', 'previous_dadN', 'curvature_distance')
  train.dadN = train.samples.scaled$dadN
  train.beta = train.samples.scaled$beta
  train.cz = train.samples.scaled$change_in_z
  
  # Set up the tuning data
  tune.input = data.frame(tune.samples.scaled$distance_to_grain_boundary, tune.samples.scaled$unit_vector_to_grain_boundary_x, tune.samples.scaled$unit_vector_to_grain_boundary_y, tune.samples.scaled$unit_vector_to_grain_boundary_z, 
                          tune.samples.scaled$phi1, tune.samples.scaled$Phi, tune.samples.scaled$phi2, tune.samples.scaled$misorientation, tune.samples.scaled$scaled_angle, tune.samples.scaled$previous_dadN, tune.samples.scaled$curvature_distance)
  colnames(tune.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle', 'previous_dadN', 'curvature_distance')
  tune.dadN = tune.samples.scaled$dadN
  tune.beta = tune.samples.scaled$beta
  tune.cz = tune.samples.scaled$change_in_z
  
  # Set up the testing data
  test.input = data.frame(test.samples.scaled$distance_to_grain_boundary, test.samples.scaled$unit_vector_to_grain_boundary_x, test.samples.scaled$unit_vector_to_grain_boundary_y, test.samples.scaled$unit_vector_to_grain_boundary_z, 
                          test.samples.scaled$phi1, test.samples.scaled$Phi, test.samples.scaled$phi2, test.samples.scaled$misorientation, test.samples.scaled$scaled_angle, test.samples.scaled$previous_dadN, test.samples.scaled$curvature_distance)
  colnames(test.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle', 'previous_dadN', 'curvature_distance')
  test.dadN = test.samples.scaled$dadN
  test.beta = test.samples.scaled$beta
  test.cz = test.samples.scaled$change_in_z
  
  models <- vector(mode = "list", length = num.models)
  i = 1
  
  print('Creating Models')
  for (my.cost in costs)
  {
    for (my.gamma in gammas)
    {
      for (my.epsilon in epsilons)
      {
        model <- svm(train.dadN~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+phi1+phi2+misorientation+scaled_angle+previous_dadN+curvature_distance, data=train.input, kernel ="radial", 
                     cost=my.cost, gamma=my.gamma, epsilon=my.epsilon)
        models[[i]] = model
        i = i + 1
      }
    }
  }
  
  train.mse.dadN <- vector(mode = "list", length = num.models)
  tune.mse.dadN <- vector(mode = "list", length = num.models)
  
  minimum.tune.mse = 999
  minimum.index = 1
  
  print('Evaluating Models')
  for (i in 1:num.models)
  {
    train.pred.dadN = predict(models[[i]], train.input)
    train.mse = sum((train.pred.dadN - train.dadN)^2) / length(train.dadN)
    tune.pred.dadN = predict(models[[i]], tune.input)
    tune.mse = sum((tune.pred.dadN - tune.dadN)^2) / length(tune.dadN)
    
    train.mse.dadN[i] = train.mse
    tune.mse.dadN[i] = tune.mse
    
    if (tune.mse < minimum.tune.mse)
    {
      minimum.tune.mse = tune.mse
      minimum.index = i
    }
  }
  # Save the best error and index of parameters
  cv.tune.mses[fold] = minimum.tune.mse
  cv.best.index[fold] = minimum.index
  #cv.to.plot[[fold]] = matrix(tune.mse.dadN, nrow=length(gammas), ncol=length(costs))
  
  test.pred.dadN = predict(models[[minimum.index]], test.input)
  test.mse.dadN = sum((test.pred.dadN - test.dadN)^2) / length(test.dadN)
  cv.test.mses[fold] = test.mse.dadN
}

# Print Test MSEs

my.sum = 0
for (i in 1:length(cv.test.mses))
{
  my.sum = my.sum + cv.test.mses[[i]]
}
result = my.sum / length(cv.test.mses)
print(result)

# Print all Test MSE

for (i in 1:length(cv.test.mses))
{
  print(cv.test.mses[[i]])
}

#library(lattice)

#sqrt(unlist(cv.test.mses))
#levelplot(cv.to.plot[[5]], at=c(.85,.9,.95,1,1.05,1.1), xlab='Gammas', ylab='Costs')
