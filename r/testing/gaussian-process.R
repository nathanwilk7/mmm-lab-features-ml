rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('6-beta.csv', header=T)

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
temp = cdata[sample(nrow(cdata), num_samples), ]

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

