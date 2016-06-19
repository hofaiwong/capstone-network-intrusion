#################################################
####       Network Intrusion Detection       ####
#### Logistic Regression with Lasso - GLMNET ####
#################################################

library(glmnet)
library(car)
library(MASS)

#Creating the data matrices for the glmnet() function.
x = model.matrix(outcome.response ~ ., subset(new.KDD.train, select=-c(num_outbound_cmds)))[, -1]
y = new.KDD.train$outcome.response


#Creating training and test sets
set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)


#Fitting the logistic regression on a grid of lambda. Alpha = 1 for lasso penalty
grid = 10^seq(0, -5, length = 200)
logit.models = glmnet(x[train, ], y[train], 
                      alpha = 1, 
                      lambda = grid, 
                      family="binomial")
plot(logit.models, 
     xvar = "lambda", 
     label = TRUE, 
     main = "Logistic Regression with Lasso penalty\n")


#Cross-validation
set.seed(0)
logit.cv = cv.glmnet(x[train, ], y[train], 
                     alpha = 1,            #Lasso penalty
                     nfolds = 5,           #5-fold CV
                     type.measure='class', #Misclassification measure
                     family="binomial",    #Logistic regression
                     lambda = grid)
plot(logit.cv, main = "Logistic Regression with Lasso penalty\n")


#Best lambda
logit.cv$lambda.min #1.122668e-05
log(logit.cv$lambda.min) #-11.39722
lambda = exp(-3) #lamdba.min still keeps ~110 features. Need to balance complexity and accuracy


#Checking accuracy of model
logit.test.class = predict(logit.cv, 
                           s = lambda, 
                           type = 'class',
                           newx = x[-train, ])
t = table(truth = y[-train], prediction = logit.test.class)
t
sum(diag(t))/sum(t) #94.5% accuracy with lambda=exp(-3); 97.6% with lambda.min


#Coefficients: 
logit.coef = predict(logit.cv, 
                     s = lambda, 
                     type = 'coefficients')
sum(logit.coef!=0)-1 #Kept only 12 features with lambda=exp(-3); 110 with lambda.min
logit.coef #log odds scale?
logit.nonzero = predict(logit.cv, 
                        s = lambda, 
                        type = 'nonzero')
colnames(x)[logit.nonzero[,1]]


#Plot lambda vs coefficients vs accuracy
res = data.frame(matrix(ncol = 3, nrow = length(grid)))
colnames(res) = c('lambda','accuracy','coef')
for (i in 1:length(grid)) {
  #Insert lambda
  res[i,1] = grid[i]
  
  #Insert accuracy %
  pred.class = predict(logit.cv, s = grid[i], type = 'class', newx = x[-train, ])
  t = table(truth = y[-train], prediction = pred.class)
  res[i,2] = sum(diag(t))/sum(t)
  
  #Insert coef count
  pred.coef = predict(logit.cv, s = grid[i], type = 'coefficients')
  res[i,3] = sum(pred.coef!=0)-1
}
plot(res$coef, res$accuracy, main = "Model accuracy by number of features",
     xlab="Count of features", ylab="Accuracy", pch=16)


###############
#### TO DO ####
###############

# #Goodness of fit test i.e. Test of deviance
# # H0: The logistic regression model is appropriate.
# # H1: The logistic regression model is not appropriate.
# pchisq(logit.overall$deviance, logit.overall$df.residual, lower.tail = FALSE)
# 
# #McFadden's pseudo R^2 based on the deviance
# 1 - logit.overall$deviance/logit.overall$null.deviance
# 
# #Checking the model summary and assumptions
# summary(logit)
# plot(logit)
# influencePlot(logit)
# vif(logit.models)
# avPlots(logit)
# confint(logit)