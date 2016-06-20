#################################################
####       Network Intrusion Detection       ####
#### Logistic Regression with Lasso - GLMNET ####
#################################################

library(glmnet)
library(car)
library(MASS)

#Define accuracy function
accuracy = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat(sum(diag(contingency))/sum(contingency),'\n')
  return(contingency)
}

#Creating the data matrices for the glmnet() function.
x = model.matrix(outcome.response ~ ., new.KDD.train)[, -1]
y = new.KDD.train$outcome.response
x.test = model.matrix(outcome.response ~ ., new.KDD.test)[, -1]
y.test = new.KDD.test$outcome.response

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


#Checking accuracy of model - train data, test subset
logit.test.class = predict(logit.cv, 
                           s = lambda, 
                           type = 'class',
                           newx = x[-train, ])
accuracy(y[-train], logit.test.class) #94.5% accuracy with lambda=exp(-3); 97.6% with lambda.min


#Checking accuracy of model - test data
logit.test.class.final = predict(logit.cv, 
                                 s = lambda, 
                                 type = 'class',
                                 newx = x.test)
accuracy(y.test, logit.test.class.final) #76% accuracy with lambda=exp(-3)


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

#####################
#### Lambda Plot ####
#####################

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
rm(i,t)

#####################
#### Diagnostics ####
#####################

#formula = as.formula(paste("outcome.response ~", paste(colnames(x)[logit.nonzero[,1]], collapse = " + ")))
temp = colnames(x)[logit.nonzero[,1]]
temp = temp[-6] #remove "wrong_fragment" which had high p-value
formula = as.formula(paste("outcome.response ~", paste(temp, collapse = " + ")))

logit.glm = glm(formula, 
                family = "binomial", 
                data = new.KDD.train)

#Goodness of fit test i.e. Test of deviance
# H0: The logistic regression model is appropriate.
# H1: The logistic regression model is not appropriate.
pchisq(logit.glm$deviance, logit.glm$df.residual, lower.tail = FALSE) #p-value of 1 > 0.05 cutoff so we fail to reject null hypothesis; model is appropriate

#McFadden's pseudo R^2 based on the deviance
1 - logit.glm$deviance/logit.glm$null.deviance #~84%

#Checking the model summary and assumptions
summary(logit.glm)
plot(logit.glm)
library(car)
influencePlot(logit.glm)
vif(logit.glm)
avPlots(logit.glm)
confint(logit.glm)