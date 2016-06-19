library(glmnet)

#Creating the data matrices for the glmnet() function.
x = model.matrix(outcome.response ~ ., new.KDD.train)[, -1]
y = new.KDD.train$outcome.response

#Creating training and test sets with an 80-20 split, respectively.
set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)

#Fitting the logistic regression on a grid of lambda. Alpha = 1 for lasso penalty
grid = 10^seq(-1, -6, length = 200)
logit.models = glmnet(x[train, ], y[train], 
                      alpha = 1, 
                      lambda = grid, 
                      family="binomial")
plot(logit.models, 
     xvar = "lambda", 
     label = TRUE, 
     main = "Logistic Regression with Lasso penalty")

#Cross-validation
set.seed(0)
logit.cv = cv.glmnet(x[train, ], y[train], 
                     alpha = 1, 
                     nfolds = 5, 
                     type.measure='deviance', 
                     lambda = grid)

plot(logit.cv, main = "Logistic Regression with Lasso penalty\n")

bestlambda = logit.cv$lambda.min
bestlambda
log(bestlambda)

logit.bestlambdatrain = predict(logit.models, 
                                s = bestlambda, 
                                newx = x[-train, ])

#8
ridge.out = glmnet(x, y, alpha = 0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)

#The coefficients have been shrunken towards 0. Most notably, it appears as though
#the pgg45 veriable has been reduced the most, with a coefficient of only about
#0.0031. On the other hand, the svi, lweight, and lcavol have the highest
#coefficient values (all greater than 0.46), indicating that they are the stronger
#predictors. NB: We cannot interpret these values directly because of the nature
#of the regularization process.

#9
ridge.bestlambda = predict(ridge.out, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)

#The overall MSE is about 0.4550; this is similar to the MSE we found when trying
#to predict on the test data, but is a bit smaller. In general, the test error and
#the cross-validation error curve helps us assess how we might expect our model
#to perform in future predictive scenarios. The overall error rate calculated
#on the data as a whole is often an underestimation simply because it incorporates
#the data in the construction of the model in the first place.



#####################
#####Question #3#####
#####################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#The coefficients all seem to shrink towards 0 as lambda gets quite large. All
#coefficients seem to go to 0 once the log lambda value gets to about 0. We
#note that in the lasso regression scenario, coefficients are necessarily set
#to exactly 0.

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#The error seems to be reduced with a log lambda value of around -3.3027; this
#corresponts to a lambda value of about 0.038. This is the value of lambda
#we should move forward with in our future analyses.

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

#The test MSE is about 0.506.

lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

#The coefficients have been shrunken towards 0; most notably, the lcp variable
#has dropped out first and has a coefficient of exactly 0. Other variables like
#gleason, pgg45, and age have pretty small coefficient values as well. Similar
#to the ridge regression scenario, the svi, lweight, and lcavol all have the
#largest coefficient estimates.

lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

#The overall MSE is about 0.45996. Again, this is similar to the test MSE we
#found above, but a little bit lower because of the way in which the model was
#fit using the data at hand.

predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
mean((ridge.bestlambda - y)^2)
mean((lasso.bestlambda - y)^2)

#Both models appear to perform in a similar manner. Both the test MSEs and the
#overall MSEs were approximately the same. Ultimately, the ridge regression MSE
#was slightly lower than that of the lasso. Although this might be due to random
#variability among our dataset, if we are strictly talking about predictive
#power, we might choose to move forth with the ridge regression in this scenario.
#On the other hand, the final lasso regression model "kicked out" the lcp
#variable altogether (setting its coefficient value to 0). If we are particularly
#interested in dimension reduction and variable selection, we might choose to
#move forth with the lasso regression.