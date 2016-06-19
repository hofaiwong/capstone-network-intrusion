library(car)
library(MASS)

set.seed(0)
new.KDD.train = new.KDD.train[,-101]
new.KDD.train$outcome.response = as.factor(new.KDD.train$outcome.response)
train.index = sample(1:nrow(new.KDD.train), 7*nrow(new.KDD.train)/10)
df.train = new.KDD.train[train.index, ]
df.test = new.KDD.train[-train.index, ]

#AIC/BIC/Stepwise
model.empty = glm(outcome.response ~ 1, family = "binomial", data = df.train) #The model with an intercept ONLY.
model.full = glm(outcome.response ~ ., family = "binomial", data = df.train) #The model with ALL variables.
model.interim = glm(outcome.response ~ duration + aol + auth + bgp + courier + csnet_ns + 
                      ctf + daytime + discard + domain + domain_u + echo + eco_i + 
                      ecr_i + efs + exec + finger + ftp + gopher + hostnames + 
                      http + http_443 + imap4 + IRC + iso_tsap + klogin + kshell + 
                      ldap + link + login + mtp + name + netbios_dgm + netbios_ns + 
                      netbios_ssn + netstat + nnsp + nntp + ntp_u + pm_dump + pop_2 + 
                      pop_3 + printer + private + red_i + remote_job + rje + shell + 
                      smtp + sql_net + ssh + sunrpc + supdup + systat + telnet + 
                      tftp_u + tim_i + time + urh_i + urp_i + uucp + uucp_path + 
                      vmnet + whois + X11 + Z39_50 + tcp + OTH + REJ + RSTO + RSTOS0 + 
                      RSTR + S0 + S1 + S2 + S3 + SF + src_bytes + dst_bytes + land + 
                      wrong_fragment + urgent + hot + num_failed_logins + logged_in + 
                      num_compromised + root_shell + su_attempted + num_root + 
                      num_file_creations + num_shells + num_access_files + is_guest_login + 
                      count + srv_count + serror_rate + srv_serror_rate + rerror_rate + 
                      srv_rerror_rate + same_srv_rate + diff_srv_rate + srv_diff_host_rate + 
                      dst_host_count + dst_host_srv_count + dst_host_same_srv_rate + 
                      dst_host_diff_srv_rate + dst_host_same_src_port_rate + dst_host_srv_diff_host_rate + 
                      dst_host_serror_rate + dst_host_srv_serror_rate + dst_host_rerror_rate + 
                      dst_host_srv_rerror_rate,
                    family = "binomial", data = df.train) #The model after partial both AIC

scope = list(lower = formula(model.empty), upper = formula(model.full))

backwardBIC = step(model.full, scope, direction = "backward", k = log(nrow(df.train)))

#Checking the model summary and assumptions of the reduced model.
summary(bothAIC.full)
plot(bothAIC.full)
influencePlot(bothAIC.full)
vif(bothAIC.full)
avPlots(bothAIC.full)
confint(bothAIC.full)


#Hypothesis test
# H0: The logistic regression model is appropriate.
# H1: The logistic regression model is not appropriate.

pchisq(logit.overall$deviance, logit.overall$df.residual, lower.tail = FALSE)

logit.overall$coefficients
exp(logit.overall$coefficients)
confint.default(logit.overall)
exp(confint.default(logit.overall))

#9
logit.noBK = glm(LC ~ . - BK, family = "binomial", data = case2002)
pchisq(logit.noBK$deviance, logit.noBK$df.residual, lower.tail = FALSE)
pchisq(logit.noBK$deviance - logit.overall$deviance,
       logit.noBK$df.residual - logit.overall$df.residual,
       lower.tail = FALSE)
anova(logit.noBK, logit.overall, test = "Chisq")

#The p-value for the drop in deviance test is < 0.001, which is quite significant.
#We reject the null hypothesis that the coefficient for the birdkeeping variable
#is 0, and conclude that having it in the model should provide a better fit.

#12
logit.justBKYR = glm(LC ~ BK + YR, family = "binomial", data = case2002)
pchisq(logit.justBKYR$deviance - logit.overall$deviance,
       logit.justBKYR$df.residual - logit.overall$df.residual,
       lower.tail = FALSE)
anova(logit.justBKYR, logit.overall, test = "Chisq")

#14abcd
AIC(logit.overall, logit.noBK, logit.justBKYR)
BIC(logit.overall, logit.noBK, logit.justBKYR)
1 - logit.overall$deviance/logit.overall$null.deviance
1 - logit.noBK$deviance/logit.noBK$null.deviance
1 - logit.justBKYR$deviance/logit.justBKYR$null.deviance


#15ab
newdata = with(case2002, data.frame(YR = mean(YR),
                                    BK = factor(c("Bird", "NoBird"))))
cbind(newdata, "Prob. Lung Cancer" = predict(logit.justBKYR, newdata, type = "response"))

newdata = data.frame(YR = 0, BK = factor(c("Bird", "NoBird")))
cbind(newdata, "Prob. Lung Cancer" = predict(logit.justBKYR, newdata, type = "response"))

LC.predicted = round(logit.justBKYR$fitted.values)
table(truth = case2002$LC, prediction = LC.predicted)
(85+22)/147
98/147