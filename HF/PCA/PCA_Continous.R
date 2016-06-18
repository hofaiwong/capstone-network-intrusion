###########################
#### PCA on continuous ####
###########################
library(psych)

#Extract only continuous variables
KDD.train.continuous = KDD.train[,-c(42,43)] #remove outcomes
index_discrete = c(2,3,4,7,12,14,15,21,22,42)
#index_discrete = c('protocol_type','service','flag','land','logged_in','root_shell','su_attempted','is_hot_login','is_guest_login','outcome_response')
KDD.train.continuous = KDD.train.continuous[,-c(index_discrete,20)]

#Select number of PCs: 9 ideal
fa.parallel(KDD.train.continuous, fa = "pc", n.iter = 100) 
abline(h = 1) 

#Carry out PCA
pc_KDD.train.continuous = principal(KDD.train.continuous, nfactors = 9, rotate = "none") #Can change nfactors
pc_KDD.train.continuous
factor.plot(pc_KDD.train.continuous, labels = colnames(KDD.train.continuous))
