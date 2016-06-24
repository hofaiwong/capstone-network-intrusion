####################
#### FactoMineR ####
####################

library(FactoMineR)

#### PCA on KDD.train
KDD.train2 = KDD.train[,-c(42,43)]
#index_discrete = c('protocol_type','service','flag','land','logged_in','root_shell','su_attempted','is_hot_login','is_guest_login','outcome_response')
index_discrete = c(2,3,4,7,12,14,15,21,22)
res.pca <- PCA(KDD.train2,
               quali.sup = index_discrete)

## plot of the eigenvalues
barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(res.pca)
plot(res.pca,choix="ind",habillage=42)
dimdesc(res.pca, axes = 1:2)
## To draw ellipses around the categories of the 42nd variable (which is categorical)
plotellipses(res.pca,42)


#### PCA on new.KDD.train
res.new.pca <- PCA(new.KDD.train, 
                   quali.sup = 123,
                   graph=FALSE)

barplot(res.new.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.new.pca$eig))
abline(h=1)
summary(res.pca)
#plot(res.new.pca,choix="ind",habillage=123)
dimdesc(res.new.pca, axes = 1:2)
plotellipses(res.pca,123)