###############################
#### Multi Factor Analysis ####
###############################

library(FactoMineR)
library(ggplot2)

#------------- Prep train for FactoMineR ----------------------------#
char.col = c(2,3,4,42)

prep.pca = function(df) {
  colnames(df)[42]='outcome'
  colnames(df)[43]='outcome.response'
  df[43] <- ifelse(df$outcome == 'normal',0,1)
  
  #Finding binary columns
  bin.col = data.frame(matrix(ncol = 4, nrow = dim(df)[2]))
  colnames(bin.col) = c('col','unique','max','min')
  for (i in 1:(dim(df)[2])) {
    bin.col[i,1]=i
    bin.col[i,2]=length(unique(df[,i]))
    bin.col[i,3]=max(df[,i])
    bin.col[i,4]=min(df[,i])
  }
  bin.col = bin.col[bin.col$unique==2,1]
  
  l=seq(1:ncol(df))[-c(char.col, bin.col)]
  
  df$protocol_type = as.factor(df$protocol_type)
  df$service = as.factor(df$service)
  df$flag = as.factor(df$flag)
  df$outcome = as.factor(df$outcome)
  return(df)
}
KDD.train2 = prep.pca(KDD.train)
KDD.test2 = prep.pca(KDD.test)


#------------- PCA function in FactoMineR ----------------------------#
#Train
pca.train = PCA(KDD.train2, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
                quanti.sup = c(43), 
                quali.sup = char.col, 
                graph = TRUE, axes = c(1,2))
summary(pca.train)
dimdesc(pca.train, axes=c(1,2))
pca.train$quali.sup
pca.train$quanti.sup
draw.train = as.data.frame(pca.test$quali.sup$coord[,1:2])
draw.train$attack = row.names(draw.train)
#Draw plot
ggplot(draw.train,aes(Dim.1, Dim.2)) + 
  geom_text(label=draw.train$attack)
barplot(pca.train$eig[,2], 
        main = "Eigenvalues",
        names.arg = paste("Dim", 1:nrow(pca.train$eig), sep = ""))
plot(pca.train, choix = "var", axes = c(3, 4), lim.cos2.var = 0)


#Test
pca.test = PCA(KDD.test2, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
               quanti.sup = c(43), 
               quali.sup = char.col, 
               graph = TRUE, axes = c(1,2))
summary(pca.test)
dimdesc(pca.test, axes=c(1,2))
pca.test$quali.sup
pca.test$quanti.sup
draw.test = as.data.frame(pca.test$quali.sup$coord[,1:2])
draw.test$attack = row.names(draw.test)
#Draw plot
ggplot(draw.test,aes(Dim.1, Dim.2)) + 
  geom_text(label=draw.test$attack)
barplot(pca.test$eig[,2], 
        main = "Eigenvalues",
        names.arg = paste("Dim", 1:nrow(pca.test$eig), sep = ""))

write.csv(pca.train$var$cor, file='pca_train_cont_cor.csv')
write.csv(pca.test$var$cor, file='pca_test_cont_cor.csv')

#------------- MFA function in FactoMineR ----------------------------#
res = MFA(df, group = c(4,5,34), 
          type = c('n','c','s'),
          name.group = c('char','bin','continuous'))
# res.adfm = DMFA(df, num.fact = 1, scale.unit = FALSE)
# res.famd = FAMD(df)