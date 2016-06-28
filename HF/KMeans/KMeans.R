KDD.train2 = as.data.frame(scale(KDD.train[,c(1,5:41)]))
KDD.train2 = KDD.train2[,-c(17)]


#-------------K-Means using default stats package ------------------------#
#Index of binary columns
bin.col = data.frame(matrix(ncol = 4, nrow = dim(KDD.train2)[2]))
colnames(bin.col) = c('col','unique','max','min')
for (i in 1:(dim(KDD.train2)[2])) {
  bin.col[i,1]=i
  bin.col[i,2]=length(unique(KDD.train2[,i]))
  bin.col[i,3]=max(KDD.train2[,i])
  bin.col[i,4]=min(KDD.train2[,i])
}
bin.col = bin.col[bin.col$unique==2,1]

KDD.train2 = KDD.train2[,-c(17, bin.col)]

wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}

wssplot(KDD.train2)

set.seed(0)
km.train = kmeans(KDD.train2, centers = 3, nstart = 100)

# #Visually & numerically inspecting the results.
# par(mfrow = c(2, 3))
# plot(KDD.train2, col = km.train$cluster,
#      main = paste("Best K-Means Attempt out of 50\n WCV: ",
#                   round(km.train$tot.withinss, 4)))

km.train

KDD.train3 = cbind(KDD.train, as.factor(km.train$cluster))
colnames(KDD.train3)[42]='outcome'
colnames(KDD.train3)[44]='cluster'

table(KDD.train3$cluster)

library(ggplot2)

#Sort factors
f_table <- table(KDD.train3$outcome)
f_levels <- names(f_table)[order(f_table)]
KDD.train3$outcome2 <- factor(KDD.train3$outcome, levels = f_levels)


ggplot(KDD.train3, aes(outcome2)) + 
  geom_bar(aes(fill=cluster)) + 
  coord_flip() +
  labs(title="Connection/Attack type by cluster",
       x="Connection/Attack type",
       y="Connections") +
  theme_bw()