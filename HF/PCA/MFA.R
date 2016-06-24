###############################
#### Multi Factor Analysis ####
###############################

library(FactoMineR)

colnames(KDD.train)[42]='outcome'
colnames(KDD.train)[43]='outcome.response'
KDD.train[43] <- ifelse(KDD.train$outcome == 'normal',0,1)

char.col = c(2,3,4,42)

#Finding binary columns to exclude from scaling
bin.col = data.frame(matrix(ncol = 4, nrow = dim(KDD.train)[2]-1))
colnames(bin.col) = c('col','unique','max','min')
for (i in 1:(dim(KDD.train)[2]-1)) {
  bin.col[i,1]=i
  bin.col[i,2]=length(unique(KDD.train[,i]))
  bin.col[i,3]=max(KDD.train[,i])
  bin.col[i,4]=min(KDD.train[,i])
}
bin.col = bin.col[bin.col$unique==2,1]

l=seq(1:ncol(KDD.train))[-c(char.col, bin.col)]

KDD.train2 = KDD.train[,c(char.col, bin.col, l)]

KDD.train2$protocol_type = as.factor(KDD.train2$protocol_type)
KDD.train2$service = as.factor(KDD.train2$service)
KDD.train2$flag = as.factor(KDD.train2$flag)
KDD.train2$outcome = as.factor(KDD.train2$outcome)

# Group1: char  4 n
# Group2: bin   5 c
# Group3: continuous non bin 34
set.seed(5)
index = sample(1:nrow(KDD.train2),0.01*nrow(KDD.train2))
df = KDD.train2[index,]#c(1:22,24:43)]
# df = df[-42]
# df = df[-c(1:3)]
res = MFA(df, group = c(4,5,34), 
          type = c('n','c','s'),
          name.group = c('char','bin','continuous'))
# res.adfm = DMFA(df, num.fact = 1, scale.unit = FALSE)
# res.famd = FAMD(df)






#################
#### Example ####
#################

data(wine)
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
           ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
           num.group.sup=c(1,6))
summary(res)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))


## Not run:
#### Confidence ellipses around categories per variable
plotellipses(res)
plotellipses(res,keepvar="Label") ## for 1 variable

#### Interactive graph
liste = plotMFApartial(res)
plot(res,choix="ind",habillage = "Terroir")

###Example with groups of categorical variables
data (poison)
MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
    name.group=c("desc","desc2","symptom","eat"),
    num.group.sup=1:2)

## End(Not run)