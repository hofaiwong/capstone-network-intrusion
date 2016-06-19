library(LiblineaR)

set.seed(0)
new.KDD.train2 = new.KDD.train[,-101]
train.index = sample(1:nrow(new.KDD.train), 7*nrow(new.KDD.train)/10)
xTrain = new.KDD.train2[train.index, 1:121]
yTrain = new.KDD.train2[train.index, 122]
xTest = new.KDD.train2[-train.index, 1:121]
yTest = new.KDD.train2[-train.index, 122]

# Center and scale data
s=scale(xTrain,center=TRUE,scale=TRUE)

# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes=6
tryCosts=seq(0.001, 1000, length.out = 100)
bestCost=NA
bestAcc=0
# bestType=NA

LiblineaR(data=s,target=yTrain,type=6,cost=1000,bias=TRUE,cross=1,verbose=FALSE)

for(ty in tryTypes){
  for(co in tryCosts){
    acc=LiblineaR(data=s,target=yTrain,type=ty,cost=co,bias=TRUE,cross=5,verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost=co
      bestAcc=acc
      # bestType=ty
    }
  }
}
# cat("Best model type is:",bestType,"\n")
cat("Best cost is:",bestCost,"\n")
cat("Best accuracy is:",bestAcc,"\n")


# Re-train best model with best cost value.
m=LiblineaR(data=s,target=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)
# Scale the test data
s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
# Make prediction
pr=FALSE
if(bestType==0 || bestType==7) pr=TRUE
p=predict(m,s2,proba=pr,decisionValues=TRUE)
# Display confusion matrix
res=table(p$predictions,yTest)
print(res)
# Compute Balanced Classification Rate
BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
print(BCR)


#' #############################################
# Example of the use of a sparse matrix:
library(SparseM)
if(require(SparseM)){
  # Sparsifying the iris dataset:
  iS=apply(iris[,1:4],2,function(a){a[a<quantile(a,probs=c(0.25))]=0;return(a)})
  irisSparse<-as.matrix.csr(iS)
  # Applying a similar methodology as above:
  xTrain=irisSparse[train,]
  xTest=irisSparse[-train,]
  # Re-train best model with best cost value.
  m=LiblineaR(data=xTrain,target=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)
  # Make prediction
  p=predict(m,xTest,proba=pr,decisionValues=TRUE)
  # Display confusion matrix
  res=table(p$predictions,yTest)
  print(res)
}