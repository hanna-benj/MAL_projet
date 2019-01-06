rm(list=ls())
X = data.frame(read.table("/home/hanna_benj/Documents/semestre5/MAL/MAL_projet/caesarian.csv", sep = ",", header = TRUE))
X$Caesarian <- as.character(X$Caesarian)
X$Caesarian <- as.factor(X$Caesarian)
library(naivebayes)
library(MASS)
library(randomForest)
library(ipred)
library(tree)
library(ROCR)
library(DAAG)
plot(X)
boxplot(X)
#tree

modtree=tree(Caesarian~.,data=X)
plot(modtree)
text(modtree)

#faire attention aux codage des variables, coder le 1,2,3,4 des enfants en faisant une colone par enfants au lieu de une colone avec 1234. il y a des package qui le fond. fonction demi coding library nnet
#class.ind(X$Delivery.number)
k=4
n = dim(X)[1]
pas = floor(n/k)
er1 =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  
  resSim = tree(Caesarian~.,data = tabSim)
  pred = predict(resSim, newdata = tabTest)
  difference= table(pred>0.5,tabTest$Caesarian)
  er1[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(er1)
boxplot(er1)
p = prediction(pred,tabTest$Caesarian)
perf = performance(p,"tpr","fpr")
plot(perf)

# Random Forest
modFor2 = randomForest(Caesarian~.,data = tabSim)
pred4 = predict(modFor2,newdata = tabTest, type ="prob")
p3 = prediction(pred4[,2],spam)
perf3 = performance(p3,"tpr","fpr")
plot(perf3, col = "red")

#Bagging
modbag = bagging(spam~.,data=tabSim,coob=TRUE)
pred3 = predict(modbag,newdata = tabTest, type ="prob")
p2 = prediction(pred3[,2],spam)
perf2 = performance(p2,"tpr","fpr")
plot(perf2, col = "blue")

