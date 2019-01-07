rm(list=ls())
X = data.frame(read.table("/home/bouchaud/Documents/MAL/Projet/MAL_projet/caesarian.csv", sep = ",", header = TRUE))
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
k=3
n = dim(X)[1]
pas = floor(n/k)
er1 =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  resSim = tree(Caesarian~.,data = tabSim)
  pred1 = predict(resSim, newdata = tabTest)
  difference= table(pred1[,2]>0.5,tabTest$Caesarian)
  er1[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(er1)
boxplot(er1)
p = prediction(pred1[,2],tabTest$Caesarian)
perf = performance(p,"tpr","fpr")
plot(perf)

# Random Forest
k=3
n = dim(X)[1]
pas = floor(n/k)
erRF =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modFor2 = randomForest(Caesarian~.,data = tabSim)
  pred4 = predict(modFor2,newdata = tabTest, type ="prob")
  difference= table(pred4[,2]>0.5,tabTest$Caesarian)
  erRF[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erRF)
boxplot(erRF)
modFor2 = randomForest(Caesarian~.,data = tabSim)
pred4 = predict(modFor2,newdata = tabTest, type ="prob")
p3 = prediction(pred4[,2],tabTest$Caesarian)
perf3 = performance(p3,"tpr","fpr")
plot(perf3, col = "red")

#Bagging
k=3
n = dim(X)[1]
pas = floor(n/k)
erBag =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modbag = bagging(Caesarian~.,data = tabSim,coob=TRUE)
  pred3 = predict(modbag,newdata = tabTest, type ="prob")
  difference= table(pred3[,2]>0.5,tabTest$Caesarian)
  erBag[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erBag)
boxplot(erBag)
modbag = bagging(Caesarian~.,data = tabSim,coob=TRUE)
pred3 = predict(modbag,newdata = tabTest, type ="prob")
p2 = prediction(pred3[,2],tabTest$Caesarian)
perf2 = performance(p2,"tpr","fpr")
plot(perf2, col = "blue")

#Comparaison des modèles
boxplot(er1,erRF,erBag, names = c('CART','RFOREST', "BAGGING"),cex = 0.5)
plot(perf)
plot(perf2, add = TRUE, col = "blue")
plot(perf3, add = TRUE, col = "red")
plot(perf4, add = TRUE, col = "green")
plot(perf5, add = TRUE, col = "orange")
plot(perf6, add = TRUE, col = "brown")
plot(perf7, add = TRUE, col = "purple")
legend(0.6,0.8,legend=c("CART","Bagging", "Random Forest", "LDA", "QAD", "Logistic Regression", "Bayes"), col=c("black","blue","red","green","orange","brown","purple"),lty=1, cex=0.8, ncol=1)

#-----

library(nnet)
delivery = class.ind(X$Delivery.number)
X2 = X[,-2]
X2 = cbind(X2,delivery)
names(X2)[6]<-"One.Child"
names(X2)[7]<-"Two.Children"
names(X2)[8]<-"Three.Children"
names(X2)[9]<-"Four.Children"

# Tree
k=3
n = dim(X2)[1]
pas = floor(n/k)
er2 =c(1:k)
for (i in 1:k){
  tabSimSplit = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestSplit = X2[((i-1)*pas):(i*pas),] #Données de test
  resSimSplit = tree(Caesarian~.,data = tabSimSplit)
  pred2 = predict(resSimSplit, newdata = tabTestSplit)
  difference2= table(pred2[,2]>0.5,tabTestSplit$Caesarian)
  er2[i] = (difference2[2] + difference2[3]) / length(tabTestSplit[,1]) #Erreur totaletabSim
}
m = mean(er2)
boxplot(er2)
p2 = prediction(pred2[,2],tabTestSplit$Caesarian)
perf6 = performance(p2,"tpr","fpr")

# Random Forest
k=3
n = dim(X2)[1]
pas = floor(n/k)
erRFSplit =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestRFSplit = X2[((i-1)*pas):(i*pas),] #Données de test
  modForRF = randomForest(Caesarian~.,data = tabSim)
  pred5 = predict(modForRF,newdata = tabTestRFSplit, type ="prob")
  difference= table(pred5[,2]>0.5,tabTestRFSplit$Caesarian)
  erRFSplit[i] = (difference[2] + difference[3]) / length(tabTestRFSplit[,1]) #Erreur totaletabSim
}
m = mean(erRFSplit)
boxplot(erRFSplit)
modForRF = randomForest(Caesarian~.,data = tabSim)
pred5 = predict(modForRF,newdata = tabTestRFSplit, type ="prob")
p4 = prediction(pred5[,2],tabTest$Caesarian)
perf4 = performance(p4,"tpr","fpr")

#Bagging
k=3
n = dim(X)[1]
pas = floor(n/k)
erBagSplit =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestBag = X2[((i-1)*pas):(i*pas),] #Données de test
  modbagS = bagging(Caesarian~.,data = tabSim,coob=TRUE)
  pred6 = predict(modbagS,newdata = tabTestBag, type ="prob")
  difference= table(pred6[,2]>0.5,tabTestBag$Caesarian)
  erBagSplit[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erBagSplit)
boxplot(erBagSplit)
modbagS = bagging(Caesarian~.,data = tabSim,coob=TRUE)
pred6 = predict(modbagS,newdata = tabTest, type ="prob")
p5 = prediction(pred6[,2],tabTest$Caesarian)
perf5 = performance(p5,"tpr","fpr")

plot(perf)
plot(perf6, add = TRUE, col ="purple")
plot(perf3,add= TRUE,  col = "red")
plot(perf4,add= TRUE, col = "orange")
plot(perf5,add= TRUE, col = "blue")
plot(perf2,add= TRUE, col = "pink")
boxplot(er1,er2, names = c('Tree','TreeSplit'),cex = 0.5)
boxplot(erRF,erRFSplit, names = c('RF','RFSplit'),cex = 0.5)
boxplot(erBag,erBagSplit, names = c('Bagging','BaggingSplit'),cex = 0.5)
