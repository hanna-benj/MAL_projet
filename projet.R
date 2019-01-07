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
library(splitstackshape)
plot(X)
boxplot(X)
#tree

modtree=tree(Caesarian~.,data=X)
plot(modtree)
text(modtree)

#faire attention aux codage des variables, coder le 1,2,3,4 des enfants en faisant une colone par enfants au lieu de une colone avec 1234. il y a des package qui le fond. fonction demi coding library nnet
#class.ind(X$Delivery.number)
#Tree
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

#Bayes
k=3
n = dim(X)[1]
pas = floor(n/k)
erBayes =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modbayes = naive_bayes(Caesarian~.,data=tabSim)
  pred8 = predict(modbayes,newdata = tabTest, type ="prob")
  difference= table(pred8[,2]>0.5,tabTest$Caesarian)
  erBayes[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erBayes)
boxplot(erBayes)
modbayes = naive_bayes(Caesarian~.,data=tabSim)
pred8 = predict(modbayes,newdata = tabTest, type ="prob")
p7 = prediction(pred8[,2],tabTest$Caesarian)
perf7 = performance(p7,"tpr","fpr")
plot(perf7, col = "purple")

#ADL
k=3
n = dim(X)[1]
pas = floor(n/k)
erADL =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modLDA = lda(Caesarian~.,data=tabSim)
  pred5 = predict(modLDA,newdata = tabTest, method="predictive")
  difference= table(pred5$posterior[,2]>0.5,tabTest$Caesarian)
  erADL[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erADL)
boxplot(erADL)
modLDA = lda(Caesarian~.,data=tabSim)
pred5 = predict(modLDA,newdata = tabTest, method="predictive")
p4 = prediction(pred5$posterior[,2],tabTest$Caesarian)
perf4 = performance(p4,"tpr","fpr")
plot(perf4, col = "green")

#QDA
k=3
n = dim(X)[1]
pas = floor(n/k)
erQDA =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modQDA = qda(Caesarian~.,data=tabSim)
  pred6 = predict(modQDA,newdata = tabTest, method="predictive")
  difference= table(pred6$posterior[,2]>0.5,tabTest$Caesarian)
  erQDA[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erQDA)
boxplot(erQDA)
modQDA = qda(Caesarian~.,data=tabSim)
pred6 = predict(modQDA,newdata = tabTest, method="predictive")
p5 = prediction(pred6$posterior[,2],tabTest$Caesarian)
perf5 = performance(p5,"tpr","fpr")
plot(perf5, col = "orange")

#Logistic Regression
k=3
n = dim(X)[1]
pas = floor(n/k)
erLR =c(1:k)
for (i in 1:k){
  tabSim = X[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = X[((i-1)*pas):(i*pas),] #Données de test
  modlogreg = glm(Caesarian~.,data=tabSim, family = "binomial")
  pred7 = predict(modlogreg,newdata = tabTest, method="predictive")
  difference= table(pred7>0.5,tabTest$Caesarian)
  erLR[i] = (difference[2] + difference[3]) / length(tabTest[,1]) #Erreur totaletabSim
}
m = mean(erLR)
boxplot(erLR)
modlogreg = glm(Caesarian~.,data=tabSim, family = "binomial")
pred7 = predict(modlogreg,newdata = tabTest, method="predictive")
p6 = prediction(pred7,tabTest$Caesarian)
perf6 = performance(p6,"tpr","fpr")
plot(perf6, col = "brown")

#KNN
for(i in 1:500){
  knnModel = knn(train = tabSim2, test = tabTest2, cl = tabSim$Weight, k = i)
  m = tabSim$Weight-as.numeric(as.character(knnModel))
  rmseknn = sqrt(mean(m^2))
  r[i]=rmseknn
}

#Comparaison des modèles
boxplot(er1,erRF,erBag,erBayes,erADL,erQDA,erLR, names = c('CART','RF', "BAGGING", "BAYES","ADL","QDA", "LR"),cex = 0.5)
plot(perf)
plot(perf2, add = TRUE, col = "blue")
plot(perf3, add = TRUE, col = "red")
plot(perf4, add = TRUE, col = "green")
plot(perf5, add = TRUE, col = "orange")
plot(perf6, add = TRUE, col = "brown")
plot(perf7, add = TRUE, col = "purple")
legend(0.8,0.8,legend=c("CART","Bagging", "Random Forest", "LDA", "QAD", "Logistic Regression", "Bayes"), col=c("black","blue","red","green","orange","brown","purple"),lty=1, cex=0.8, ncol=1)

##réplicage du dataset
Z = expandRows(X, count = 5, count.is.col = FALSE)
Z$Caesarian <- as.character(X$Caesarian)
Z$Caesarian <- as.factor(X$Caesarian)
Y = Z[sample(nrow(Z)),]

#Tree
k=10
n = dim(Y)[1]
pas = floor(n/k)
er1 =c(1:k)
for (i in 1:k){
  tabSim = Y[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest = Y[((i-1)*pas):(i*pas),] #Données de test
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
k=8
n = dim(Y)[1]
pas = floor(n/k)
erRF =c(1:k)
for (i in 1:k){
  tabSim = Y[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTest =Y[((i-1)*pas):(i*pas),] #Données de test
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


#-----
  
