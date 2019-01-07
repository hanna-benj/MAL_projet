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
perfTree = performance(p2,"tpr","fpr")

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
perfRFS = performance(p4,"tpr","fpr")

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
pred6 = predict(modbagS,newdata = tabTestBag, type ="prob")
p5 = prediction(pred6[,2],tabTest$Caesarian)
perfBagS = performance(p5,"tpr","fpr")

#Bayes
k=3
n = dim(X)[1]
pas = floor(n/k)
erBayesS =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestBayes = X2[((i-1)*pas):(i*pas),] #Données de test
  modbayesS = naive_bayes(Caesarian~.,data=tabSim)
  predBayesS = predict(modbayes,newdata = tabTestBayes, type ="prob")
  difference= table(predBayesS[,2]>0.5,tabTestBayes$Caesarian)
  erBayesS[i] = (difference[2] + difference[3]) / length(tabTestBayes[,1]) #Erreur totaletabSim
}
m = mean(erBayesS)
boxplot(erBayesS)
modbayesS = naive_bayes(Caesarian~.,data=tabSim)
predBayesS = predict(modbayesS,newdata = tabTestBayes, type ="prob")
pbs = prediction(predBayesS[,2],tabTestBayes$Caesarian)
perfbs = performance(pbs,"tpr","fpr")

#ADL
k=3
n = dim(X)[1]
pas = floor(n/k)
erADLS =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestADLS = X2[((i-1)*pas):(i*pas),] #Données de test
  modLDAS = lda(Caesarian~.,data=tabSim)
  predADLS = predict(modLDAS,newdata = tabTestADLS, method="predictive")
  difference= table(predADLS$posterior[,2]>0.5,tabTestADLS$Caesarian)
  erADLS[i] = (difference[2] + difference[3]) / length(tabTestADLS[,1]) #Erreur totaletabSim
}
m = mean(erADLS)
boxplot(erADLS)
modLDAS = lda(Caesarian~.,data=tabSim)
predADLS = predict(modLDAS,newdata = tabTestADLS, method="predictive")
pADLS = prediction(predADLS$posterior[,2],tabTest$Caesarian)
perfADLS = performance(pADLS,"tpr","fpr") # problème de collinéarité

#QDA
k=3
n = dim(X)[1]
pas = floor(n/k)
erQDAS =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestQDAS = X2[((i-1)*pas):(i*pas),] #Données de test
  modQDAS = qda(Caesarian~.,data=tabSim)
  pred6 = predict(modQDAS,newdata = tabTestQDAS, method="predictive")
  difference= table(pred6$posterior[,2]>0.5,tabTestQDAS$Caesarian)
  erQDAS[i] = (difference[2] + difference[3]) / length(tabTestQDAS[,1]) #Erreur totaletabSim
}
m = mean(erQDAS)
boxplot(erQDAS)
modQDAS = qda(Caesarian~.,data=tabSim)
pred6 = predict(modQDAS,newdata = tabTestQDAS, method="predictive")
p5 = prediction(pred6$posterior[,2],tabTestQDAS$Caesarian)
perfQDAS = performance(p5,"tpr","fpr") #groupe 0 n'est pas de rang plein

#Logistic Regression
k=3
n = dim(X)[1]
pas = floor(n/k)
erLRS =c(1:k)
for (i in 1:k){
  tabSim = X2[setdiff(1:n,((i-1)*pas):(i*pas)),] #On retire la k-ieme partition
  tabTestLRS = X2[((i-1)*pas):(i*pas),] #Données de test
  modlogregLRS = glm(Caesarian~.,data=tabSim, family = "binomial")
  pred7 = predict(modlogregLRS,newdata = tabTestLRS, method="predictive")
  difference= table(pred7>0.5,tabTestLRS$Caesarian)
  erLRS[i] = (difference[2] + difference[3]) / length(tabTestLRS[,1]) #Erreur totaletabSim
}
m = mean(erLRS)
boxplot(erLRS)
modlogregLRS = glm(Caesarian~.,data=tabSim, family = "binomial")
pred7 = predict(modlogregLRS,newdata = tabTestLRS, method="predictive")
p6 = prediction(pred7,tabTestLRS$Caesarian)
perfLRS = performance(p6,"tpr","fpr") # les prédictions venant d'un modèle de rang faible peuvent être trompeuses

plot(perf) # Tree
plot(perfTree, add = TRUE, col ="purple")
plot(perf3,add= TRUE,  col = "red") #Random Forest
plot(perfRFS,add= TRUE, col = "orange")
plot(perf5,add= TRUE, col = "blue") #Bagging
plot(perf2,add= TRUE, col = "pink")
plot(perf7,add = TRUE, col = "green")#Bayes
plot(perfbs,add = TRUE, col = "grey") 
plot(perf4,add = TRUE, col = "aquamarine1") #ADL
plot(perfADLS,add = TRUE, col = "darksalmon")
plot(perf5,add = TRUE, col = "chocolate3") #QDA
plot(perfQDAS,add = TRUE, col = "brown3")
plot(perf6,add = TRUE, col = "burlywood2") # Logistic Regression
plot(perfLRS,add = TRUE, col = "chartreuse1")
boxplot(er1,er2, names = c('Tree','TreeSplit'),cex = 0.5)
boxplot(erRF,erRFSplit, names = c('RF','RFSplit'),cex = 0.5)
boxplot(erBag,erBagSplit, names = c('Bagging','BaggingSplit'),cex = 0.5)
boxplot(erBayes,erBayesS, names = c('Bayes','BayesSplit'),cex = 0.5)
boxplot(erADL,erADLS, names = c('ADL','ADLSplit'),cex = 0.5)
boxplot(erQDA,erQDAS, names = c('QDA','QDASplit'),cex = 0.5)
boxplot(erLR,erLRS, names = c('LR','LRSplit'),cex = 0.5)
