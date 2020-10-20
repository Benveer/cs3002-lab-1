irisData = read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 3\\iris.csv', sep=",",header=FALSE)
irisRealData = read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 3\\iris_real.csv', sep=",",header=FALSE)

colnames(irisRealData)[colnames(irisRealData) == "V1"] <- "v0" # Rename column

irisData<-cbind(irisRealData,irisData )

#set up a training set
iris_rand=irisData[sample(150,150),]
X_train <- iris_rand[1:100,2:5]
X_test <- iris_rand[100:150,2:5]

y_train <- iris_rand[1:100,1]
y_test <- iris_rand[100:150,1]



library(rpart)
fit <- rpart(y_train~., method="class", data=X_train)


plot(fit, uniform=TRUE, main="Decision Tree for iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, X_test, type = 'class')

accuracy <- function(test, treepred) {

  n = length(test) #the number of test cases
  ncorrect = sum(treepred==test) #the number of correctly predicted
  accuracy=ncorrect/n
  return (accuracy)
}

print(accuracy(y_test,treepred))




#Pruning

list=c(0.1,0.5,0.9)
table_mat = table(y_test, treepred)
print(table_mat)

for(i in list){

  pfit<- prune(fit, cp=i)
  #plot(pfit, uniform=TRUE, main="Pruned Decision Tree for iris")
  #text(pfit, use.n=TRUE, all=TRUE, cex=.8)

  treepred <-predict(pfit, X_test, type = 'class')
  print(accuracy(y_test,treepred))
  
  
}



#Question 3



X_train_two <- X_train[,c("V1","V2")]
X_test_two <- X_test[,c("V1","V2")]


plot(irisData[,c("V1")],irisData[,c("V2")],col=c("blue","red"), main="Scatter Plot for 2 select variables")


fit <- rpart(y_train~., method="class", data=X_train_two)


plot(fit, uniform=TRUE, main="Decision Tree for 2 select variables")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, X_test_two, type = 'class')


print(accuracy(y_test,treepred))


#k nearest neighbour 




library(class)



knn3pred = knn(X_train_two, X_test_two, y_train, k=15)



n = length(y_test) #the number of test cases
ncorrect = sum(knn3pred==y_test) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
print( table(y_test, knn3pred))

