
library(neuralnet)

#Assessed Exercise 1 


#XOR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
#XOR gate output data
trainout = rbind(0, 1, 1, 0);
#Combined OR gate data
XORdat=cbind(trainout,trainin)


# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = 0 , threshold = 0.001,stepmax = 1e+05, linear.output = FALSE)
#visualise the NN
plot(NN, main = "XOR Linear")

NN$weights


testin= rbind(c(1,1))
predict_testNN = compute(NN, testin)


predict_testNN$net.result



predict_out = as.numeric(predict_testNN$net.result>0.5)
print("XOR with linear solution:")
print(predict_out)


#set up the input sequence
testin=rbind(c(-1,-1),c(1,-1),c(-1,1), c(1,1))

predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
XOR_predict_out = as.numeric(predict_testNN$net.result>0.5)
XOR_predict_out

#XOR Multilayer

NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = c(3,3) , threshold =
                 0.001, stepmax = 1e+05, linear.output = FALSE)


plot(NN, main= "XOR Multilayer")

predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
XOR_predict_out_two = as.numeric(predict_testNN$net.result>0.5)
print("XOR with multilayer:")
print(XOR_predict_out_two)









#Assessed Exercise 2


wineData = read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 4\\winedata2.csv', sep=",")

wineClass = wineData[,1]
wineValues=wineData[,2:3]


wineClassTrain= wineClass[1:65]
wineValuesTrain= wineValues[1:65,]

wineClassTest= wineClass[66:130]
wineValuesTest= wineValues[66:130,]

wineClassTrain = replace(wineClassTrain,wineClassTrain==1,0)
wineClassTrain = replace(wineClassTrain,wineClassTrain==2,1)

wineClassTest = replace(wineClassTest,wineClassTest==1,0)
wineClassTest = replace(wineClassTest,wineClassTest==2,1)



# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(wineClassTrain~., wineValuesTrain, hidden = c(2,3) , threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)
#visualise the NN
plot(NN, main = "Wine Data Multilayer")




NN$weights


predict_testNN = compute(NN, wineValuesTest)


predict_testNN$net.result



predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)



accuracy = sum(predict_out == wineClassTest) / length(wineClassTest)
print(accuracy)


