getwd()
mydata= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 1\\forestfires.csv',sep=',')


plot(mydata)

View(mydata)

#refer by column name
plot(mydata$temp, mydata$wind) 

#refer by index
plot(mydata[,9],mydata[,11])

#histogram
hist(mydata$temp)

#line plots
plot(mydata$temp,type="l")

#plot colours
plot(mydata$X, mydata$Y, col=mydata$temp)

#calculate mean
meantemp = mean(mydata$temp)


write.csv(meantemp, file = "C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 1\\Output.csv")


#linear model with regression
plot(mydata$temp,mydata$ISI)
lmfire=line(mydata$ISI~mydata$temp)
abline(coef(lmfire))
