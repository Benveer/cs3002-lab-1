getwd()
mydata= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\iris.csv',sep=',')

plot(mydata)

mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables 


d <- dist(mydata, method = "manhattan") # distance matrix
source("C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\WK_R.r")

iter=20

list=c("single","complete","average")

matrix <- matrix('', iter, 2)
for(i in 1:length(list))
{
  
  print(list[i])
  
  
  for(noOfClusters in 2:iter)
  {
    
    #Hierarchical clustering
    #Average
    
    
    fit <- hclust(d, method=list[i])
    #plot(fit) # display dendogram for each iter
    
    Hgroups <- cutree(fit, k=noOfClusters) # cut tree into 5 clusters
    
    rect.hclust(fit, k=noOfClusters, border="red") 
    #plot(mydata, col=Hgroups) #display scatter plot for each iter of hcluster
    
    
    
    
    
    #Kmeans clustering
    fit <- kmeans(mydata, noOfClusters) # 5 cluster solution
    
    aggregate(mydata,by=list(fit$cluster),FUN=mean)
    
    Kgroups = fit$cluster 
    
    #plot(mydata, col=Kgroups) #display scatter plot for each iter of kcluster
    
    
    
    
    wk = WK_R(Kgroups, Hgroups)
    
    
    matrix[noOfClusters,1]<- noOfClusters
    matrix[noOfClusters,2]<- wk
    
    
    
    print(paste(list[i],"vs kmeans:"))
    print(wk)
    
    
    
    
    
    
  }
  
  plot(matrix,main=list[i],xlab= "Number of Clusters", ylab= "Weight")
  
  
  
  }  












