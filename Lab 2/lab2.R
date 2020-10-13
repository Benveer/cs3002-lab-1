getwd()
mydata= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\iris.csv',sep=',')

irisReal= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\iris_real.csv',sep=',')

plot(mydata)

mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables 


d <- dist(mydata, method = "manhattan") # distance matrix
source("C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\WK_R.r")

iter=5

methods=c("single","complete","average","kmeans")



matrixMeans <- matrix('', iter, 2)





for(method in methods)
  
{
  for(i in 2:iter)
   {
    #Hierarchical clustering
    #Average is best
    #noOfClusters<-i-1
    
    if(isTRUE(method!= "kmeans"))
    {    
    fit <- hclust(d, method=method)

    plot(fit,main=paste(method,' Dendrogram ',i," clusters"),xlab="Distance") # Plot dendrogram for each number of clusters
    
    
    
    Hgroups <- cutree(fit, k=i) # cut tree into x clusters
    rect.hclust(fit, k=i,border="red")  #Draws clusters on dendrogram
    
    

    
    #plot(mydata, col=Hgroups) #display scatter plot for each iter of hcluster
    
    wk = WK_R( Hgroups,irisReal$X1)
    
    matrixMeans[i,1]<- i
    matrixMeans[i,2]<- wk
    #print(wk) #prints weighted kappa
    
    
    }
    
    
    
    else{
      
      k=5
      #Kmeans clustering
      fit <- kmeans(mydata, k) 

      aggregate(mydata,by=list(fit$cluster),FUN=mean)
      
      Kgroups = fit$cluster 
      
      #plot(mydata, col=Kgroups) #display scatter plot for each iter of kcluster
      
      wk = WK_R( Kgroups,irisReal$X1)
      
      matrixMeans[i,1]<- i
      matrixMeans[i,2]<- wk
      
      #print(wk) #prints weighted kappa
      
      
      
    }


    
  }
  plot(matrixMeans,main=method,xlab= "Number of Clusters", ylab= "Weight")
  
  
  
  if(method!="kmeans")
  {
    
   # fit <- hclust(d, method=method)
  #  plot(fit,main=paste(method,' Dendrogram'),xlab="Distance") # plots dendrogram at final iter
    
  }


    
}
  


  






