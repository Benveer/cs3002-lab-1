getwd()
mydata= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\iris.csv',sep=',')

irisReal= read.csv('C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\iris_real.csv',sep=',')

plot(mydata)

mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables 


d <- dist(mydata, method = "manhattan") # distance matrix
source("C:\\Users\\benve\\Documents\\university\\year3\\CS3002\\Labs\\Lab 2\\WK_R.r")

iter=20 # +1 to intended iter

methods=c("single","complete","average","kmeans")

matrixHCluster <- matrix('', iter, 2)
matrixKMeans <- matrix('', iter, 2)
matrixMeans <- matrix('', iter, 2)


for(method in methods )
{
  for(noOfClusters in 2:iter)
  {
    
    #Hierarchical clustering
    #Average is best
    
    
    if(isTRUE(method!= "kmeans"))
    {    
    fit <- hclust(d, method=method)

    Hgroups <- cutree(fit, k=noOfClusters) # cut tree into x clusters
    
    rect.hclust(fit, k=noOfClusters,border="red")  #Draws clusters on dendrogram

    #plot(fit,main=paste(method,' Dendrogram ',noOfClusters," clusters"),xlab="Distance") # Plot dendrogram for each number of clusters
    
    
    
    #plot(mydata, col=Hgroups) #display scatter plot for each iter of hcluster
    
    wk = WK_R( Hgroups,irisReal$X1)
    
    matrixMeans[noOfClusters,1]<- noOfClusters
    matrixMeans[noOfClusters,2]<- wk
    #print(wk) #prints weighted kappa
    
    
    }
    
    
    
    else{
      
      
      #Kmeans clustering
      fit <- kmeans(mydata, noOfClusters) 

      aggregate(mydata,by=list(fit$cluster),FUN=mean)
      
      Kgroups = fit$cluster 
      
      #plot(mydata, col=Kgroups) #display scatter plot for each iter of kcluster
      
      wk = WK_R( Kgroups,irisReal$X1)
      
      matrixMeans[noOfClusters,1]<- noOfClusters
      matrixMeans[noOfClusters,2]<- wk
      
      #print(wk) #prints weighted kappa
      
      
      
    }


    
  }
  plot(matrixMeans,main=method,xlab= "Number of Clusters", ylab= "Weight")
  
  
  
  if(method!="kmeans")
  {
    
    fit <- hclust(d, method=method)
    plot(fit,main=paste(method,' Dendrogram'),xlab="Distance") # plots dendrogram at final iter
    
  }


    
}
  


  






