#########################################
# author: Xiang Li (xxiang13@gmail.com) #
# March 7 2016                          #
#########################################

radart.chart = function(fit,clus_toview = c(1,2,3)){
  library(fmsb)
  n_clusters = nrow(fit$centers[clus_toview,])
  centers = data.frame(fit$centers[clus_toview,])
  min = apply(fit$centers[clus_toview,], 2, function(x) round(min(x,na.rm = TRUE)-2,2))
  max = apply(fit$centers[clus_toview,], 2, function(x) round(max(x,na.rm = TRUE)+2,2))
  minmax = data.frame(rbind(max,min))
  #colnames(minmax) = colnames(centers)
  
  centers = rbind(minmax,centers)
  #colors6 = c("royalblue4","chocolate1","chartreuse3","darkorchid3","hotpink3","darkgoldenrod1")
  
  radarchart(centers, maxmin= TRUE, axistype=0, pcol=clus_toview, plwd = 2,plty=1,
             cglcol = 'black', axislabcol = "black", vlcex=1.2,cglwd = 1,
             title=paste(nrow(fit$centers),"Clusters",sep=" "))
  
  cluster_name = sapply(clus_toview,function(x) paste("Clutser",x,sep=' '))
  legend(1.5,1,legend=cluster_name, title="Clusters",col =clus_toview,lty=1, cex=1.2,lwd = 2)
}
