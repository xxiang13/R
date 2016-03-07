#########################################
# author: Xiang Li (xxiang13@gmail.com) #
# March 7 2016                          #
#########################################


summary.clusters = function(data,fit,vars_toSummary){

  data$cluster = fit$cluster # append cluster assignment
  mean = c()
  n = sum(fit$size)
  data_mean = data.frame(lapply(data[,vars_toSummary], function(x) round(mean(x,na.rm = TRUE),2)))
  for (i in 1:length(fit$size)){
    mean = rbind(mean,
                            lapply(data[which(data$cluster == i),
                                        colnames(fit$centers)], 
                                   function(x) round(mean(x,na.rm = TRUE),2)))
  }
  mean = data.frame(mean)
  
  print(data.frame(
    Clusters = c(seq(1,nrow(fit$centers)),"Total"),
    n=c(fit$size, n),
    Pct=paste((round(c(fit$size, n)/n,2))*100,"%",sep=''),
    rbind(mean,data_mean)
  ))

}
