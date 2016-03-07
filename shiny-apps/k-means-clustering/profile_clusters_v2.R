#########################################
# author: Xiang Li (xxiang13@gmail.com) #
# March 7 2016                          #
#########################################


profile.clusters.v2 = function(fit,vars_toProfile,data){

  profiling = c()
  n = sum(fit$size)
  
  if (class(data[,vars_toProfile]) %in% c("numeric","integer")){
    data$cluster = fit$cluster # append cluster assignment
    mean = c()
    data_mean = round(mean(data[,vars_toProfile]),2)
    
    for (i in 1:length(fit$size)){
      mean = rbind(mean,round(mean(data[which(data$cluster == i), vars_toProfile]),2))
      }
    
    profiling = rbind(mean,data_mean)
    colnames(profiling) = vars_toProfile
  }
  
  else{
  a = apply( 
    prop.table(table(fit$cluster,data[,vars_toProfile]),margin = 1)*100, 
    2, 
    function(u) sprintf( "%.2f%%", u ))
  
  b = apply(
    prop.table(table(data[,vars_toProfile]))*100,
    1,
    function(u) sprintf( "%.2f%%", u ))
  
  profiling = rbind(a,b)
  } 

  print(data.frame(
    Clusters = c(seq(1,nrow(fit$centers)),"Total"),
    n=c(fit$size, n),
    Pct=paste((round(c(fit$size, n)/n,2))*100,"%",sep=''),
    profiling))

}