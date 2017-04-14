

#' A function to simulate movement using rStan models
#'
#' This function simulates movement using a stanfit object.
#' @param modelList A list of stanfit models.
#' @export
#' @examples
simMovements <- function(modelList, iter=100, startingContext= c(0,0), names=c("x","y"),formula="a ~ beta_cv * cv", functionList = NULL){
  
  postList <- list(extract(fit.groupOnly))
  
  df.sim <- as.data.frame(t(startingContext))
  
  #for each iteration
  for(i in iter){
    
    #for each model
    for j in length(postList){
      
      postList[[1]]    
      
    }
    
    
  }
  
  
  require(ks)
  
  #get samples
  post<-extract(model.fit)
  
  #set extent
  min.x <- min(df.obs[rangePred,]$x)-buffer
  min.y <- min(df.obs[rangePred,]$y)-buffer
  max.x <- max(df.obs[rangePred,]$x)+buffer
  max.y <- max(df.obs[rangePred,]$y)+buffer
  
  #genrate plot
  plot(x=one.obs$x,y=one.obs$y, xlim=c(min.x,max.x), ylim=c(min.y,max.y))
  for(i in rangePred) {
    
    one.obs <- df.obs[i,]
    pred.points <- matrix(,numbDraws,2)
    
    for(j in 1:numbDraws){
      predX <- one.obs$x + cos(post$y_pred[j,i])*post$d_pred[i,j]
      predY <- one.obs$y + sin(post$y_pred[j,i])*post$d_pred[i,j]
      pred.points[j,] <- c(predX,predY) 
    }
    
    ks.pred <- kde(pred.points, binned = T)
    plot(ks.pred,display="slice",cont=contours,add=T)
    points(df.obs[i+1,]$x,df.obs[i+1,]$y,col="green") 
    points(df.obs[i,]$x,df.obs[i,]$y,col="blue") 
    
  }
  
}