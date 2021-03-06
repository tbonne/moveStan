#' A function to plot posterior predicitve checks on the model
#'
#' This function uses samples from the stanfit object to generate predictive regions in 2d.
#' @param model.fit stanfit model, containing y_pred and d_pred as generated quantities: y_pred is the model predicted angle, and d_pred is the model predicted step length.
#' @param df.obs dataframe of observed travel points.
#' @param rangePred Range of observed travel points to predict.
#' @param numbDraws Number of predictions to make for each observed point.
#' @param buffer Spatial buffer for the extent of the output plot around observed points.
#' @param contours Contours used in the plot of the kernel density of predicted points.
#' @import ks
#' @export
#' @examples
#'
#'

postPredMovements <- function(model.fit, df.obs, contours=c(25,50,75), rangePred=1:10, numbDraws=500, buffer=10){

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
