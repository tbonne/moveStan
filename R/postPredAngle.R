#' A function to plot posterior predicitve checks for rStan movement models
#'
#' This function uses samples from a stanfit object to generate predictive checks on directions of travel.
#' @param model.fit stanfit model, containing a_pred as generated quantities: a_pred is the model predicted angle.
#' @param df.obs dataframe of observed travel points.
#' @param rangePred Range of observed travel points to predict.
#' @param numbDraws Number of predictions to make for each observed point.
#' @param contours Contours used in the plot of the kernel density of predicted points, if null a raster is produced.
#' @param buffer Display parameter, used to extend the plot extent range.
#' @param extention Display parameter, used to define the radius on which to plot predictions for each point.
#' @param context List of shapefiles or rasters that can provide contextual information.
#' @import ks
#' @import rstan
#' @export
#' @examples
postPredAngle <- function(model.fit, df.obs, contours=c(25,50,75), rangePred=1:10, numbDraws=500, buffer = 10, extention=10, context = NULL){

  #get samples
  post<-rstan::extract(model.fit)

  ### no contextual information given
  if(is.null(context)){

    #start with an empty plot with set extent
    min.x <- min(df.obs[rangePred,]$x)-buffer
    min.y <- min(df.obs[rangePred,]$y)-buffer
    max.x <- max(df.obs[rangePred,]$x)+buffer
    max.y <- max(df.obs[rangePred,]$y)+buffer
    plot(1,type='n', xlim=c(min.x,max.x), ylim=c(min.y,max.y), xlab="X", ylab="Y")


    if(is.null(contours)){    ### raster based density
      #genrate plot
      for(i in rangePred) {

        one.obs <- df.obs[i,]
        pred.points <- matrix(,numbDraws,2)

        for(j in 1:numbDraws){
          predX <- df.obs[i,]$x+cos(post$a_pred[j,i])*extention
          predY <- df.obs[i,]$y+sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="image",add=T)
        actualA <- atan2(df.obs[i+1,]$y-df.obs[i,]$y,df.obs[i+1,]$x-df.obs[i,]$x)
        points(df.obs[i,]$x+cos(actualA)*extention,df.obs[i,]$y+sin(actualA)*extention,col="green")
        points(df.obs[i,]$x,df.obs[i,]$y,col="blue")
        lines(x=c(df.obs[i,]$x,df.obs[i,]$x+cos(actualA)*extention), y=c(df.obs[i,]$y,df.obs[i,]$y+sin(actualA)*extention), type="l", lty="dashed")

      }
    }else{
      #genrate plot
      for(i in rangePred) {   ### contour based density

        one.obs <- df.obs[i,]
        pred.points <- matrix(,numbDraws,2)

        for(j in 1:numbDraws){
          predX <- df.obs[i,]$x+cos(post$a_pred[j,i])*extention
          predY <- df.obs[i,]$y+sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="slice",cont=contours, add=T)
        actualA <- atan2(df.obs[i+1,]$y-df.obs[i,]$y,df.obs[i+1,]$x-df.obs[i,]$x)
        points(df.obs[i,]$x+cos(actualA)*extention,df.obs[i,]$y+sin(actualA)*extention,col="red")
        points(df.obs[i,]$x,df.obs[i,]$y,col="blue")
        lines(x=c(df.obs[i,]$x,df.obs[i,]$x+cos(actualA)*extention), y=c(df.obs[i,]$y,df.obs[i,]$y+sin(actualA)*extention), type="l", lty="dashed")

      }
    }

  #### contextual information given
  } else {


    plot(contextList[[1]], xlab="X", ylab="Y")
    for (l in 1:length(contextList)){
      plot(contextList[[l]], xlab="X", ylab="Y", add=T)
    }
    if(is.null(contours)){
      #genrate plot
      for(i in rangePred) {

        one.obs <- df.obs[i,]
        pred.points <- matrix(,numbDraws,2)

        for(j in 1:numbDraws){
          predX <- df.obs[i,]$x+cos(post$a_pred[j,i])*extention
          predY <- df.obs[i,]$y+sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="image",add=T)
        actualA <- atan2(df.obs[i+1,]$y-df.obs[i,]$y,df.obs[i+1,]$x-df.obs[i,]$x)
        points(df.obs[i,]$x+cos(actualA)*extention,df.obs[i,]$y+sin(actualA)*extention,col="green")
        points(df.obs[i,]$x,df.obs[i,]$y,col="blue")
        lines(x=c(df.obs[i,]$x,df.obs[i,]$x+cos(actualA)*extention), y=c(df.obs[i,]$y,df.obs[i,]$y+sin(actualA)*extention), type="l", lty="dashed")

      }
    }else{
      #genrate plot
      for(i in rangePred) {

        one.obs <- df.obs[i,]
        pred.points <- matrix(,numbDraws,2)

        for(j in 1:numbDraws){
          predX <- df.obs[i,]$x+cos(post$a_pred[j,i])*extention
          predY <- df.obs[i,]$y+sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="slice",cont=contours, add=T)
        actualA <- atan2(df.obs[i+1,]$y-df.obs[i,]$y,df.obs[i+1,]$x-df.obs[i,]$x)
        points(df.obs[i,]$x+cos(actualA)*extention,df.obs[i,]$y+sin(actualA)*extention,col="red")
        points(df.obs[i,]$x,df.obs[i,]$y,col="blue")
        lines(x=c(df.obs[i,]$x,df.obs[i,]$x+cos(actualA)*extention), y=c(df.obs[i,]$y,df.obs[i,]$y+sin(actualA)*extention), type="l", lty="dashed")
      }
    }
  }
}
