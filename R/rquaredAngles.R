#' A function to estimate explained variance
#'
#' This function uses samples from a stanfit object to estimate the error between predicted and observed angles. R-squared is then estimated as 1 minus the variation in the error divided by the total variation in the observed angles.
#' @param model.fit stanfit model, containing a_pred as generated quantities: a_pred is the model predicted angle.
#' @param obs.angles Vector of observed angles used in model fitting.
#' @import rstan
#' @export
#' @examples
#'
#'
r.squared.angles <- function(model.fit, obs.angles){

  #observed angles
  obs.a <- as.circular(obs.angles,type="angles",units="radians",modulo="2pi")

  #generate posterior sampling
  post<-extract(model.fit)
  postSamples <- length(post[[1]])

  #calculate preditction error at the level of the data point
  var.est <- data.frame(var=rep(0,postSamples))
  var.error <- data.frame(var=rep(0,postSamples))
  for(i in 1:postSamples){
    pred.a <- atan2(sin(post$a_pred[i,]),cos(post$a_pred[i,]))
    error.a<- atan2(sin(pred.a-obs.a),cos(pred.a-obs.a))
    var.est[i,1] <- var.circular(as.circular(pred.a,type="angles",units="radians"))
    var.error[i,1] <- var.circular(as.circular(error.a,type="angles",units="radians"))
  }

  #proportion explained vs. total
  r.square.simple <- (var.est[,1])/(var.est[,1]+var.error[,1])
  print(median(r.square.simple))

  return(r.square.simple)

}
