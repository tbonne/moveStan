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
#'library(rstan)
#'
#'#################################
#'#   Get data ready for rStan    #
#'#################################
#'
#'focalBaboon[is.na(focalBaboon)]<-0
#'
#'data.for.stan<-list(N =  nrow(focalBaboon),
#'ax = focalBaboon$dx.obs,
#'ay = focalBaboon$dy.obs,
#'bx=focalBaboon$dx.bearing,
#'by=focalBaboon$dy.bearing,
#'ax_cv=focalBaboon$dx.resultant,
#'ay_cv=focalBaboon$dy.resultant,
#'ax0=focalBaboon$dx.0,
#'ax1=focalBaboon$dx.1,
#'ay0=focalBaboon$dy.0,
#'ay1=focalBaboon$dy.1)
#'
#'
#'#################################
#'#   Define rStan model          #
#'#################################
#'
#'stan.model.test = '
#'data{
#'int<lower = 0> N;    //number of data points
#'
#'vector[N] ax;        //displacement of observed travel
#'vector[N] ay;        //displacement of observed travel
#'vector[N] bx;        //displacement of previous travel
#'vector[N] by;        //displacement of previous travel
#'
#'vector[N] ax_cv;      //X component of unit vector towards group
#'vector[N] ay_cv;      //Y component of unit vector towards group
#'
#'vector[N] ax0;        //X component of unit vector towards individual 0
#'vector[N] ay0;        //Y component of unit vector towards individual 0
#'vector[N] ax1;        //X component of unit vector towards individual 1
#'vector[N] ay1;        //Y component of unit vector towards individual 1
#'
#'}
#'
#'transformed data{
#'
#'vector[N] a;
#'
#'//set observed angle of travel
#'for(i in 1:N){
#'a[i] = atan2(ay[i],ax[i]);
#'}
#'
#'}
#'
#'parameters{
#'
#'//These are the parameters to be estimated for angle of travel
#'real<lower = 0> kappa;
#'real beta_cv;
#'real beta_a0;
#'real beta_a1;
#'
#'}
#'
#'model{
#'
#'vector[N] y_hat;
#'
#'//setting our priors for betas
#'kappa ~ normal(0,10);
#'beta_cv ~ normal(0,1);
#'beta_a0 ~ normal(0,1);
#'beta_a1 ~ normal(0,1);
#'
#'//running the model
#'for(i in 1:N){
#'
#'//estimating angle of travel
#'y_hat[i] = atan2( by[i] + beta_cv*ay_cv[i] + beta_a0*ay0[i] + beta_a1*ay1[i], bx[i] + beta_cv*ax_cv[i] + beta_a0*ax0[i] + beta_a1*ax1[i]);
#'
#'}
#'
#'//likelihood
#'a ~ von_mises(y_hat, kappa);
#'}
#'
#'generated quantities{
#'
#'//generate log-likelihood for observations & predictions
#'vector[N] log_lik;
#'vector[N] a_pred;
#'
#'for(i in 1:N){
#'
#'a_pred[i] = von_mises_rng(atan2( by[i] + beta_cv*ay_cv[i], bx[i] + beta_cv*ax_cv[i] ),kappa);
#'
#'log_lik[i] = von_mises_lpdf(a[i]|atan2( by[i] + beta_cv*ay_cv[i], bx[i] + beta_cv*ax_cv[i] ),kappa);
#'
#'}
#'}
#''
#'
#'#################################
#'#   Fit the model               #
#'#################################
#'
#'fit.1 = stan(model_code = stan.model.test, data = data.for.stan, iter=1000, chains=1, cores=1)
#'
#'#################################
#'#   Check model fit             #
#'#################################
#'
#'print(fit.1, digits = 4, pars=c("kappa","beta_cv","beta_a0","beta_a1"))
#'#launch_shinystan(fit.1)
#'
#'
#'#################################
#'# Posterior predictive checks   #
#'#################################
#'
#'postPredAngle(fit.1, focalBaboon, rangePred=1:10)
#'
#'
postPredAngle <- function(model.fit, df.obs=NULL, contours=c(25,50,75), rangePred=1:10, numbDraws=500, buffer = 10, extention=10,angles=NULL){

  #get samples
  post<-rstan::extract(model.fit)

  #no spatial coordinates given
  if(is.null(df.obs)){

    #No angles given
    if(is.null(angles)){

      print("To contrast observed and predicted angles please supply observed angles")

      for(i in rangePred){

        for(j in 1:numbDraws){
          predX <- cos(post$a_pred[j,i])*extention
          predY <- sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="slice",cont=contours)
      }

      #Yes, angles given
    } else {

      angle.colors<-rainbow(ncol(angles))
      angle.colors[1]<-"black"

      for(i in rangePred){

        for(j in 1:numbDraws){
          predX <- cos(post$a_pred[j,i])*extention
          predY <- sin(post$a_pred[j,i])*extention
          pred.points[j,] <- c(predX,predY)
        }

        ks.pred <- kde(pred.points, binned = T)
        plot(ks.pred,display="slice",cont=contours)
        for(j in 1:ncol(angles)){
          arrows(x0=0,y0=0,x1=cos(angles[i,j])*extention,y1=sin(angles[i,j])*extention,col=angle.colors[j])
        }
        legend("topright", title="Angles", colnames(angles), fill=angle.colors)
      }
    }

    #Yes, spatial coordinates are given
  } else {

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
