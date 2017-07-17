#' Movement data from one individual in a baboon group
#'
#' A dataset containing the directions towards individual group members, as well as the group center.
#' The variables are as follows:
#'
#' @format A data frame with 4529 rows and 55 variables:
#' \itemize{
#'   \item dx.obs, dy.obs: Unit vector towards the next observed location.
#'   \item dx.bearing, dy.bearing: Unit vector from the subsequent observed location.
#'   \item dx.0, dy.0: Unit vector towards group member 0 (individuals range from 1-13, with individual 10 missing).
#'   \item dist.0: Distance towards individual 0 (individuals range from 1-13, with individual 10 missing).
#'   \item dx.resultant, dy.resultant: Unit vector towards the mean of angle towards all group memebers.
#'   \item mag.resultant: Degree of aggreement in group member angles (0,1).
#'   \item RevisitT: Time from last observed location.
#'   \item time: Time of observation (YYYY-MM-dd T hh:mm).
#'   \item x: x coordinate (UTM34S WGS84)
#'   \item y: y coordinate (UTM34S WGS84)
#'   \item iid: sum of the inter-individual distances from the focal animal
#' }
#'
#' @docType data
#' @usage data(focalBaboon)
"focalBaboon"
