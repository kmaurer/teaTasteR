#' Function to permute y column in dataframe
#'
#' @param dat dataframe
#' @param xname column name from dat for x-dimension in plots (defaults "x")
#' @param yname column name from dat for y-dimension in plots (defaults "y")
#' @param bootstrap logical indicator of if bootstrapping should be used following permutation
#' @param jitter logical indicator of if jittering should be used on final points
#'
#' @return dataframe with columns x, permy and type
#' @export
#'
#' @examples
#' dat=data.frame(x=rnorm(5))
#' dat$y=dat$x+rnorm(5)
#' null_dat <- make_null_dat(dat)
#' null_dat <- make_null_dat(dat, bootstrap=TRUE)
#' null_dat <- make_null_dat(dat, bootstrap=TRUE, jitter=TRUE)
make_null_dat=function(dat,xname="x",yname="y", bootstrap=FALSE, jitter=FALSE){
  permy=sample(dat[[yname]],length(dat[[yname]]),replace=FALSE) #permutation
  #If we want to bootstrap y instead of permuting y
  #permy=sample(y,length(y),replace=TRUE)
  outdat <- data.frame(x=dat[[xname]],
             permy=permy,
             type="null")
  if(bootstrap){
    row_idx <- sample(1:nrow(outdat), nrow(outdat), replace=T)
    outdat <- outdat[row_idx, ]
  }
  if(jitter){
    data_resx <- ggvis_resolution(outdat[,"x"], zero=T)
    data_resy <- ggvis_resolution(outdat[,"permy"], zero=T)
    outdat$x <- outdat$x + runif(nrow(outdat), -data_resx/2,data_resx/2)
    outdat$permy <- outdat$permy + runif(nrow(outdat), -data_resy/2,data_resy/2)
  }
  return(outdat)
}

#' Function to make alternate plot data
#'
#' capture mean structure between X and Y with local linear
#'  regression with optimal bandwidth, then permute residuals
#'
#' @param dat dataframe
#' @param xname column name from dat for x-dimension in plots (defaults "x")
#' @param yname column name from dat for y-dimension in plots (defaults "y")
#' @param bootstrap logical indicator of if (bootstrap==TRUE) bootstrapping used, if (bootstrap==FALSE) then LOESS residuals permuted
#' @param jitter logical indicator of if jittering should be used on final points
#'
#' @return
#' @export output dataframe with columns x, permy and type
#'
#' @examples
#' dat=data.frame(x=rnorm(25))
#' dat$y=dat$x+rnorm(25)
#' alt_dat <- make_alt_dat(dat)
#' alt_dat <- make_alt_dat(dat,bootstrap=T,jitter=T)
make_alt_dat=function(dat,xname="x",yname="y", bootstrap=FALSE, jitter=FALSE){
  if(bootstrap){
    row_idx <- sample(1:nrow(dat), nrow(dat), replace=T)
    outdat = data.frame(x=dat[[xname]][row_idx],
                        permy=dat[[yname]][row_idx],
                        type="alt")
    if(jitter){
      data_resx <- ggvis_resolution(dat[[xname]], zero=T)
      data_resy <- ggvis_resolution(dat[[yname]], zero=T)
      outdat$x <- outdat$x + runif(nrow(outdat), -data_resx/2,data_resx/2)
      outdat$permy <- outdat$permy + runif(nrow(outdat), -data_resy/2,data_resy/2)
    }
  }else{
    x=dat[[xname]]
    y=dat[[yname]]
    b=get.dpill(x,y) #bandwidth selection (with robust selection function in kernplus package)
    if(b <=0) b=abs(b)
    #local linear regression
    fitted=c()
    for(i in 1:length(x)){
      v=x-x[i]
      Ki=dnorm(v/b)
      fitted[i]=lm(y~v,weight=Ki)$coef[1]
    }
    resi=y-fitted
    permy=fitted+sample(resi,length(resi),replace=FALSE)
    outdat = data.frame(x=dat[[xname]],
               permy=permy,
               type="alt")
  }
  return(outdat)
}


#' Make a lineup plot data
#'
#' Function to create stacked dataframe with M pairs of plots
#'  based on original dataframe named dat. ItWill randomize the
#'  order labels used in building plot lineup
#'
#' @param M number of pairs of plots
#' @param dat dataframe
#' @param xname column name from dat for x-dimension in plots (defaults "x")
#' @param yname column name from dat for y-dimension in plots (defaults "y")
#' @param seed set random seed for reproducible lineups
#'
#' @return dataframe with columns x, permy, type, order
#' @export
#'
#' @examples
#' dat=data.frame(x=rnorm(5))
#' dat$y=x+rnorm(5)
#' lineup_dat = make_lineup_dat(M=8, dat, xname="x", yname="y")
make_lineup_dat=function(M, dat, xname="x", yname="y", seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  order_types = sample(rep(c("null","alt"),each=M),2*M)
  dat_stacked = NULL
  for(i in 1:(2*M)){
    if(order_types[i] =="null") dat_i = make_null_dat(dat, xname, yname)
    if(order_types[i] =="alt") dat_i = make_alt_dat(dat, xname, yname)
    dat_i$order = i
    dat_stacked = rbind(dat_stacked,dat_i)
  }
  dat_stacked
}

## robust kernel selection function
# source: https://rdrr.io/cran/kernplus/src/R/est_bandwidth.R
get.dpill <- function(cov, y) {
  bw <- KernSmooth::dpill(cov, y)
  if (is.nan(bw)) {
    par <- 0.06
    while (is.nan(bw)) {
      bw <- KernSmooth::dpill(cov, y, proptrun = par)
      par <- par + 0.01
    }
  }
  return(bw)
}

#Adapted copy of ggvis::resolution version 0.4.4 to avoid requiring installation for one helper
ggvis_resolution <- function(x, zero = TRUE) {
  x <- unique(as.numeric(x))
  if (zero) {
    x <- unique(c(0, x))
  }
  min(diff(sort(x)))
}
