#' Function to permute y column in dataframe
#'
#' @param dat dataframe
#' @param xname column name from dat for x-dimension in plots (defaults "x")
#' @param yname column name from dat for y-dimension in plots (defaults "y")
#'
#' @return dataframe with columns x, permy and type
#' @export
#'
#' @examples
#' dat=data.frame(x=rnorm(5))
#' dat$y=x+rnorm(5)
#' null_dat <- make_null_dat(dat)
make_null_dat=function(dat,xname="x",yname="y"){
  permy=sample(dat[[yname]],length(dat[[yname]]),replace=FALSE) #permutation
  #If we want to bootstrap y instead of permuting y
  #permy=sample(y,length(y),replace=TRUE)
  data.frame(x=dat[[xname]],
             permy=permy,
             type="null")
}

#' Function to make alternate plot data
#'
#' capture mean structure between X and Y with local linear
#'  regression with optimal bandwidth, then permute residuals
#'
#' @param dat dataframe
#' @param xname column name from dat for x-dimension in plots (defaults "x")
#' @param yname column name from dat for y-dimension in plots (defaults "y")
#'
#' @return
#' @export output dataframe with columns x, permy and type
#'
#' @examples
#' dat=data.frame(x=rnorm(5))
#' dat$y=x+rnorm(5)
#' alt_dat <- make_alt_dat(dat)
make_alt_dat=function(dat,xname="x",yname="y"){
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

  data.frame(x=dat[[xname]],
             permy=permy,
             type="alt")
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
