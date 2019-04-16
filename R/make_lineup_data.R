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
#' dat=data.frame(x=rnorm(5)) %>%
#' mutate(y=x+rnorm(5))
#' null_dat <- make_null_dat(dat)
make_null_dat=function(dat,xname="x",yname="y"){
  permy=sample(dat[[yname]],length(dat[[yname]]),replace=FALSE) #permutation
  #If we want to bootstrap y instead of permuting y
  #permy=sample(y,length(y),replace=TRUE)
  data.frame(x=dat[[xname]],
             permy=permy,
             type="null")
}

