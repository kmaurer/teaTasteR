#' Function to generate tea-tasting lineup
#'
#' @param lineup_dat dataframe structured from \code{\link{make_lineup_dat}}
#'
#' @return ggplot
#' @export
#'
#' @examples
#' dat=data.frame(x=rnorm(50))
#' dat$y=x+rnorm(50)
#' lineup_dat = make_lineup_dat(M=8, dat, xname="x", yname="y")
#' tt_lineup_plot(lineup_dat)
tt_lineup_plot = function(lineup_dat){
  lineup_dat$facet_lab = paste("Plot",lineup_dat$order)
  lineup_dat$facet_lab = factor(lineup_dat$facet_lab,
                                levels=paste("Plot",min(lineup_dat$order):max(lineup_dat$order)) )
  ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=x,y=permy), data=lineup_dat) +
    ggplot2::facet_wrap(~facet_lab)+
    ggplot2::labs(x="",y="")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.spacing = ggplot2::unit(.75, "cm"),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank())
}
