#' Function to generate tea-tasting lineup
#'
#' @param lineup_dat dataframe structured from \code{\link{make_lineup_dat}}
#'
#' @return lineup faceted ggplot object
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
    ggplot2::facet_wrap(~facet_lab,nrow=2)+
    ggplot2::labs(x="",y="")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.spacing = ggplot2::unit(.75, "cm"),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank())
}


#' Make lineup plot solution key
#'
#' Function to return list of plot types related to order of plots, used in assessment step
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
#' lineup_keyplot(lineup_dat)
#!# need to fix dependencies on magridr, dplyr and ggplot2
lineup_keyplot = function(lineup_dat){
  lineup_dat %>%
    select(order, type) %>%
    unique() %>%
    as.data.frame()%>%
    mutate(sol_y = 4-(order-1) %/% 4,
           sol_x = (order-1) %% 4 + 1,
           type=factor(type, levels=c("null","alt"))) %>%
    ggplot(aes(x=sol_x, y=sol_y, fill=type, label=paste0("Plot ",order,"\n",type))) +
    geom_tile(color="black")+
    geom_text(size=2) +
    scale_fill_manual(values=c("white","gray80"))+
    theme_void()+
    theme(legend.position = "none",
          plot.title = element_text(hjust=.5))
}
#

