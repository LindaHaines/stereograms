#' Plot points with colour by converting to
#' stereogram coordinates bigx bigy
#'
#' @param xmat Matrix of 3 columns for points
#' @param cx   Size of points
#' @param clr  Colour of points
#' @param r.   Radius of stereogram
#'
gplot=function(xmat,cx,clr,r)
{# set up bigx bigy with z < 0
  xmat1=subset(xmat,xmat[,3]< -0.0001)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  points(r*bigx,r*bigy,type="p",pch=1,cex=sqrt(2)*cx,col=clr)
  # set up bigx bigy z > 0
  xmat1=subset(xmat,xmat[,3]>=0)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  points(r*bigx,r*bigy,type="p",pch=16,cex=cx,col=clr)
}

