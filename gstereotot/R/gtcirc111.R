#' Plot four circles round [111] axes
#' Calls function gccalc
#'
#' @param xc  Coordinates of circle around (111)
#' @param clr Colour of circles
#' @param lw  Line width for circles
#' @param r   Radius of stereogram
#'
gtcirc111=function(xc,clr,lw,r)
{
# circle through points
bigx=xc[,1]/(1+abs(xc[,3]))
bigy=xc[,2]/(1+abs(xc[,3]))
xymat=cbind(bigx,bigy)
cr=gccalc(xymat)
# draw a circle
cmat=rbind(c(1,1),c(1,-1),c(-1,1),c(-1,-1))
cmat=cmat*cr[1:2]
rad=cr[3]
n=1000
for(j in 1:4)
{cx=rad*cos(seq(0,2*pi,by=pi/n))+cmat[j,1]
cy=rad*sin(seq(0,2*pi,by=pi/n))+cmat[j,2]
lines(r*cx,r*cy,type="l",lwd=lw,col=clr)}
}
