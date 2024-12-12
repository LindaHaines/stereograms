#' Draws circle of stereogram and places axis labels:
#' Called by grstereo
#'
#' @param r   Radius of circle 0 < r <= 1
#' @param nbb Labels 0 (x1,x2,x3), 1 (110 added)
#'
gcircle=function(r,nbb)
{
  par(pty="s")
  cp1=c(-1.15,1.15)
  cp2=c(-1.15,1.15)
  plot(cp1,cp2,type="p",axes="F",xlab="",ylab="",cex=0)
  # circle
  n=1000
  cx=cos(seq(0,2*pi,by=pi/n))
  cy=sin(seq(0,2*pi,by=pi/n))
  lines(r*cx,r*cy,type="l",lwd=0.5)
  # annotate x
  rt=r+0.01
  text(0,rt*1.075,expression(paste(x[2])),cex=1.25)
  text(rt*1.075,0.0,expression(paste(x[1])),cex=1.25)
  text(rt*0.075,rt*0.065,expression(paste(x[3])),cex=1.25)
  #annotate 110
  if(nbb==1)
  { dt=((1/sqrt(2))+0.01)*(rt+0.05)
    text(dt,dt,expression(paste("[110]")),cex=0.65)
    text(dt,-dt,expression(paste("[1",bar(1),"0]")),cex=0.65)
    text(-dt,dt,expression(paste("[",bar(1),"10]")),cex=0.65)
    text(-dt,-dt,expression(paste("[",bar(1),bar(1),"0]")),
         cex=0.65)}
}
