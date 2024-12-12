#' Draws planes through NS poles: medians/longitudes
#'
#' @param d   Degree of rotation
#' @param lw  Line width
#' @param clr Colour
#' @param R   Radius of stereogram
#'
gmeridian=function(d,lw,clr,r)
{
for(i in 1:floor(d/2))
{a=i*pi/d
ptx=c(sin(a), -sin(a))
pty=c(cos(a), -cos(a))
lines(r*ptx,r*pty,lwd=lw,col=clr)
ptx=c(-sin(a), sin(a))
pty=c(cos(a), -cos(a))
lines(r*ptx,r*pty,lwd=lw,col=clr)}
ptx=c(0,0)
pty=c(-1,1)
lines(r*ptx,r*pty,lwd=lw,col=clr)
}

