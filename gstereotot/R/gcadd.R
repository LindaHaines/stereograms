#' Draws circle on plane perpendicular to x3
#'
#' @param rad Radius of circle to be drawn rad < r
#' @param lw  Line width for circle
#' @param clr Colour of the circle for circle
#' @param r   Radius of stereogram
#'
gcadd=function(rad,lw,clr,r)
{
  n=1000
  cx=rad*cos(seq(0,2*pi,by=pi/n))
  cy=rad*sin(seq(0,2*pi,by=pi/n))
  lines(r*cx,r*cy,type="l",lwd=lw,col=clr)
}
