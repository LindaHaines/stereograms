#' Draws axes:
#' Called by grstereo
#'
#' @param r Radius 0 < r <=1
#' @param nda 0 (none), 1 (main only), 2 (main+110 axes)
#'
gaxes=function(r,nda)
{
  # plot main axes
  if(nda>0)
  {ptx=c(0,0)
  pty=c(-1,1)
  lines(r*ptx,r*pty,lwd=0.25)
  ptx=c(-1,1)
  pty=c(0,0)
  lines(r*ptx,r*pty,lwd=0.25)}
  # plot diaginal axes
  if(nda==2)
  {a=1/sqrt(2)
  ptx=c(-a,a)
  pty=c(-a,a)
  lines(r*ptx,r*pty,lwd=0.25)
  ptx=c(a,-a)
  pty=c(-a,a)
  lines(r*ptx,r*pty,lwd=0.25)
  }
}
