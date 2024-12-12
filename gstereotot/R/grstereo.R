#' Draws circle, axes and labels of a stereogram
#' Calls functions gaxes and greatc
#'
#' @param r   Radius of stereogram
#' @param nda Axes: 0 (none), 1(x,y), 2 (x,y + 110)
#' @param ngc Great circles: 0 (none), 1 (x,y), 2 (x,y+110)
#' @param nbb Labels 0 (x1,x2,x3), 1 (110 added)
#'
#' @examples
#' gstereo(1,1,0,1)
#'
grstereo=function(r,nda,ngc,nbb)
{ gcircle(r,nbb)
  if(nda > 0)
  {gaxes(r,nda)}
  if(ngc > 0)
  {greatc(r,ngc)}
}
