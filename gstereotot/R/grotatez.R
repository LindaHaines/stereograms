#' Matrix to rotate points on sphere around z-axis
#'
#' @param theta  Angle of rotation
#'
#' @return rmatz Rotation matrix
#'
rmatz=function(theta)
{rmatz=rbind(c(cos(theta),- sin(theta),0),
             c(sin(theta),cos(theta),0),
             c(0,0,1))
return(rmatz)}
