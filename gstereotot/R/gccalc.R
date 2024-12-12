#' Centre and radius of circle thru 3 points on the
#' equatorial plane specified in xymat
#'
#' @param xymat
#'
#' @return c(gc,ra) gc:coordinates of centre. ra:radius
#'
gccalc=function(xymat)
{x1=xymat[1,1]
y1=xymat[1,2]
x2=xymat[2,1]
y2=xymat[2,2]
x3=xymat[3,1]
y3=xymat[3,2]
a=rbind(c(2*(x2-x1),2*(y2-y1)),
        c(2*(x3-x1),2*(y3-y1)))
b=rbind(y2^2-y1^2+x2^2-x1^2,
        y3^2-y1^2+x3^2-x1^2)
gc=solve(a,b)
ra=sqrt((x1-gc[1])^2+(y1-gc[2])^2)
return(c(gc,ra))
}
