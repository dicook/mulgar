#' Generate points on the surface of an ellipse
#'
#' This function generates points by transforming points
#' on the surface of a sphere.
#'
#' @param vc symmetric square matrix describing the
#' variance-covariance matrix which defines the shape
#' of the ellipse.
#' @param xm center of the ellipse, a vector of length
#' equal to the dimension of vc
#' @param n number of points to generate
#' @return matrix of size n x p
#' @export
#' @examples
#' require(ggplot2)
#' require(tibble)
#' ell2d <- f_vc_ellipse(vc = matrix(c(4, 2, 2, 6), ncol=2, byrow=T), xm = c(1,1)
#' ell2d <- as_tibble(ell2d)
#' ggplot(ell2d, aes(x = V1, y = V2)) + geom_point()
f_vc_ellipse <- function(vc, xm, n = 500) {
	p <- ncol(vc)
	x <- sphere.hollow(p, n)$points

	evc <- eigen(vc)
	vc2 <- (evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
	x <- x%*%vc2

	x <- x + matrix(rep(xm, each = n), ncol = p)
}
