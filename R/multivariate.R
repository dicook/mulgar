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
#' ell2d <- f_vc_ellipse(vc = matrix(c(4, 2, 2, 6), ncol=2, byrow=T), xm = c(1,1))
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

#' Normalise a vector to have length 1
f_norm_vec<-function(x) {
	x <- x/f_norm(x)
	x
}

#' Calculate the norm of a vector
f_norm <- function(x) { sqrt(sum(x^2)) }

#' Generate a sample from a multivariate normal
#'
#' This function generates a  sample of size n from a
#' multivariate normal distribution
#'
#' @param n number of points to generate
#' @param p dimension
#' @param mn mean of the distribution, a vector of length
#' equal to the dimension of vc
#' @param vc symmetric square matrix describing the
#' variance-covariance matrix which defines the shape
#' of the ellipse.
#' @return matrix of size n x p
#' @export
#' @examples
#' require(ggplot2)
#' require(tibble)
#' d <- f_gen_mvn(n=100, p=2, mn = c(1,1), vc = matrix(c(4, 2, 2, 6), ncol=2, byrow=T))
#' d <- as_tibble(d)
#' ggplot(d, aes(x = V1, y = V2)) + geom_point()
f_gen_mvn<-function(n=100, p=5, mn=rep(0,p), vc=diag(rep(1,p))) {
	x <-  matrix(rnorm(n*p), ncol=p)
	ev <- eigen(vc)
	vcsqrt <- diag(sqrt(ev$values))%*%t(ev$vectors)
	x <- x%*%vcsqrt
	x <- x + matrix(rep(mn,n), ncol=p,  byrow=T)
	return(x)
}

