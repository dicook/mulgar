#' Normalise a vector to have length 1
#'
#' Returns the normalised vector, where the sum of
#' squares is equal to 1
#'
#' @param x numeric vector
#' @return numeric vector
#' @export
#' @examples
#' x <- rnorm(5)
#' norm_vec(x)
norm_vec <- function(x) {
	x <- x/calc_norm(x)
	x
}

#' Calculate the norm of a vector
#'
#' Returns the square root of the sum of squares
#' of a vector
#'
#' @param x numeric vector
#' @return numeric value
#' @export
#' @examples
#' x <- rnorm(5)
#' calc_norm(x)
calc_norm <- function(x) {
	sqrt(sum(x^2))
}

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
#' @importFrom stats rnorm
#' @export
#' @examples
#' require(ggplot2)
#' d <- mulgar::rmvn(n=100, p=2, mn = c(1,1),
#'                   vc = matrix(c(4, 2, 2, 6),
#'                          ncol=2, byrow=TRUE))
#' ggplot(data.frame(d), aes(x = x1, y = x2)) +
#'   geom_point() + theme(aspect.ratio=1)
rmvn <- function(n=100, p=5, mn=rep(0,p), vc=diag(rep(1,p))) {
	x <-  matrix(rnorm(n*p), ncol=p)
	ev <- eigen(vc)
	vcsqrt <- diag(sqrt(ev$values)) %*% t(ev$vectors)
	x <- x %*% vcsqrt
	x <- x + matrix(rep(mn,n), ncol=p,  byrow=T)
	colnames(x) <- paste0("x", 1:ncol(x))
	return(x)
}

#' Compute Mahalanobis distances between all pairs of observations
#'
#' For a data matrix, compute the sample variance-covariance,
#' which is used to compute the Mahalanobis distance.
#'
#' This is useful for checking distance arise from a
#' multivariate normal sample.
#'
#' @param x multivariate data set
#' @return vector of length n
#' @export
#' @examples
#' require(ggplot2)
#' require(tibble)
#' data(aflw)
#' aflw_std <- apply(aflw[,7:35], 2, function(x)
#'                     (x-mean(x, na.rm=TRUE))/
#' 							       sd(x, na.rm=TRUE))
#' d <- calc_mv_dist(aflw_std[,c("goals","behinds",
#'                                "kicks","disposals")])
#' d <- as_tibble(d, .name_repair="minimal")
#' ggplot(d, aes(x=value)) + geom_histogram()
calc_mv_dist <- function(x){
	n <- dim(x)[1]
	p <- dim(x)[2]
	mn <- apply(x, 2, mean)
	vc <- var(x)
	ev <- eigen(vc)
	vcinv <- ev$vectors %*% diag(1/ev$values) %*% t(ev$vectors)
	x <- x - matrix(rep(mn, n), ncol=p, byrow=T)
	dx <- NULL
	for (i in 1:n)
		dx <- c(dx, x[i,] %*% vcinv %*% as.matrix(x[i,]))
	return(dx)
}

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
#' ell2d <- gen_vc_ellipse(vc = matrix(c(4, 2, 2, 6),
#'                         ncol=2, byrow=TRUE),
#'                         xm = c(1,1))
#' ell2d <- as_tibble(ell2d)
#' ggplot(ell2d, aes(x = V1, y = V2)) + geom_point() +
#'   theme(aspect.ratio=1)
gen_vc_ellipse <- function(vc, xm=rep(0, ncol(vc)), n = 500) {
	p <- ncol(vc)
	x <- geozoo::sphere.hollow(p, n)$points

	evc <- eigen(vc)
	vc2 <- (evc$vectors) %*% diag(sqrt(evc$values)) %*% t(evc$vectors)
	x <- x%*%vc2

	x <- x + matrix(rep(xm, each = n), ncol = p)
	colnames(x) <- colnames(vc)

	return(x)
}

#' Ellipse matching data center and variance
#'
#' This function generates points on the surface of an
#' ellipse with the same center and variance-covariance
#' of the provided data.
#'
#' This is useful for checking the equal variance-covariance
#' assumption from linear discriminant analysis.
#'
#' @param x multivariate data set.
#' @param n number of points to generate
#' @param nstd scale factor for size of ellipse, in terms
#'   of number of standard deviations
#' @return matrix of size n x p
#' @export
#' @examples
#' data(aflw)
#' aflw_vc <- gen_xvar_ellipse(aflw[,c("goals","behinds",
#'                                "kicks","disposals")], n=500)
#' require(ggplot2)
#' ggplot(aflw_vc, aes(x=goals, y=behinds)) + geom_point() +
#'   theme(aspect.ratio=1)
#' if (interactive()) {
#'   require(tourr)
#'   animate_slice(aflw_vc, rescale=TRUE, v_rel=0.02)
#'   aflw_all <- rbind(aflw_vc, aflw[,c("goals","behinds",
#'                                "kicks","disposals")])
#'   clrs <- c(rep("orange", 500), rep("black", nrow(aflw)))
#'   animate_xy(aflw_all, col=clrs)
#' }
gen_xvar_ellipse <- function(x, n=100, nstd=1){
	xm <- apply(x, 2, mean)
	p <- dim(x)[2]
	xn <- dim(x)[1]
	xv <- var(x)
	ev <- eigen(xv)
	sph <- matrix(rnorm(n*p), ncol=p)
	cntr <- t(apply(sph, 1, norm_vec))
	cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors)
	cntr <- nstd*cntr + matrix(rep(xm,n), nrow=n, byrow=T)
	colnames(cntr) <- colnames(x)
	cntr <- as.data.frame(cntr)
	return(cntr)
}

#' Compute pooled variance-covariance matrix
#'
#' This function computes the group variance-covariance
#' matrices, and produces a weighted average. It is useful
#' for examining the linear discriminant analysis model.
#'
#' @param x multivariate data set, matrix.
#' @param cl class variable
#' @param prior prior probability for each class, must sum to 1, default all equal
#' @return matrix
#' @export
#' @examples
#' data(clusters)
#' pooled_vc(clusters[,1:5], clusters$cl)
pooled_vc <- function(x, cl,
		prior=rep(1/length(unique(cl)), length(unique(cl)))) {

	stopifnot(abs(sum(prior)-1) < 0.001)

	classes <- unique(cl)
  ncl <- length(classes)
  vc_all <- matrix(0, ncol(x), ncol(x))
  for (i in 1:ncl) {
    x_sub <- x[cl==classes[i],]
    vc <- data.frame(var(x_sub))
    vc_all <- vc_all + vc*prior[i]
  }

  return(vc_all)
}
