#' This function produces a simple scree plot
#'
#' Takes a PCA object returned by `prcomp()`, extracts
#' the standard deviations of the principal components (PC), and
#' plots these against the PC number. The guidance line assumes that
#' all of the variables have been standardised prior to PCA.
#'
#' @param pc PCA object
#' @param guide logical whether to compute and add
#'    a typical value of the variance, if the data was full-dimensional
#' @param cumulative logical whether to draw cumulative variance
#'
#' @return scree a ggplot object
#' @importFrom methods is
#' @export
#' @examples
#' data(aflw)
#' aflw_std <- apply(aflw[,7:35], 2, function(x)
#'                     (x-mean(x, na.rm=TRUE))/
#' 							       sd(x, na.rm=TRUE))
#' aflw_pca <- prcomp(aflw_std[,c("goals","behinds",
#'                                "kicks","disposals")])
#' ggscree(aflw_pca)
ggscree <- function(pc, guide=TRUE, cumulative=FALSE) {
  # Check input
	try (if(!is(pc, "prcomp")) stop("You need to provide a prcomp object."))

	# Generate guidance line
  if (guide) {
  	samples_list <- purrr::map(1:100, ~ matrix(rnorm(nrow(pc$x)*ncol(pc$x)), nrow=nrow(pc$x)))
    sdev_samples <- purrr::map(samples_list, ~ prcomp(.)$sdev^2)
    sdev_samples <- matrix(unlist(sdev_samples), nrow=100, byrow=TRUE)
    sdev_guide <- tibble::tibble(
    	n = 1:length(pc$sdev),
      #upper = apply(sdev_samples, 2, function(x) quantile(x, probs=0.95)))
      median = apply(sdev_samples, 2, median))
    if (cumulative) {
    	#cs <- cumsum(sdev_guide$upper)
    	#sdev_guide$upper <- cs/sum(sdev_guide$upper)
    	cs <- cumsum(sdev_guide$median)
    	sdev_guide$median <- cs/sum(sdev_guide$median)
    }
  }

	# Create screeplot from sdev component
  sc_data <- tibble::tibble(n = 1:length(pc$sdev), v = pc$sdev^2)
  if (cumulative) {
  	cs <- cumsum(sc_data$v)
  	sc_data$v <- cs/sum(sc_data$v)
  }

  maxy <- 0
  if (guide) {
  	scree <- ggplot2::ggplot() +
  		ggplot2::geom_line(data=sdev_guide, ggplot2::aes(x=n, y=median),
  											 colour="grey", size=2)
  	  #ggplot2::geom_line(data=sdev_guide, ggplot2::aes(x=n, y=upper),
  	  #									 colour="grey", size=2)
  	maxy <- max(sdev_guide$median)
  } else {
  	scree <- ggplot2::ggplot()
  }
  scree <- scree +
  	ggplot2::geom_line(data=sc_data, ggplot2::aes(x=n, y=v)) +
  	ggplot2::geom_point(data=sc_data, ggplot2::aes(x=n, y=v)) +
  	ggplot2::xlab("") + ggplot2::ylab("Variance") +
  	ggplot2::ylim(0, max(sc_data$v, maxy))

  if (cumulative)
  	scree <- scree + ggplot2::ylab("Cumulative Proportion")

  scree
}

#' Create wire frame of PCA model
#'
#' This function takes the PCA and produces a wire frame
#' of the PCA to examine with the data in a tour. The purpose
#' is to see how well the variance is explained. The model
#' will be centered at the mean, and extend 3 SDs towards the edge
#' of the data, which is assuming that the data is standardised.
#'
#' @param pc PCA object
#' @param d number of dimensions to use, default=2
#' @param s scale model, default=1
#'
#' @return a list of points and edges
#' @importFrom methods is
#' @export
#' @examples
#' data(plane)
#' plane_pca <- prcomp(plane)
#' plane_m <- pca_model(plane_pca)
#' plane_m_d <- rbind(plane_m$points, plane)
#' if (interactive()) {
#'   require(tourr)
#'   animate_xy(plane_m_d, edges=plane_m$edges, axes="bottomleft")
#' }
pca_model <- function(pc, d=2, s=1) {
	# Check input
	try (if (!is(pc, "prcomp")) stop("You need to provide a prcomp object."))

	n <- nrow(pc$x)
	p <- ncol(pc$x)

	# corners are from a cube, eg 2D:
	#          -1, -1
	#          -1, 1
	#           1, -1
	#           1, 1
	cube <- geozoo::cube.iterate(d)
	cube$points <- (cube$points - 0.5)*2

	# Assumed that data provided was standardised
  p_m <- matrix(rep(pc$center, nrow(cube$points)), ncol=p, byrow=TRUE)
  for (j in 1:nrow(cube$points)) {
  	for (k in 1:d) {
      p_m[j,] <- p_m[j,] + t(s*pc$sdev[k]*pc$rotation[,k]*cube$points[j,k])
  	}
  }
  colnames(p_m) <- rownames(pc$rotation)

  # Return data plus model and edges
  return(list(points = p_m, edges = cube$edges))
}
