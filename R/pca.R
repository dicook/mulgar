#' This function produces a simple scree plot
#'
#' Takes a PCA object returned by `prcomp()`, extracts
#' the standard deviations of the principal components (PC), and
#' plots these against the PC number.
#'
#' @param pc PCA object
#'
#' @return scree a ggplot object
#' @export
#' @examples
#' data(aflw)
#' aflw_std <- apply(aflw[,7:35], 2, function(x)
#'                     (x-mean(x, na.rm=TRUE))/
#' 							       sd(x, na.rm=TRUE))
#' aflw_pca <- prcomp(aflw_std[,c("goals","behinds",
#'                                "kicks","disposals")])
#' ggscree(aflw_pca)
ggscree <- function(pc) {
  # Check input
	try (if(class(pc) != "prcomp") stop("You need to provide a prcomp object."))

	# Create screeplot from sdev component
  sc_data <- tibble::tibble(n = 1:length(pc$sdev), v = pc$sdev^2)
  scree <- ggplot2::ggplot(sc_data) +
  	ggplot2::geom_line(ggplot2::aes(x=n, y=v)) +
  	ggplot2::geom_point(ggplot2::aes(x=n, y=v)) +
  	ggplot2::xlab("") + ggplot2::ylab("Variance") +
  	ggplot2::ylim(0, max(sc_data$v))
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
#' @export
#' @examples
#' data(plane)
#' plane_pca <- prcomp(plane)
#' plane_m <- pca_model(plane_pca)
#' plane_m_d <- rbind(plane_m$points, plane)
#' if (interactive) {
#'   require(tourr)
#'   animate_xy(plane_m_d, edges=plane_m$edges, axes="bottomleft")
#' }
pca_model <- function(pc, d=2, s=1) {
	# Check input
	try (if (class(pc) != "prcomp") stop("You need to provide a prcomp object."))

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
