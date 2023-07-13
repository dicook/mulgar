#' This function generates an axis-parallel slice display
#'
#' more info on the function
#'
#' @param data data frame containing only variables used for the display
#' @param h slice thickness
#' @param v1 column number of variable mapped to x-axis
#' @param v2 column number of variable mapped to y-axis
#' @param center center point vector used for anchoring the slice,
#'    if NULL the mean of the data is used
#' @param col grouping vector mapped to color in the display
#'
#' @return ggplot object showing the sliced data
#' @export
#' @examples
#' d <- geozoo::sphere.hollow(4, 1000)$points
#' ggslice(d, 0.3, 1, 2)
#' ggslice(d, 0.3, 1, 2, center = c(0, 0, 0.7, 0))


ggslice <- function(data, h, v1=1, v2=2, center=NULL,
										col=NULL) {
	plane <- basis_proj(v1, v2, ncol(data))
	ggslice_projection(data, h, plane, center, col)
}

ggslice_projection <- function(data, h, proj, center=NULL,
															col=NULL) {
	d <- tourr::anchored_orthogonal_distance(proj, data, anchor = center)
	data_proj <- as.matrix(data) %*% proj
	colnames(data_proj) <- c("P1", "P2")
	data_proj <- tibble::as.tibble(data_proj)
	ggplot2::ggplot(data_proj[d >= h,], aes(P1, P2)) +
		geom_point(aes(color = col[d >= h]), pch = 46, alpha = 0.5) +
		geom_point(data = data_proj[d < h,],
							 mapping = aes(color = col[d < h])) +
		theme_bw() +
		coord_fixed()
}

basis_vector <- function(i, p){
	v <- rep(0, p)
	v[i] <- 1
	v
}
basis_proj <- function(i, j, p){
	matrix(c(basisVector(i, p), basisVector(j, p)), ncol=2)
}
