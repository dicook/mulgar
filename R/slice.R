#' Generate an axis-parallel slice display
#'
#' Following the slice definition available in `tourr`
#' this function returns a `ggplot2` display of a slice
#' defined via the projection onto two of the variables.
#' Note that because the underlying function works with any
#' projection, the axis labels need to be set by the user.
#'
#' @param data data frame containing only variables used for the display
#' @param h slice thickness
#' @param v1 column number of variable mapped to x-axis
#' @param v2 column number of variable mapped to y-axis
#' @param center center point vector used for anchoring the slice,
#'    if NULL the mean of the data is used
#' @param col grouping vector mapped to color in the display
#'
#' @return ggplot2 object showing the sliced data
#' @export
#' @seealso ggslice_projection
#' @examples
#' d <- geozoo::sphere.hollow(4, 1000)$points
#' ggslice(d, 0.3, 1, 2)
#' ggslice(d, 0.3, 1, 2, center = c(0, 0, 0.7, 0))
ggslice <- function(data, h, v1=1, v2=2, center=NULL,
										col=NULL) {
	plane <- basis_proj(v1, v2, ncol(data))
	ggslice_projection(data, h, plane, center, col)
}

#' Generate slice display
#'
#' @param data data frame containing only variables used for the display
#' @param h slice thickness
#' @param proj projection matrix from p to 2 dimensions
#' @param center center point vector used for anchoring the slice,
#'    if NULL the mean of the data is used
#' @param col grouping vector mapped to color in the display
#'
#' @return ggplot2 object showing the sliced data
#' @export
#' @seealso ggslice
#' @examples
#' d <- geozoo::sphere.hollow(4, 1000)$points
#' ggslice_projection(d, 0.3, tourr::basis_random(4))
#' ggslice_projection(d, 0.3, tourr::basis_random(4),
#'                    center = c(0.4, 0.4, 0.4, 0.4))
ggslice_projection <- function(data, h, proj, center=NULL,
															col=NULL) {
	d <- tourr::anchored_orthogonal_distance(proj, data, anchor = center)
	data_proj <- as.matrix(data) %*% proj
	colnames(data_proj) <- c("P1", "P2")
	data_proj <- tibble::as.tibble(data_proj)
	ggplot2::ggplot(data_proj[d >= h,], ggplot2::aes(x=P1, y=P2)) +
		ggplot2::geom_point(ggplot2::aes(color = col[d >= h]), pch = 46, alpha = 0.5) +
		ggplot2::geom_point(data = data_proj[d < h,],
							 mapping = ggplot2::aes(color = col[d < h])) +
		ggplot2::theme_bw() +
		ggplot2::coord_fixed()
}

#' Generate basis vector
#'
#' @noRd
basis_vector <- function(i, p){
	v <- rep(0, p)
	v[i] <- 1
	v
}

#' Generate axis-parallel projection matrix
#'
#' @noRd
basis_proj <- function(i, j, p){
	matrix(c(basis_vector(i, p), basis_vector(j, p)), ncol=2)
}
