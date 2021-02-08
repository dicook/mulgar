#' Process the output from SOM to display the map and data
#'
#' This function generates a grid of points to match the
#' nodes from the self-organising map (SOM), and jitters points
#' from the data so they can be seen relative to the grid.
#' This allows the clustering of points by SOM to be inspected.
#'
#' @param x multivariate data set
#' @param x_som object returned by som
#' @export
#' @examples
#' cat("make an example\n")
f_som <- function(x, x_som) {
	xmx <- jitter(x.som$visual$x, factor=2)
	xmy <- jitter(x.som$visual$y, factor=2)
	ncols <- ncol(x)
	som_map <- cbind(x, xmx, xmy)
	dimnames(som_map)[[2]][ncols+1] <- "Map 1"
	dimnames(som_map)[[2]][ncols+2] <- "Map 2"
	x_grid <- cbind(x_som$code, x_som$code.sum[,1:2])
	dimnames(x_grid)[[2]] <- dimnames(som_map)[[2]]
	x_clust <- rbind(som_map, x_grid)
	x_clust
}

#' Generate the SOM net
#'
#' Generates the edge set from an SOM model.
#' Combine this with the data matrix to display
#' the model in p-dimensional space.
#'
#' @param x_som object returned by som
#' @export
#' @examples
#' cat("make an example\n")
f_som_net <- function(x_som) {
	x_net <- NULL
	for (i in 1:x_som$xdim) {
		for (j in 1:x_som$ydim) {
			if (j < x_som$ydim)
				x_net <- rbind(x_net, c((i-1)*x_som$xdim + j,
														 (i-1)*x_som$xdim + j + 1))
			if (i < x.som$xdim) x_net <- rbind(x_net,
																		 c((i-1)*x_som$xdim + j,
																		 	i*x_som$xdim + j))
		}
	}
	return(x_net)
}
