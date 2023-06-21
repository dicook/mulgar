#' Process the output from SOM to display the map and data
#'
#' This function generates a grid of points to match the
#' nodes from the self-organising map (SOM), and jitters points
#' from the data so they can be seen relative to the grid.
#' This allows the clustering of points by SOM to be inspected.
#'
#' @param x_som object returned by kohonen::som
#' @param j_val amount of jitter, should range from 0-1, default 0.3
#' @return
#' \itemize{
#' \item data this object contains
#'   \itemize{
#'     \item original variables from the data
#'     \item map1, map2 location of observations in 2D som map, jittered
#'     \item distance distances between observations and the closest node
#'     \item id row id of data
#'   }
#' \item net this object contains
#'   \itemize{
#'     \item values of the nodes in the high-d space
#'     \item map1, map2 nodes of the som net
#'     \item distance distances between observations and the closest node
#'     \item id row id of net
#'   }
#' \item edges from, to specifying row ids of net to connect with lines
#' \item edges_s x, xend, y, yend for segments to draw lines to form 2D map
#' }
#' @export
#' @examples
#' require(kohonen)
#' data(clusters)
#' c_grid <- kohonen::somgrid(xdim = 5, ydim = 5,
#'   topo = 'rectangular')
#' c_som <- kohonen::som(as.matrix(clusters[,1:5]), grid = c_grid)
#' c_data_net <- som_model(c_som)
#' require(ggplot2)
#' ggplot() +
#'   geom_segment(data=c_data_net$edges_s,
#'     aes(x=x, xend=xend, y=y, yend=yend)) +
#'   geom_point(data=c_data_net$data, aes(x=map1, y=map2),
#'     colour="orange", size=2, alpha=0.5)
som_model <- function(x_som, j_val=0.5) {
	data <- data.frame(
		x_som$data[[1]],
		map1 = jitter(x_som$grid$pts[x_som$unit.classif, 1], j_val),
		map2 = jitter(x_som$grid$pts[x_som$unit.classif, 2], j_val),
		distance = x_som$distances,
		id = factor(1:nrow(x_som$data[[1]]))
	)

	net <- data.frame(
		x_som$codes,
		map1 = x_som$grid$pts[, 1],
		map2 = x_som$grid$pts[, 2],
		id = factor(paste("net"), 1:nrow(x_som$grid$pts))
	)
	rownames(net) <- paste("net", 1:nrow(net), sep="")

	xs <- x_som$grid$xdim
	ys <- x_som$grid$ydim
	netlines <- with(expand.grid(y=1:(xs-1), x=1:(ys)), rbind(
		cbind((x - 1) * xs + y, (x - 1)     * xs + y + 1),
		cbind((x - 1) * xs + y,   x         * xs + y)
	))
	netlines <- netlines[-(nrow(netlines):(nrow(netlines)-xs+2)),]
	netlines <- rbind(netlines, cbind(1:(ys-1) * xs, 2:ys * xs))
	colnames(netlines) <- c("from", "to")
	netlines <- data.frame(netlines)

	net_segs <- data.frame(x=net[netlines[,1], "map1"],
												 xend=net[netlines[,2], "map1"],
												 y=net[netlines[,1], "map2"],
												 yend=net[netlines[,2], "map2"])

	return(list(data=data, net=net, edges=netlines, edges_s=net_segs))
}

