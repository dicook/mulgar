#' Generate a dendrogram to be added to data
#'
#' Supplements a data set with information needed to draw a
#' dendrogram. Intermediate cluster nodes are added as needed, and
#' positioned at the centroid of the combined clusters. Note that
#' categorical variables need to be factors.
#'
#' @param data data set
#' @param h an hclust object
#' @param metric distance metric to use, see \code{\link{dist}} for list of
#'   possibilities
#' @param method cluster distance measure to use, see \code{\link{hclust}} for
#'   details
#' @param scale logical value whether to scale data or not, default TRUE
#' @return list with data and edges and segments
#' @keywords cluster
#' @importFrom stats dist hclust median rnorm sd var
#' @export
#' @examples
#' data(clusters)
#' cl_dist <- dist(clusters[,1:5])
#' cl_hw <- hclust(cl_dist, method="ward.D2")
#' require(ggdendro)
#' ggdendrogram(cl_hw, type = "triangle", labels = FALSE)
#' clusters$clw <- factor(cutree(cl_hw, 3))
#' cl_hfly <- hierfly(clusters, cl_hw, scale=FALSE)
#' if (interactive()) {
#'   require(tourr)
#'   glyphs <- c(16, 46)
#'   pch <- glyphs[cl_hfly$data$node+1]
#'   require(colorspace)
#'   clrs <- heat_hcl(length(unique(cl_hfly$data$clw)))
#'   pcol <- clrs[cl_hfly$data$clw]
#'   ecol <- clrs[cl_hfly$data$clw[cl_hfly$edges[,1]]]
#'   animate_xy(cl_hfly$data[,1:5], edges=cl_hfly$edges,
#'     col=pcol, pch=pch, edges.col=ecol,
#'     axes="bottomleft")
#' }
hierfly <- function(data, h=NULL, metric="euclidean", method="ward.D2", scale=TRUE) {
  if (tibble::is.tibble(data)) data <- as.data.frame(data)
	if (scale) data <- rescaler(data)
	id <- 1:nrow(data)
	cat_var <- sapply(data, is.factor)
	if (is.null(h))
		h <- hclust(dist(data[,!cat_var], metric), method)
	data_hc <- data

	data_hc$ORDER <- order(h$order)
	data_hc$HEIGHT <- 0
	data_hc$LEVEL <- 0
	data_hc$POINTS <- 1

	for (i in 1:nrow(h$merge)) {
		newr <- combinerows(data_hc[as.character(-h$merge[i,]),], c(cat_var, FALSE, FALSE, FALSE, FALSE))
		newr$HEIGHT <- h$height[i]
		newr$LEVEL <- i
		rownames(newr) <- as.character(-i)

		data_hc <- rbind(data_hc, newr)
	}
	data_hc$id <- 1:nrow(data_hc)
	data_hc$node <- (as.numeric(rownames(data_hc)) < 0) + 0

	vars <- c("ORDER", "POINTS")

	leaves <- subset(data_hc, node == 0)
	nodes <- subset(data_hc, node == 1)

	edges <- h$merge
	edges[edges > 0] <- edges[edges > 0] + nrow(leaves)
	edges[edges < 0] <- abs(edges[edges < 0])

	segments <- data.frame(x=data_hc[edges[,1],1],
												 xend=data_hc[edges[,2],1],
												 y=data_hc[edges[,1],2],
												 yend=data_hc[edges[,2],2])

	return(list(data=data_hc, edges=edges, segments=segments))
}

#' Join dendrogram data
#'
#' @noRd
combinerows <- function(df, cvar) {
	same <- function(x) if (length(unique(x)) == 1) x[1] else x[2]
	points <- df$POINTS

	# Note that this subsets only the numeric data columns
	# and will generate an error if the data is a tibble
	# because tibble is strict, disallowing subsetting with
	# a less than full column logical vector, cat_var
	cont_var <- as.data.frame(lapply(df[, !cvar, drop=FALSE] * points,
															 sum)) / sum(points)
	cat_var <- as.data.frame(lapply(df[, cvar, drop=FALSE], same))

	df <- if (nrow(cont_var) > 0 && nrow(cat_var) > 0) {
		cbind(cont_var, cat_var)
	} else if (nrow(cont_var) > 0) {
		cont_var
	} else {
		cat_var
	}
	df$POINTS <- sum(points)
	df
}

#' Produces an mclust summary plot with ggplot
#'
#' Takes data returned by `mclustBIC()`, converts to a tibble
#' for plotting.
#'
#' @param mc mclustBIC object
#' @param cl subset of clusters to show
#' @param top number to indicate how many models to show, default "all"
#'
#' @return mc_bic a ggplot object
#' @importFrom methods is
#' @export
#' @examples
#' require(mclust)
#' data(clusters)
#' clusters_BIC <- mclustBIC(clusters[,1:5])
#' ggmcbic(clusters_BIC)
#' ggmcbic(clusters_BIC, cl=3:9)
#' ggmcbic(clusters_BIC, top=4)
#'
#' data(simple_clusters)
#' clusters_BIC <- mclustBIC(simple_clusters[,1:2])
#' ggmcbic(clusters_BIC, cl=2:5, top=3)
ggmcbic <- function(mc, cl=1:nrow(mc), top=ncol(mc)) {
	# Check input
	try (if(!is(mc, "mclustBIC")) stop("You need to provide an mclustBIC object."))

  # Convert to a tibble, and long form
	mc_df <- tibble::as_tibble(unclass(mc))
  mc_df_long <- mc_df |>
  	dplyr::mutate(nc = 1:nrow(mc_df)) |>
  	tidyr::pivot_longer(!nc, names_to = "model", values_to = "BIC_vals")

  # Subset clusters
  if (length(cl) < nrow(mc)) {
  	mc_df_long <- mc_df_long |>
  		dplyr::filter(nc %in% cl)
  }

  # Subset models
  if (top < ncol(mc)) {
    mods <- mc_df_long |>
    	dplyr::arrange(desc(BIC_vals)) |>
    	dplyr::slice_head(n=top) |>
    	dplyr::pull(model) |>
    	unique()
    mc_df_long <- mc_df_long |>
    	dplyr::filter(model %in% mods)
  }

  # Make plot object
  ggmcbic <- ggplot2::ggplot(mc_df_long,
  	ggplot2::aes(x=nc, y=BIC_vals,
  							 colour=model,
  							 linetype=model)) +
  	ggplot2::geom_line() +
  	ggplot2::scale_x_continuous("Number of clusters", breaks=unique(mc_df_long$nc)) +
  	ggplot2::theme(legend.position="bottom")

  return(ggmcbic)
}

#' Computes the ellipses of an mclust model
#'
#' Takes data returned by `Mclust()`, extracts
#' parameter estimates, and computes points on
#' ellipses.
#'
#' @param mc Mclust object
#' @param npts Number of points to simulate for each cluster, default 100
#'
#' @return mc_ellipses data frame
#' @importFrom methods is
#' @export
#' @examples
#' require(mclust)
#' data(simple_clusters)
#' clusters_mc <- Mclust(simple_clusters[,1:2],
#'                      G=2,
#'                      modelname="EEI")
#' mce <- mc_ellipse(clusters_mc, npts=400)
#' require(ggplot2)
#' sc <- simple_clusters
#' sc$cl <- factor(clusters_mc$classification)
#' ggplot() +
#'   geom_point(data=sc, aes(x=x1, y=x2, colour=cl)) +
#'   geom_point(data=mce$ell, aes(x=x1, y=x2, colour=cl), shape=4) +
#'   geom_point(data=mce$mn, aes(x=x1, y=x2, colour=cl), shape=3) +
#'   theme(aspect.ratio=1, legend.position="none")
mc_ellipse <- function(mc, npts=100) {
	# Check input
	try (if(!is(mc, "Mclust")) stop("You need to provide an Mclust object."))

  # Extract parameter estimates
	mn <- mc$parameters$mean
	nvars <- nrow(mn)
	ncl <- ncol(mn)

	# Compute vc ellipses
	ell <- NULL
	for (i in 1:ncl) {
		vc <- mc$parameters$variance$sigma[,,i]
		e <- gen_vc_ellipse(vc, mn[,i], npts)
		colnames(e) <- rownames(mn)
		e <- data.frame(e)
		e$cl <- factor(i)
		ell <- rbind(ell, e)
	}

	mn <- data.frame(t(mn))
	mn$cl <- factor(1:ncl)

	return(list(mn=mn, ell=ell))
}
