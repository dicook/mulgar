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
#' pca <- prcomp(aflw[,7:10])
#' scree <- ggscree(pca)
#' scree
ggscree <- function(pc) {
  sc_data <- tibble::tibble(n = 1:length(pc$sdev), v = pc$sdev^2)
  scree <- ggplot2::ggplot(sc_data) +
  	ggplot2::geom_line(ggplot2::aes(x=n, y=v)) +
  	ggplot2::geom_point(ggplot2::aes(x=n, y=v)) +
  	xlab("") + ylab("Variance")
  scree

}
