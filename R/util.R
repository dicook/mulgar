#' This function turns a projection sequence into a tibble
#'
#' Take an array of a projection sequence, and turn into
#' a tibble with numbered projections
#'
#' @param t1 tour projection sequence
#' @export
#' @examples
#'
#' library(tourr)
#' t1 <- interpolate(save_history(flea[, 1:6], grand_tour(4), max = 2))
#' tbl1 <- convert_proj_tibble(t1)
convert_proj_tibble <- function(t1) {
	tbl1 <- apply(t1, 2, c)
	tbl1 <- tbl1 %>%
		        as_tibble() %>%
		        mutate(frame = rep(1:dim(t1)[[3]],
		        rep(dim(t1)[[1]], dim(t1)[[3]])))
	tbl1
}
