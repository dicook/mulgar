#' This function turns a projection sequence into a tibble
#'
#' Take an array of a projection sequence, and turn into
#' a tibble with numbered projections
#'
#' @param t1 tour projection sequence
#' @export
#' @examples
#'
#' require(tourr)
#' t1 <- interpolate(save_history(flea[, 1:6], grand_tour(4), max = 2))
#' tbl1 <- convert_proj_tibble(t1)
convert_proj_tibble <- function(t1) {
	tbl1 <- apply(t1, 2, c)
	tbl1 <- tbl1 |>
		        tibble::as_tibble() |>
		        dplyr::mutate(frame = rep(1:dim(t1)[[3]],
		        rep(dim(t1)[[1]], dim(t1)[[3]])))
	tbl1
}

#' Scale variable to 0-1
#'
#' @noRd
rescale01 <- function(x) {
	rng <- range(x, na.rm = TRUE)
	(x - rng[1]) / (rng[2] - rng[1])
}

#' Scale all numerical variables to 0-1
#'
#' @noRd
rescaler <- function(df) {
	is_numeric <- vapply(df, is.numeric, logical(1))
	df[is_numeric] <- lapply(df[is_numeric], rescale01)
	df
}

#' Standardise variable
#'
#' @noRd
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

