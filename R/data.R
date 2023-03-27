#' PISA scores
#'
#' This is data from the 2018 testing, available from
#' https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip.
#' A subset of the data has been generated using only
#' Australian scores, and the simulated scores for
#' math, reading and science.
#'
#' \itemize{
#' CNT Country (AUS, IDN)
#' PV1MATH-PV10SCIE simulated scores
#' }
#'
#' @name pisa
#' @docType data
#' @format A
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(pisa)
#' pisa %>% count(CNT)
NULL

#' AFLW player statistics
#'
#' This is data from the 2021 Women's Australian Football League.
#' These are average player statistics across the season, with game statistics
#' provided by the [fitzRoy](https://jimmyday12.github.io/fitzRoy/) package.
#' If you are new to the game of AFL, there is a nice
#' explanation on [Wikipedia](https://en.wikipedia.org/wiki/Women%27s_Australian_rules_football).
#' The primary analysis is to summarise the variation using
#' principal component analysis, which gives information about
#' relationships between the statistics or skills sets common in
#' players. One also might be tempted to cluster the players, but
#' there are no obvious clusters so it could be frustrating. At
#' best one could partition the players into groups, while recognising
#' there are no absolutely distinct and separated groups.
#'
#' \itemize{
#' id, given_name, surname, number, position, team: player identification details
#' time_pct, ..., clearances: player statistics for the match
#' }
#'
#' @name aflw
#' @docType data
#' @format A
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(aflw)
#' glimpse(aflw)
NULL

#' 2D plane in 5D
#'
#' This data is simulated to use for testing.
#' It has two dimensions of variability and
#' three of noise. It is created from a 2 factor
#' model.
#'
#' \itemize{
#' x1, x2, x3, x4, x5 are five variables
#' }
#'
#' @name plane
#' @aliases plane
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso box
#' @examples
#' plane_pca <- prcomp(plane)
#' ggscree(plane_pca)
NULL

#' 3D plane in 5D
#'
#' This data is simulated to use for testing.
#' It has three dimensions of variability and
#' two of noise. It is created from a 3 factor
#' model.
#'
#' \itemize{
#' x1, x2, x3, x4, x5 are five variables
#' }
#'
#' @name box
#' @aliases box
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso plane
#' @examples
#' box_pca <- prcomp(box)
#' ggscree(box_pca)
NULL

#' Three clusters in 5D
#'
#' This data is simulated to use for testing.
#' It has three elliptical clusters in mostly
#' variables 2 and 4. They are not equidistant.
#'
#' \itemize{
#' x1, x2, x3, x4, x5 are five variables
#' }
#'
#' @name clusters
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso simple_clusters
#' @examples
#' clusters_pca <- prcomp(clusters)
#' ggscree(clusters_pca)
NULL

#' Two clusters in 2D
#'
#' This data is simulated to use for testing.
#' It has two spherical clusters, and two variables.
#'
#' \itemize{
#' x1, x2 are variables
#' }
#'
#' @name simple_clusters
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso clusters
#' @examples
#' require(ggplot2)
#' ggplot(simple_clusters, aes(x=X1, y=X2)) +
#'   geom_point() + theme(aspect.ratio=1)
NULL

