#' PISA scores
#'
#' This is data from the 2018 testing, available from
#' https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip.
#' A subset of the data containing only
#' Australia and Indonesia, and the simulated
#' scores for math, reading and science.
#'
#' \itemize{
#' \item CNT Country (Australia, Indonesia)
#' \item PV1MATH-PV10SCIE simulated scores for math, reading and science
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

#' Australian bushfires 2019-2020
#'
#' This data was collated by Weihao (Patrick) Li as part of his
#' Honours research at Monash University. It contains fire
#' ignitions as detected from satellite hotspots, and processed
#' using the [spotoroo](https://tengmcing.github.io/spotoroo/) package, augmented with
#' measurements on weather, vegetation, proximity to human activity. The
#' cause variable is predicted based on historical fire ignition data
#' collected by County Fire Authority personnel.
#'
#' \itemize{
#' id, lon, lat, time unique ide, and spatiotemporal information for each fire ignition
#' FOR_CODE, FOR_TYPE, COVER, HEIGHT, FOREST vegetation variables
#' rf, arf7-arf720 average rainfall, on that day, and over last 7, ..., 720 days
#' se, ase7-ase720 solar exposure, on that day, and over last 7, ..., 720 days
#' maxt, amaxt7-amaxt720 max temperature, on that day, and over last 7, ..., 720 days
#' mint, amint7-amint720 min temperature, on that day, and over last 7, ..., 720 days
#' ws, aws_m0-aws_m24 average wind speed, on that day, and for last 1-24 months
#' dist_road, log_dist_road distance to nearest road
#' dist_cfa, log_dist_cfa distance to neaest county fire authority facility
#' dist_camp, log_dist_camp distance to nearest camp site
#' cause predicted ignition cause, accident, arson, burning_off, lightning
#' }
#'
#' @name bushfires
#' @docType data
#' @format A
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(bushfires)
#' glimpse(bushfires)
NULL

#' 2D plane in 5D
#'
#' This data is simulated to use for testing.
#' It has two dimensions of variability and
#' three of noise. It is created from a 2 factor
#' model, where all variables are related.
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
#' model. All variables are linearly associated.
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

#' Non-linear relationship in 5D
#'
#' This data is simulated to use for testing.
#' It has three dimensions of variability and
#' two of noise. It is created from a 2 factor
#' non-linear model. All variables are associated.
#'
#' \itemize{
#' x1, x2, x3, x4, x5 are five variables
#' }
#'
#' @name plane_nonlin
#' @aliases plane_nonlin
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso plane, box
#' @examples
#' plane_nonlin_pca <- prcomp(plane_nonlin)
#' ggscree(plane_nonlin_pca)
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

#' Multiple clusters of different sizes, shapes and distance from each other
#'
#' This data is originally from http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt,
#' and provided as a challenge for non-linear dimension reduction.It was used
#' as an example in Lee, Laa, Cook (2023) https://doi.org/10.52933/jdssv.v2i3.
#'
#' \itemize{
#' group cluster label
#' X1, ... X10 are variables
#' }
#'
#' @name multicluster
#' @docType data
#' @format A
#' @keywords datasets
#' @seealso clusters
#' @examples
#' require(ggplot2)
#' ggplot(simple_clusters, aes(x=X1, y=X2)) +
#'   geom_point() + theme(aspect.ratio=1)
NULL
