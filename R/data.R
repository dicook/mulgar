#' PISA scores
#'
#' This is data from the 2018 testing, available from
#' https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip.
#' A subset of the data containing only
#' Australia and Indonesia, and the simulated
#' scores for math, reading and science.
#'
#' \describe{
#' \item{CNT}{Country (Australia, Indonesia)}
#' \item{PV1MATH-PV10SCIE}{simulated scores for math, reading and science}
#' }
#'
#' @name pisa
#' @docType data
#' @format A data set with 26371 rows and 31 columns
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
#' \describe{
#' \item{id, given_name, surname, number, position, team}{player identification details}
#' \item{time_pct, ..., clearances}{player statistics for the match}
#' }
#'
#' @name aflw
#' @docType data
#' @format A dataset with 381 rows and 35 columns
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
#' \describe{
#' \item{id, lon, lat, time}{unique id, and spatiotemporal information for each fire ignition}
#' \item{FOR_CODE, FOR_TYPE, COVER, HEIGHT, FOREST}{vegetation variables}
#' \item{rf, arf7-arf720}{average rainfall, on that day, and over last 7, ..., 720 days}
#' \item{se, ase7-ase720}{solar exposure, on that day, and over last 7, ..., 720 days}
#' \item{maxt, amaxt7-amaxt720}{max temperature, on that day, and over last 7, ..., 720 days}
#' \item{mint, amint7-amint720}{min temperature, on that day, and over last 7, ..., 720 days}
#' \item{ws, aws_m0-aws_m24}{average wind speed, on that day, and for last 1-24 months}
#' \item{dist_road, log_dist_road}{distance to nearest road}
#' \item{dist_cfa, log_dist_cfa}{distance to nearest county fire authority facility}
#' \item{dist_camp, log_dist_camp}{distance to nearest camp site}
#' \item{cause}{predicted ignition cause, accident, arson, burning_off, lightning}
#' }
#'
#' @name bushfires
#' @docType data
#' @format A dataset with 1021 rows and 60 columns
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(bushfires)
#' glimpse(bushfires)
NULL

#' Images of sketches for training
#'
#' This data is a subset of images from https://quickdraw.withgoogle.com
#' The subset was created using the quickdraw R package at
#' https://huizezhang-sherry.github.io/quickdraw/.
#' It has 6 different groups: banana, boomerang, cactus, flip flops,
#' kangaroo. Each image is 28x28 pixels. This data would be
#' used to train a classification model.
#'
#' \describe{
#' \item{V1-V784}{grey scale 0-255}
#' \item{word}{what the person was asked to draw}
#' \item{id}{unique id for each sketch}
#' }
#'
#' @name sketches_train
#' @docType data
#' @format A data frame with 5998 rows and 786 columns
#' @keywords datasets
#' @examples
#'
#' require(ggplot2)
#' data("sketches_train")
#' x <- sketches_train[sample(1:nrow(sketches_train), 1), ]
#' # print(x$word)
#' xm <- data.frame(gry=t(as.matrix(x[,1:784])),
#'         x=rep(1:28, 28),
#'         y=rep(28:1, rep(28, 28)))
#' ggplot(xm, aes(x=x, y=y, fill=gry)) +
#'   geom_tile() +
#'   scale_fill_gradientn(colors = gray.colors(256, start = 0, end = 1, rev = TRUE )) +
#'   theme_void() + theme(legend.position="none")
NULL

#' Images of sketches for testing
#'
#' This data is a subset of images from https://quickdraw.withgoogle.com
#' The subset was created using the quickdraw R package at
#' https://huizezhang-sherry.github.io/quickdraw/.
#' It has 6 different groups: banana, boomerang, cactus, flip flops,
#' kangaroo. Each image is 28x28 pixels.
#'
#' \describe{
#' \item{V1-V784}{grey scale 0-255}
#' \item{word}{all NA, you need to predict this}
#' \item{id}{unique id for each sketch}
#' }
#'
#' @name sketches_test
#' @docType data
#' @format A data frame with 1200 rows and 786 columns
#' @keywords datasets
#' @seealso sketches_train
#' @examples
#'
#' require(ggplot2)
#' data("sketches_test")
#' x <- sketches_test[sample(1:nrow(sketches_test), 1), ]
#' xm <- data.frame(gry=t(as.matrix(x[,1:784])),
#'         x=rep(1:28, 28),
#'         y=rep(28:1, rep(28, 28)))
#' ggplot(xm, aes(x=x, y=y, fill=gry)) +
#'   geom_tile() +
#'   scale_fill_gradientn(colors = gray.colors(256, start = 0, end = 1, rev = TRUE )) +
#'   theme_void() + theme(legend.position="none")
NULL


#' 2D plane in 5D
#'
#' This data is simulated to use for testing.
#' It has two dimensions of variability and
#' three of noise. It is created from a 2 factor
#' model, where all variables are related.
#'
#' \describe{
#' \item{x1, x2, x3, x4, x5}{five numeric variables}
#' }
#'
#' @name plane
#' @aliases plane
#' @docType data
#' @format A data set with 100 rows and 5 columns
#' @keywords datasets
#' @seealso box
#' @examples
#'
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
#' \describe{
#' \item{x1, x2, x3, x4, x5}{five numeric variables}
#' }
#'
#' @name box
#' @aliases box
#' @docType data
#' @format A dataset with 200 rows and 5 columns
#' @keywords datasets
#' @seealso plane
#' @examples
#'
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
#' \describe{
#' \item{x1, x2, x3, x4, x5}{five numeric variables}
#' }
#'
#' @name plane_nonlin
#' @aliases plane_nonlin
#' @docType data
#' @format A dataset with 100 rows and 5 columns
#' @keywords datasets
#' @seealso plane, box
#' @examples
#'
#' plane_nonlin_pca <- prcomp(plane_nonlin)
#' ggscree(plane_nonlin_pca)
NULL

#' Three clusters in 5D
#'
#' This data is simulated to use for testing.
#' It has three elliptical clusters in mostly
#' variables 2 and 4. They are not equidistant.
#'
#' \describe{
#' \item{x1, x2, x3, x4, x5}{five numeric variables}
#' \item{cl}{class variable}
#' }
#'
#' @name clusters
#' @docType data
#' @format A dataset with 300 rows and 6 columns
#' @keywords datasets
#' @seealso simple_clusters
#' @examples
#'
#' clusters_pca <- prcomp(clusters[,1:5])
#' ggscree(clusters_pca)
NULL

#' Two clusters in 2D
#'
#' This data is simulated to use for testing.
#' It has two spherical clusters, and two variables.
#'
#' \describe{
#' \item{x1, x2}{two numeric variables}
#' \item{cl}{class variable}
#' }
#'
#' @name simple_clusters
#' @docType data
#' @format A dataset with 137 rows and 3 columns
#' @keywords datasets
#' @seealso clusters
#' @examples
#'
#' require(ggplot2)
#' ggplot(simple_clusters, aes(x=x1, y=x2)) +
#'   geom_point() + theme(aspect.ratio=1)
NULL

#' Multiple clusters of different sizes, shapes and distance from each other
#'
#' This data is originally from http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt,
#' and provided as a challenge for non-linear dimension reduction.It was used
#' as an example in Lee, Laa, Cook (2023) https://doi.org/10.52933/jdssv.v2i3.
#'
#' \describe{
#' \item{group}{cluster label}
#' \item{x1, ... x10}{numeric variables}
#' }
#'
#' @name multicluster
#' @docType data
#' @format A dataset with 400 rows and 11 columns
#' @keywords datasets
#' @seealso clusters
#' @examples
#'
#' require(ggplot2)
#' ggplot(multicluster, aes(x=x1, y=x2)) +
#'   geom_point() + theme(aspect.ratio=1)
NULL

#' Four unusually shaped clusters in 4D
#'
#' This data is simulated to use for testing.
#' It has two small spherical clusters, and
#' a curve cluster and a sine wave cluster.
#'
#' \describe{
#' \item{x1, x2, x3, x4}{five numeric variables}
#' }
#'
#' @name clusters_nonlin
#' @docType data
#' @format A dataset with 300 rows and 6 columns
#' @keywords datasets
#' @seealso clusters
#' @examples
#'
#' require(ggplot2)
#' ggplot(clusters_nonlin, aes(x=x1, y=x2)) +
#'   geom_point() +
#'   theme(aspect.ratio=1)
NULL

#' Cluster challenge data sets
#'
#' Simulated data with different structures
#'
#' \describe{
#' \item{x1, x2, ... }{numeric variables}
#' }
#'
#' @name clusterchallenges
#' @docType data
#' @format A datasets with differing number of rows and columns
#' @keywords datasets
#' @source Created by Di Cook.
#' @examples
#' require(ggplot2)
#' data(c1)
#' ggplot(c1, aes(x=x1, y=x2)) +
#'   geom_point() + theme(aspect.ratio=1)
NULL

#' @rdname clusterchallenges
#' @format NULL
"c1"

#' @rdname clusterchallenges
#' @format NULL
"c2"

#' @rdname clusterchallenges
#' @format NULL
"c3"

#' @rdname clusterchallenges
#' @format NULL
"c4"

#' @rdname clusterchallenges
#' @format NULL
"c5"

#' @rdname clusterchallenges
#' @format NULL
"c6"

#' @rdname clusterchallenges
#' @format NULL
"c7"

#' Data sets with anomalies
#'
#' Simulated data with anomalies
#'
#' \describe{
#' \item{x1, x2, x3, x4}{numeric variables}
#' }
#'
#' @name anomalies
#' @docType data
#' @format A datasets with anomalies
#' @keywords datasets
#' @source Created by Di Cook.
#' @examples
#' require(GGally)
#' data(anomaly1)
#' data(anomaly2)
#' ggscatmat(anomaly1)
#' ggscatmat(anomaly2)
NULL

#' @rdname anomalies
#' @format NULL
"anomaly1"

#' @rdname anomalies
#' @format NULL
"anomaly2"

#' @rdname anomalies
#' @format NULL
"anomaly3"

#' @rdname anomalies
#' @format NULL
"anomaly4"

#' @rdname anomalies
#' @format NULL
"anomaly5"

#' Data sets with different types of association
#'
#' Simulated data with various associations
#'
#' \describe{
#' \item{x1, x2, x3, x4}{numeric variables}
#' }
#'
#' @name associations
#' @docType data
#' @format A datasets with various association
#' @keywords datasets
#' @examples
#' require(GGally)
#' data(assoc1)
#' ggscatmat(assoc1)
NULL

#' @rdname associations
#' @format NULL
"assoc1"

#' @rdname associations
#' @format NULL
"assoc2"

#' @rdname associations
#' @format NULL
"assoc3"
