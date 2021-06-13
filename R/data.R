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
#' @name PISA test scores
#' @aliases pisa
#' @docType data
#' @format A
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(pisa)
#' pisa %>% count(CNT)
NULL
