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

#' AFLW player statistics
#'
#' This is data from the 2021 Women's Australian Football League.
#' These are player statistics for the matches that they
#' participated in, as provided by the [fitzRoy]() package. Note
#' that the id for a player is unique, but their names might differ
#' across matches due to marriage, or slight errors in spelling ornicknames.
#'
#' \itemize{
#' id, given_name, surname, number, position, team: player identification details
#' time_pct, ..., clearances: player statistics for the match
#' round, date, day, hour: timing of match
#' venue, home_team, away_team: location of match
#' provider: identifier for record provider
#' }
#'
#' @name aflw player statistics
#' @aliases aflw
#' @docType data
#' @format A
#' @keywords datasets
#' @examples
#'
#' require(dplyr)
#' data(aflw)
#' glimpse(aflw)
NULL
