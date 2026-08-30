# PISA scores

This is data from the 2018 testing, available from
https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip. A subset of the data
has been generated using only Australian scores, and the simulated
scores for math, reading and science.

## Format

A

## Details

- CNT Country (AUS, IDN) PV1MATH-PV10SCIE simulated scores

## Examples

``` r

require(dplyr)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data(pisa)
pisa %>% count(CNT)
#> # A tibble: 2 × 2
#>   CNT            n
#>   <hvn_lbll> <int>
#> 1 AUS        14273
#> 2 IDN        12098
```
