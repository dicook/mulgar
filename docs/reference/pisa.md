# PISA scores

This is data from the 2018 testing, available from
https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip. A subset of the data
containing samples from Australia and Indonesia, and the simulated
scores for math, reading and science.

## Format

A data set with 10548 rows and 31 columns

## Details

- CNT:

  Country (Australia, Indonesia)

- PV1MATH-PV10SCIE:

  simulated scores for math, reading and science

## Examples

``` r

require(dplyr)
data(pisa)
pisa |> count(CNT)
#> Error in order(y): unimplemented type 'list' in 'orderVector1'
```
