# Data sets generated using copulas

Simulated data from covsim, using different copula models

## Usage

``` r
copclayton

copjoe

copfrank

copnorm
```

## Format

A datasets with various association

## Details

- x1, x2, x3, x4, x5:

  numeric variables

## Examples

``` r
require(GGally)
data(copclayton)
ggscatmat(copclayton)
```
