# This function turns a projection sequence into a tibble

Take an array of a projection sequence, and turn into a tibble with
numbered projections

## Usage

``` r
convert_proj_tibble(t1)
```

## Arguments

- t1:

  tour projection sequence

## Value

tbl1 tibble

## Examples

``` r

require(tourr)
#> Loading required package: tourr
#> Warning: package ‘tourr’ was built under R version 4.6.1
#> 
#> Attaching package: ‘tourr’
#> The following object is masked from ‘package:GGally’:
#> 
#>     flea
t1 <- interpolate(save_history(flea[, 1:6], grand_tour(4), max = 2))
#> Converting input data to the required matrix format.
tbl1 <- convert_proj_tibble(t1)
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.
#> ℹ The deprecated feature was likely used in the mulgar package.
#>   Please report the issue at <https://github.com/dicook/mulgar/issues>.
```
