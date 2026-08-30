# Generate an axis-parallel slice display

Following the slice definition available in `tourr` this function
returns a `ggplot2` display of a slice defined via the projection onto
two of the variables. Note that because the underlying function works
with any projection, the axis labels need to be set by the user.

## Usage

``` r
ggslice(data, h, v1 = 1, v2 = 2, center = NULL, col = NULL)
```

## Arguments

- data:

  data frame containing only variables used for the display

- h:

  slice thickness

- v1:

  column number of variable mapped to x-axis

- v2:

  column number of variable mapped to y-axis

- center:

  center point vector used for anchoring the slice, if NULL the mean of
  the data is used

- col:

  grouping vector mapped to color in the display

## Value

ggplot2 object showing the sliced data

## See also

ggslice_projection

## Examples

``` r
d <- geozoo::sphere.hollow(4, 1000)$points
ggslice(d, 0.3, 1, 2)
#> Warning: `as.tibble()` was deprecated in tibble 2.0.0.
#> ℹ Please use `as_tibble()` instead.
#> ℹ The signature and semantics have changed, see `?as_tibble`.
#> ℹ The deprecated feature was likely used in the mulgar package.
#>   Please report the issue at <https://github.com/dicook/mulgar/issues>.

ggslice(d, 0.3, 1, 2, center = c(0, 0, 0.7, 0))
```
