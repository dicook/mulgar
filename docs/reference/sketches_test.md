# Images of sketches for testing

This data is a subset of images from https://quickdraw.withgoogle.com
The subset was created using the quickdraw R package at
https://huizezhang-sherry.github.io/quickdraw/. It has 6 different
groups: banana, boomerang, cactus, flip flops, kangaroo. Each image is
28x28 pixels.

## Format

A data frame with 1200 rows and 786 columns

## Details

- V1-V784:

  grey scale 0-255

- word:

  all NA, you need to predict this

- id:

  unique id for each sketch

## See also

sketches_train

## Examples

``` r

require(ggplot2)
data("sketches_test")
x <- sketches_test[sample(1:nrow(sketches_test), 1), ]
xm <- data.frame(gry=t(as.matrix(x[,1:784])),
        x=rep(1:28, 28),
        y=rep(28:1, rep(28, 28)))
ggplot(xm, aes(x=x, y=y, fill=gry)) +
  geom_tile() +
  scale_fill_gradientn(colors = gray.colors(256, start = 0, end = 1, rev = TRUE )) +
  theme_void() + theme(legend.position="none")
```
