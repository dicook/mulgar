# Simulate data for testing
f1 <- runif(100, -3, 3)
f2 <- runif(100, -2, 2)
x1 <- 2*f1+f2+rnorm(100, 0.5)
x2 <- -2*f1-2*f2+rnorm(100, 0.5)
x3 <- f1+2*f2+rnorm(100, 0.5)
x4 <- f1-2*f2+rnorm(100, 0.5)
x5 <- -f1+2*f2+rnorm(100, 0.5)

plane <- data.frame(x1, x2, x3, x4, x5)
save(plane, file="data/plane.rda")

# Look at it
library(tourr)
library(mulgar)
animate_xy(plane)

# Do PCA
plane_pca <- prcomp(plane)
ggscree(plane_pca)

plane_model <- pca_model(plane_pca)

d <- rbind(as.matrix(plane_model$points), as.matrix(plane))
animate_xy(d, edges=plane_model$edges, axes="off")
