# Simulate data for testing
f1 <- runif(100, -3, 3)
f2 <- runif(100, -2, 2)
f3 <- runif(100, -2.5, 2.5)
x1 <- 2*f1+f2+rnorm(100, 0.5)
x2 <- -2*f1-2*f2+rnorm(100, 0.5)
x3 <- f1+2*f2+rnorm(100, 0.5)
x4 <- f1-2*f2+rnorm(100, 0.5)
x5 <- -f1+2*f2+rnorm(100, 0.5)

plane <- data.frame(x1, x2, x3, x4, x5)
# Look at it
library(tourr)
library(mulgar)
animate_xy(plane)
save(plane, file="data/plane.rda")

x1 <- f1+f2+f3+rnorm(200, 0.5)
x2 <- -f1+f3+rnorm(200, 0.5)
x3 <- f1+f3+rnorm(200, 0.5)
x4 <- f2-f3+rnorm(200, 0.5)
x5 <- -f1+f2+rnorm(200, 0.5)
box <- data.frame(x1, x2, x3, x4, x5)
animate_xy(box)
save(box, file="data/box.rda")

# Do PCA
plane_pca <- prcomp(plane)
ggscree(plane_pca)

box_pca <- prcomp(box)
ggscree(box_pca)

# Make model component
plane_model <- pca_model(plane_pca)

# Add data, and examine
d <- rbind(plane_model$points, plane)
animate_xy(d, edges=plane_model$edges, axes="bottomleft")

# Make model component
box_model <- pca_model(box_pca, d=3)

# Add data, and examine
d <- rbind(box_model$points, box)
animate_xy(d, edges=box_model$edges, axes="bottomleft")