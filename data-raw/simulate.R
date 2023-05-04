# load libraries
library(tourr)
library(mulgar)

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
plane <- data.frame(apply(plane, 2, function(x) (x-mean(x))/sd(x)))
plane_pca <- prcomp(plane)
ggscree(plane_pca)
# Look at it
animate_xy(plane)
save(plane, file="data/plane.rda")

x1 <- f1+f2+f3+rnorm(200, 0.5)
x2 <- -f1+f3+rnorm(200, 0.5)
x3 <- f1+rnorm(200, 0.5)
x4 <- f2+rnorm(200, 0.5)
x5 <- f3+rnorm(200, 0.5)
box <- data.frame(x1, x2, x3, x4, x5)
box <- data.frame(apply(box, 2, function(x) (x-mean(x))/sd(x)))

box_pca <- prcomp(box)
ggscree(box_pca)

animate_xy(box)
save(box, file="data/box.rda")

# Check full dimensional
library(geozoo)
cube5d <- data.frame(cube.solid.random(p=5, n=300)$points)
colnames(cube5d) <- paste0("x", 1:5)
cube5d <- data.frame(apply(cube5d, 2, function(x) (x-mean(x))/sd(x)))
c_pca <- prcomp(cube5d)
ggscree(c_pca)

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

# Non-linear association
f1 <- runif(100, -3, 3)
f2 <- runif(100, -2, 2)
f3 <- runif(100, -2.5, 2.5)
x1 <- 2*f1+f2+rnorm(100, 0.5)
x2 <- -2*f1^2-2*f2^2+rnorm(100, 0.5)
x3 <- f1^2+2*f2+rnorm(100, 0.5)
x4 <- f1-2*f2+rnorm(100, 0.5)
x5 <- -f1^3+2*f2+rnorm(100, 0.5)
plane_nonlin <- data.frame(x1, x2, x3, x4, x5)
plane_nonlin <- data.frame(apply(plane_nonlin, 2, function(x) (x-mean(x))/sd(x)))
animate_xy(plane_nonlin)
ggscatmat(plane_nonlin)

plane_nonlin_pca <- prcomp(plane_nonlin)
ggscatmat(plane_nonlin_pca$x)
plane_nonlin_pca <- prcomp(plane_nonlin)
ggscatmat(plane_nonlin_pca$x)
ggscree(plane_nonlin_pca)
save(plane_nonlin, file="data/plane_nonlin.rda")

# Simulate clustered data, 5D, three clusters
clusters <- matrix(rnorm(100*3*5), ncol=5)
#clusters[1:100, 1] <- clusters[1:100, 1] - 3
clusters[1:100, 2] <- clusters[1:100, 2] - 4
#clusters[1:100, 3] <- clusters[1:100, 3] - 2
clusters[1:100, 4] <- clusters[1:100, 4] - 5
clusters[101:200, 3] <- clusters[101:200, 3] + 2
clusters[101:200, 4] <- clusters[101:200, 4] - 4
clusters[201:300, 5] <- clusters[201:300, 5] - 2
#clusters[201:300, 1] <- clusters[201:300, 1] + 2
clusters[201:300, 2] <- clusters[201:300, 2] + 5
#clusters[201:300, 5] <- clusters[201:300, 5] + 2
clusters <- apply(clusters, 2, function(x) (x-mean(x))/sd(x))
clusters <- as.data.frame(clusters)
colnames(clusters) <- paste0("x",1:5)

# Check
library(GGally)
ggpairs(clusters)
cl_pca <- prcomp(clusters)
ggscree(cl_pca)
ggpairs(data.frame(cl_pca$x))
animate_xy(clusters, axes="off")
animate_xy(clusters, guided_tour(holes()), sphere = TRUE)
cl <- c(rep("A", 100), rep("B", 100), rep("C", 100))
animate_xy(clusters, guided_tour(holes()), sphere = TRUE, col=cl)
animate_xy(clusters, guided_tour(lda_pp(cl)), col=cl, sphere = TRUE)
animate_xy(clusters, guided_tour(lda_pp(cl)), col=cl)

clusters$cl <- cl
save(clusters, file="data/clusters.rda")

simple_clusters <- data.frame(x1=c(rnorm(73), rnorm(64, mean=5)),
															x2=c(rnorm(73), rnorm(64, mean=5)),
															cl = factor(c(rep("A", 73), rep("B", 64))))
simple_clusters <- simple_clusters %>%
	mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
ggplot(simple_clusters, aes(x=X1, y=X2)) + geom_point()
save(simple_clusters, file="simple_clusters.rda")

cl_hw <- hclust(dist(simple_clusters[,1:2]),
								method="ward.D2")
cl_hfly <- hierfly(simple_clusters, cl_hw, scale=FALSE)
debug(hierfly)
