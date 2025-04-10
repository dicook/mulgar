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
clusters[1:100, 2] <- clusters[1:100, 2] - 4
clusters[1:100, 4] <- clusters[1:100, 4] - 5
clusters[101:200, 3] <- clusters[101:200, 3] + 2
clusters[101:200, 4] <- clusters[101:200, 4] - 4
clusters[201:300, 5] <- clusters[201:300, 5] - 2
clusters[201:300, 2] <- clusters[201:300, 2] + 5
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

# nonlinear and clusters
set.seed(202300002)
theta = runif(568, 0.20,0.60 * pi)
x = cos(theta) + rnorm(568, 0, 0.03)
y = sin(theta) + rnorm(568, 0, 0.03)

z <- rep(0, 568) + rnorm(568, 0, 0.03)
w <- rep(0, 568) - rnorm(568, 0, 0.03)

df1 <- data.frame(x1 = x, x2 = y, x3 = z, x4 = w)
animate_xy(df1)

theta = runif(601, 0,1.80 * pi)
x = theta
y = sin(theta)

z <- rep(0, 601) + rnorm(601, 1, 0.5)
w <- rep(0, 601) - rnorm(601, 1, 0.03)

df2 <- data.frame(x1 = z, x2 = w, x3 = x, x4 = y)
animate_xy(df2)

df3 <- data.frame(x1=rnorm(57, mean=1, sd=0.1),
									x2=rnorm(57, mean=1, sd=0.1),
									x3=rnorm(57, mean=1, sd=0.1),
									x4=rnorm(57, mean=1, sd=0.1))

df4 <- data.frame(x1=rnorm(42, mean=-1, sd=0.1),
									x2=rnorm(42, mean=-1, sd=0.1),
									x3=rnorm(42, mean=1, sd=0.1),
									x4=rnorm(42, mean=1, sd=0.1))

df <- rbind(df1, df2, df3, df4)
animate_xy(df)

set.seed(42)
df_tsne <- Rtsne(df) #, pca=FALSE, perplexity=30, theta=0.0)
ggplot(as.data.frame(df_tsne$Y), aes(x=V1, y=V2)) +
	geom_point() + theme(aspect.ratio=1)

clusters_nonlin <- df
save(clusters_nonlin, file="data/clusters_nonlin")

# Hiding anomalies
set.seed(946)
d <- tibble(x1=runif(200, -1, 1),
						x2=runif(200, -1, 1),
						x3=runif(200, -1, 1))
d <- d %>%
	mutate(x4 = x3 + runif(200, -0.1, 0.1))
d <- bind_rows(d, c(x1=0, x2=0, x3=-0.5, x4=0.5))

d_r <- d %>%
	mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
				 x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
				 x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
				 x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)

# Check data
library(GGally)
ggscatmat(d)
ggscatmat(d_r)
animate_xy(d)
animate_xy(d_r)

# Save
anomaly1 <- d
anomaly2 <- d_r
save(anomaly1, file="data/anomaly1.rda")
save(anomaly2, file="data/anomaly2.rda")

# 4D sphere with point in middle
library(geozoo)

set.seed(626)
d <- sphere.hollow(p = 4, n = 96)$points |>
	as_tibble() |>
	rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4)
d <- d |>
	bind_rows(c(x1 = 0, x2 = 0, x3 = 0, x4 = 0))
animate_xy(d, axes="off")

# Save
anomaly3 <- d[sample(1:97),]
save(anomaly3, file="data/anomaly3.rda")

# 2D parabola with point
set.seed(946)
d <- tibble(x1=runif(200, -1, 1),
						x2=runif(200, -1, 1),
						x3=runif(200, -1, 1))
d <- d %>%
	mutate(x4 = (x3^2-0.5)*2 + runif(200, -0.1, 0.1))
d <- bind_rows(d, c(x1=0, x2=0, x3=0, x4=0.7))

d_r <- d %>%
	mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
				 x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
				 x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
				 x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)
ggscatmat(d)
ggscatmat(d_r)
animate_xy(d_r, axes="off")

anomaly4 <- d[sample(1:nrow(d)),]
anomaly5 <- d_r[sample(1:nrow(d_r)),]
save(anomaly4, file="data/anomaly4.rda")
save(anomaly5, file="data/anomaly5.rda")

# Simulating different association
set.seed(946)
n <- 136
d <- tibble(x1=runif(n, -1, 1)) |>
	mutate(x2 = 2*x1 + x1^2 + runif(n, -0.1, 0.1),
				 x3 = x1^3 + runif(n, -0.1, 0.1),
				 x4 = 3*x1 - x1^2 + runif(n, -0.1, 0.1)) |>
  mutate(x2 = (x2 - min(x2))/(max(x2) - min(x2)),
  			 x3 = (x3 - min(x3))/(max(x3) - min(x3)),
  			 x4 = (x4 - min(x4))/(max(x4) - min(x4)))

ggscatmat(d)
assoc1 <- d
save(assoc1, file="data/assoc1.rda")

set.seed(741)
n <- 322
d <- tibble(x1=runif(n, -1, 1),
						x2=runif(n, -1, 1),
						x3=runif(n, -1, 1))
d <- d %>%
	mutate(x4 = (x3^2-0.5)*2 + runif(n, -0.1, 0.1))

d_r <- d %>%
	mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
				 x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
				 x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
				 x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)

assoc2 <- d_r
save(assoc2, file="data/assoc2.rda")

set.seed(746)
n <- 576
d <- cube.solid.random(p = 4, n = n)$points
d <- as_tibble(d) |>
	rename(x1 = Var1, x2 = Var2, x3 = Var3, x4 = Var4) |>
	filter(2*x1 + x2 < 1.3) |>
	filter(!between(3*x3 - x4, 0.5, 1.2))

d_r <- d %>%
	mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
				 x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
				 x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
				 x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)

assoc3 <- d_r
save(assoc3, file="data/assoc3.rda")
