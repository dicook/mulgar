# Copulas
library(covsim)
library(tidyverse)
library(tourr)
p <- 5
# define a target covariance
# Basically randomise the target covariance bc small sample
#set.seed(308)
set.seed(932)
sigma.target <- cov(MASS::mvrnorm(10, mu=rep(0, p), Sigma=diag(1, p)))
# normal margins that match the covariances:
marginsnorm <- lapply(X=sqrt(diag(sigma.target)), function(X) list(distr="norm", sd=X) )
# Default is clayton
set.seed(826)
clayton.vine <- vita(marginsnorm, sigma.target = sigma.target, Nmax=10^5, cores=1)
copclayton <- rvinecopulib::rvine(467, clayton.vine)
colnames(copclayton) <- paste0("x", 1:5)
copclayton <- as.data.frame(copclayton)
GGally::ggscatmat(copclayton)
cov(copclayton)
sigma.target
animate_xy(copclayton)
save(copclayton, file="data/copclayton.rda")

# joe
set.seed(826)
joe.vine <- vita(marginsnorm,
												sigma.target = sigma.target,
												family_set = "joe",
												Nmax=10^5, cores=1)
copjoe <- rvinecopulib::rvine(467, joe.vine)
colnames(copjoe) <- paste0("x", 1:5)
copjoe <- as.data.frame(copjoe)
GGally::ggscatmat(copjoe)
save(copjoe, file="data/copjoe.rda")

# Doesn't work, think other margins than norm not implemented
set.seed(826)
marginst <- lapply(X=sqrt(diag(sigma.target)), function(X) list(distr="t", sd=X) )
gumbel.vine <- vita(marginst,
								 sigma.target = sigma.target,
								 family_set = "gumbel",
								 Nmax=10^5, cores=1)
copgumbel <- rvinecopulib::rvine(467, gumbel.vine)
copgumbel <- as.data.frame(copgumbel)
GGally::ggscatmat(copgumbel)

# frank
set.seed(826)
frank.vine <- vita(marginsnorm,
										sigma.target = sigma.target,
										family_set = "frank",
										Nmax=10^5, cores=1)
copfrank <- rvinecopulib::rvine(467, frank.vine)
colnames(copfrank) <- paste0("x", 1:5)
copfrank <- as.data.frame(copfrank)
GGally::ggscatmat(copfrank)
save(copfrank, file="data/copfrank.rda")

# norm
set.seed(826)
norm.vine <- vita(marginsnorm,
									 sigma.target = sigma.target,
									 family_set = "gauss",
									 Nmax=10^5, cores=1)
copnorm <- rvinecopulib::rvine(467, norm.vine)
colnames(copnorm) <- paste0("x", 1:5)
copnorm <- as.data.frame(copnorm)
GGally::ggscatmat(copnorm)
save(copnorm, file="data/copnorm.rda")

# Using different margins: code fails!
set.seed(428)
marginsexp <- lapply(X=sqrt(diag(sigma.target)), function(X) list(distr="exp", sd=X) )
calibrated.vine <- vita(marginsexp, sigma.target = sigma.target, Nmax=10^5, cores=1)

marginst <- lapply(X=sqrt(diag(sigma.target)), function(X) list(distr="t", sd=X) )
calibrated.vine <- vita(marginst, sigma.target = sigma.target, Nmax=10^5, cores=1)

# Created with assistance from claude
# Using different copulas for different pairs
library(VineCopula)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create a valid R-vine matrix
create_custom_vine <- function(d, fam=NULL) {
  # Check fam id dxd matrix
  stopifnot(nrow(fam) == d)

  # Create structure matrix (C-vine structure)
  Matrix <- matrix(0, d, d)
  for(i in 1:d) {
    Matrix[i:d, i] <- i:d
  }

  # Create family matrix
  if (is.null(fam)) {
    family <- matrix(0, d, d)
    # Assign different copula families (1:Gaussian, 3:Clayton, 4:Gumbel, 5:Frank)
    for(i in 1:(d-1)) {
      indices <- (i+1):d
      family[indices, i] <- sample(c(1, 3, 4, 5), length(indices), replace = TRUE)
    }
  }
  else
    family <- fam

  # Create parameter matrix
  par <- matrix(0, d, d)
  # Assign parameters based on copula family
  for(i in 1:(d-1)) {
    indices <- (i+1):d
    for(j in seq_along(indices)) {
      idx <- indices[j]
      # Different parameter ranges for different families
      if(family[idx, i] == 1) {  # Gaussian
        par[idx, i] <- runif(1, -0.8, 0.8)  # keeping away from bounds
      } else if(family[idx, i] == 3) {  # Clayton
        par[idx, i] <- runif(1, 0.1, 2)
      } else if(family[idx, i] == 4) {  # Gumbel
        par[idx, i] <- runif(1, 1.1, 2)
      } else if(family[idx, i] == 5) {  # Frank
        par[idx, i] <- runif(1, -4, 4)
      }
    }
  }

  # Create second parameter matrix (needed for t-copula)
  par2 <- matrix(0, d, d)

  # Create RVineMatrix object
  RVM <- RVineMatrix(
    Matrix = Matrix,
    family = family,
    par = par,
    par2 = par2
  )

  return(RVM)
}

# Function to simulate from custom vine copula
simulate_custom_vine <- function(n, vine_matrix) {
  # Simulate from the vine copula
  u_samples <- RVineSim(n, vine_matrix)

  # Transform to normal margins
  x_samples <- qnorm(u_samples)

  # Create data frame
  df <- as.data.frame(x_samples)
  colnames(df) <- paste0("V", 1:ncol(x_samples))

  return(df)
}

# Custom plotting functions for GGally
my_density <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(alpha = 0.5) +
    theme_minimal()
}

my_scatter <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_density_2d(alpha = 0.5) +
    theme_minimal()
}

my_cor <- function(data, mapping, ...) {
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)

  cor_p <- cor(x, y, method = "pearson")
  cor_k <- cor(x, y, method = "kendall")

  ggally_text(
    label = paste("ρ =", round(cor_p, 2), "\nτ =", round(cor_k, 2)),
    mapping = mapping,
    ...
  ) + theme_minimal()
}

# Function to print copula families used
print_copula_structure <- function(RVM) {
  d <- dim(RVM$Matrix)[1]
  copula_types <- c("Independence", "Gaussian", "Student t", "Clayton", "Gumbel", "Frank")

  cat("\nCopula Structure:\n")
  for(i in 1:(d-1)) {
    for(j in (i+1):d) {
      if(RVM$family[j,i] > 0) {
        cat(sprintf("Pair (%d,%d): %s copula (param = %.2f)\n",
                    i, j,
                    copula_types[RVM$family[j,i] + 1],
                    RVM$par[j,i]))
      }
    }
  }
}

# Example usage
set.seed(42)

# Create 5-dimensional example
d <- 5
n_samples <- 2000

# Create custom vine matrix
custom_vine <- create_custom_vine(d)
custom_vine <- create_custom_vine(d, fam=family)

# Print the structure
print_copula_structure(custom_vine)

# Simulate data
simulated_data <- simulate_custom_vine(n_samples, custom_vine)

# Create visualization
p <- ggpairs(simulated_data,
             lower = list(continuous = my_scatter),
             diag = list(continuous = my_density),
             upper = list(continuous = my_cor)) +
  theme_minimal() +
  ggtitle("Mixed Copula Structure") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)
animate_xy(simulated_data)

# Calculate and print summary statistics
summary_stats <- simulated_data %>%
  summarise(across(everything(),
                   list(mean = mean,
                        sd = sd,
                        skew = function(x) moments::skewness(x),
                        kurt = function(x) moments::kurtosis(x)),
                   .names = "{.col}_{.fn}")) %>%
  round(3)

print(summary_stats)

# Using ChatGPT to generate L-shapes
# Comment: It's really NOT an L-shape and is a weird
# way to generate the copula data, I think.
# Load the copula package
library(copula)

# Set the seed for reproducibility
set.seed(1031)

theta <- 5  # Higher theta creates stronger lower tail dependence
clayton_cop <- claytonCopula(param = theta, dim = 2)

# Simulate data from the Clayton copula
n <- 1000
u <- rCopula(n, clayton_cop)  # Uniform [0,1] data on the copula space

# Apply skewed marginal transformations for L-shape
x <- qexp(u[,1], rate = 1)    # Exponential marginal for X
y <- qexp(u[,2], rate = 0.5)  # Exponential marginal for Y (slower decay)

# Combine the data
copula_data <- data.frame(x = x, y = y)

# Plot the L-shaped data
plot(
  copula_data$x, copula_data$y,
  main = "Simulated L-Shaped Data Using Clayton Copula",
  xlab = "X (Exponential Marginal)",
  ylab = "Y (Exponential Marginal)",
  pch = 16,
  col = rgb(0.2, 0.4, 0.8, 0.5)
)

# Using ecoCopula
library(ecoCopula)
data(spider)
X <- spider$x
abund <- spider$abund

# Example 1: Simple example
myfamily <- "negative.binomial"
# Example 1: Funkier example where Species are assumed to have different distributions
# Fit models including all covariates are linear terms, but exclude for bare sand
fit0 <- stackedsdm(abund, formula_X = ~. -bare.sand, data = X, family = myfamily, ncores = 2)

# Example 2: Funkier example where Species are assumed to have different distributions
abund[,1:3] <- (abund[,1:3]>0)*1 # First three columns for presence absence
myfamily <- c(rep(c("binomial"), 3),
              rep(c("negative.binomial"), (ncol(abund)-3)))
fit0 <- stackedsdm(abund, formula_X = ~ bare.sand, data = X, family = myfamily, ncores = 2)

spider_abund = spider$abund

spider_mod_ssdm = stackedsdm(spider_abund,~1, data = spider$x, ncores=2)
spid_lv_ssdm = cord(spider_mod_ssdm)
spider_sim <- simulate(spid_lv_ssdm, nsim=1, seed=1001)
options(digits=2)
cov(abund)
cov(spider_sim)
cor(abund)
cor(spider_sim)

spider_cov <- tibble(orig=as.vector(spider_abund),
                     sim=as.vector(spider_sim))
ggplot(spider_cov, aes(x=orig, y=sim)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(colour="red") +
  xlim(c(0,200)) + ylim(c(0,200)) +
  scale_x_sqrt() +
  scale_y_sqrt()

# These don't appear to match at all

# Re-do to show in tourr
library(ecoCopula)
data(spider)
spider_abund <- spider$abund
animate_xy(spider_abund)
spider_mod_ssdm = stackedsdm(spider_abund,~1, data = spider$x, ncores=2)
spid_lv_ssdm = cord(spider_mod_ssdm)
spider_sim <- simulate(spid_lv_ssdm, nsim=1, seed=1001)
spider_all <- bind_rows(as_tibble(spider_abund),
												as_tibble(spider_sim)) |>
	mutate(gp = factor(c(rep("obs", nrow(spider_abund)),
											 rep("sim", nrow(spider_sim)))))
animate_xy(spider_all[,1:12], col=spider_all$gp)

