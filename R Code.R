# Name: Chee Kitt Win
# Student Number:45589140

# PROBLEM 1 PART 5 AND 6

# Load libraries
library(Rdimtools)
library(kernlab)

# load data
digits = read.csv("C:\\Users\\Owner\\Desktop\\UQ Year 3 Sem 2 Courses\\STAT3006\\Assignment 4\\Data and Question Sheet\\zip.txt", sep = "", header = FALSE)
xdigit = data.matrix(digits[,2:257])
ydigit = data.matrix(digits[1])

# Conduct PPCA
PPCA <- do.ppca(xdigit, 4)


# Report the MLEs
sigma2_hat = PPCA$mle.sigma2
sigma2_hat
R_hat = PPCA$mle.W
R_hat
mu_hat = colMeans(xdigit)
mu_hat

PPCA$Y

library(fields)
# Plot PPCA
plot(
  as.data.frame(PPCA$Y),
  col = tim.colors(9)[as.factor(ydigit)],
  cex = 1,
  pch = 16
)

# PROBLEM 1 PART 7

# Load library
library(ruta)
Sys.setenv(RETICULATE_PYTHON="/Users/Owner/AppData/Local/Programs/Python/Python39")
# Line above is so that reticulate can find python libraries (in particular tensorflow)

# Declare the autoencoder model
AE <- autoencoder(
  input() + dense(4, 'tanh') + output("tanh"),
  loss = "mean_squared_error"
) 

# Train the autoencoder
xdigit <- data.frame(apply(xdigit, 2, function(x) as.numeric(as.character(x))))
TRAINING <- train(AE,scale(xdigit,scale = FALSE),epoch=50,optimizer = keras::optimizer_nadam())


# Extract the encoding
Encoding <- encode(TRAINING,scale(xdigit,scale = FALSE))

# Plot the encoding
plot(
  as.data.frame(jitter(Encoding)),
  col = tim.colors(9)[as.factor(ydigit)],
  cex = 1,
  pch = 16
)

# You can also plot your network
plot(AE$network)


# PROBLEM 2 PART 1

genes = read.csv("C:\\Users\\Owner\\Desktop\\UQ Year 3 Sem 2 Courses\\STAT3006\\Assignment 4\\Data and Question Sheet\\golub_genes.csv")
genes_labels =  read.csv("C:\\Users\\Owner\\Desktop\\UQ Year 3 Sem 2 Courses\\STAT3006\\Assignment 4\\Data and Question Sheet\\golub_labels.csv")

names = genes[,"X"]
names =  as.list(names)

genes = genes[,-1]
genes = as.data.frame(t(genes))

colnames(genes) = names

genes["x"] = genes_labels["x"]
X<-split(genes, genes$x)
ALL_genes = X$ALL
AML_genes = X$AML
ALL_genes$x = NULL
AML_genes$x = NULL

ALL_genes <- data.frame(apply(ALL_genes, 2, function(x) as.numeric(as.character(x))))
AML_genes <- data.frame(apply(AML_genes, 2, function(x) as.numeric(as.character(x))))

# PROBLEM 2 PART 1

# KMMD
# k = kmmd(as.matrix(ALL_genes[,1:1000]),as.matrix(AML_genes[,1:1000]),kernel = "rbfdot", kpar = list(sigma = 2^(-28)), alpha = 0.1)


### Some C Code Libraries
library(Rcpp)
library(RcppArmadillo)
library(inline)

### HIENS CODE
# Source for the U-statistic estimator of the radial-basis (Gaussian) kernel MMD
U_radialMMD_source <- '
// Set namespaces
using namespace arma;
using namespace Rcpp;
// Load inpute variables
mat data_1 = as<mat>(data_1_r);
mat data_2 = as<mat>(data_2_r);
double sigma_sq = as<double>(sigma_sq_r);
// Get object dimensions
int obs_1 = data_1.n_rows;
int obs_2 = data_2.n_rows;
// Initialize three storage objects
double XX = 0;
double YY = 0;
double XY = 0;
// Compute required terms
for (int ii = 0; ii < obs_1; ii++) {
for (int jj = 0; jj < ii; jj++) {
XX += exp(-pow(norm(data_1.row(ii)-data_1.row(jj),"fro"),2)/2/sigma_sq);
}
}
for (int ii = 0; ii < obs_2; ii++) {
for (int jj = 0; jj < ii; jj++) {
YY += exp(-pow(norm(data_2.row(ii)-data_2.row(jj),"fro"),2)/2/sigma_sq);
}
}
for (int ii = 0; ii < obs_1; ii++) {
for (int jj = 0; jj < obs_2; jj++) {
XY += exp(-pow(norm(data_1.row(ii)-data_2.row(jj),"fro"),2)/2/sigma_sq);
}
}
// Create a result storage
double RESULT = 2*XX/obs_1/(obs_1-1) + 2*YY/obs_2/(obs_2-1) - 2*XY/(obs_1*obs_2);
return Rcpp::List::create(
Rcpp::Named("U_radialMMD") = RESULT);
'

# Computes the U-statistic estimator of the Gaussian kernel
# Input 2 data sets (matrices) and a sigma_sq > 0
U_radialMMD_fun <- cxxfunction(signature(data_1_r='numeric',
                                         data_2_r='numeric',
                                         sigma_sq_r='numeric'),
                               U_radialMMD_source, plugin = 'RcppArmadillo')

# UStat
Ustat <- U_radialMMD_fun(as.matrix(AML_genes), as.matrix(ALL_genes), sigma_sq_r = 2^27)
Ustat
# Compute critical value at level of significance alpha
alpha <- 0.1
min_sample_size <- min(dim(ALL_genes)[1],dim(AML_genes)[1])
# Critical value
Crit <- 2*sqrt(2/floor(min_sample_size/2)*log(1/alpha))
Crit


# PROBLEM 2 PART 2 AND 3
p = vector()
for (i in 1:3571){
  p[i] = t.test(as.vector(ALL_genes[i]),as.vector(AML_genes[i]))$p.value
}
hist(p, breaks = 20, main = "Histogram of p values")
plot(ecdf(p), main = "Empirical Cumulative Distribution of P-values", xlab = "p values", ylab = "cdf")
lines(ecdf(runif(10000)), col = "magenta")
legend(x = 0.7, y = 0.3, legend = "Magenta line is the ecdf of\n the uniform distribution", cex = 0.7)


# PROBLEM 2 PART 4

# Benjamin Yakutieli
BY = p.adjust(p,method =  "BY")
hist(BY, breaks = 20)
plot(ecdf(BY))
lines(ecdf(runif(10000)))
pval_BY = vector()
BY_sig_genes = vector()
for (i in 1:length(BY)){
  if (BY[i] <= 0.05){
    BY_sig_genes = append(BY_sig_genes,i)
    pval_BY = append(pval_BY,p[i])
  }
}
max(pval_BY)
length(BY_sig_genes)

# Benjamin Hochberg
BH = p.adjust(p,method =  "BH")
hist(BH, breaks = 20)
plot(ecdf(BH))
lines(ecdf(runif(10000)))
pval_BH = vector()
BH_sig_genes = vector()
for (i in 1:length(BH)){
  if (BH[i] <= 0.05){
    BH_sig_genes = append(BH_sig_genes,i)
    pval_BH = append(pval_BH,p[i])
  }
}
max(pval_BH)
length(BH_sig_genes)


# PROBLEM 2 PART 5


my_cols <- c("#00AFBB", "#E7B800")  
pairs(genes[,5:8], pch = 19,  cex = 1,
      col = my_cols[as.factor(genes$x)])


# PROBLEM 3 PART 4
library(glmnet)
library(fastDummies)
prostate = read.csv("C:\\Users\\Owner\\Desktop\\UQ Year 3 Sem 2 Courses\\STAT3006\\Assignment 4\\Data and Question Sheet\\prostate.csv")
prostate$svi <- as.factor(prostate$svi)
prostate$gleason <- as.factor(prostate$gleason)
Dummy <- dummy_cols(prostate,remove_first_dummy = TRUE, remove_selected_columns = TRUE)
model = glmnet(x = scale(Dummy[,-7]),
               y = scale(Dummy[7]),
               alpha = 0.5,
               standardize = FALSE)
  
# HIENS CODE
# Plot the elastic net fit
plot(model,lwd=2,xvar='lambda')
grid()
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  legend('topright', legend=labs, col=1:length(labs), lty=1) # <<< ADDED BY ME
}
lbs_fun(model)

model$a0
model$beta

