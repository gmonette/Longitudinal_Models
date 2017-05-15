#'
#' On linux see:
#' 
#' 1. [Installing Stan on Linux](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux#r)
#'

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)
cat("\nCXXFLAGS+=-flto -ffat-lto-objects  -Wno-unused-local-typedefs", 
    file = M, sep = "\n", append = TRUE)
cat(readLines(M), sep = '\n')
Sys.setenv(MAKEFLAGS='-j16')
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)

# restart and:
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )
fx( 2L, 5 ) # should be 10
fx( 2L, 5L ) # should be 10

#'
#' [](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan)
#' 
library('rstan')
#'
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model <- "
// saved as 8schools.stan
data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real<lower=0> tau;
  real eta[J];
}
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * eta[j];
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
"
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = model, data = schools_dat, 
            iter = 2000, chains = 4)

system.time(
fit <- stan(model_code = model, data = schools_dat, 
            iter = 2000, chains = 4,
            control = list(adapt_delta=0.99))
)
#' 
#' [Divergent 
#' transitions](http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup)
#' 
#' Quote:
#' 
#' As the warning message says, you should call pairs() on the resulting object.
#' Red points indicate divergent transitions. In our experience, divergent 
#' transitions that occur above the diagonal of the pairs() plot — meaning that 
#' the amount of numerical error was above the median over the iterations — can 
#' often be eliminated simply by increasing the value of the adapt_delta 
#' parameter (see below for example code). This is the target average proposal 
#' acceptance probability during Stan’s adaptation period, and increasing it 
#' will force Stan to take smaller steps. The downside is that sampling will 
#' tend to be slower because a smaller step size means that more steps are 
#' required. Since the validity of the estimates is not guaranteed if there are 
#' post-warmup divergences, the slower sampling is a minor cost.
#' 
#' Conversely, divergent transitions that occur below the diagonal of the
#' pairs() plot — meaning that the amount of numerical error was below the
#' median over the iterations — often cannot be eliminated simply by increasing
#' the value of the adapt_delta parameter, although it does not hurt to try and
#' it might actually work if the number of divergent transitions is small. If
#' the divergent transitions cannot be eliminated by increasing the  adapt_delta
#' parameter, we have to find a different way to write the model that is
#' logically equivalent but simplifies the geometry of the posterior
#' distribution. This problem occurs frequently with hierarchical models and one
#' of the simplest examples is Neal’s Funnel, which is discussed in the
#' Optimizing Stan Code chapter of the Stan manual and can be performed by
#' calling
#' 
library(rstan)
funnel <- stan_demo("funnel", seed = 12345)   # has 5 divergent transitions
pairs(funnel, pars = c("y", "x[1]", "lp__"), las = 1) # below the diagonal
funnel_reparam <- stan_demo("funnel_reparam") # has no divergent transitions
#' 
