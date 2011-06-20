\name{hglm2}
\alias{hglm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fitting Hierarchical Generalized Linear Models}
\description{
  \code{hglm2} is used to fit hierarchical generalized linear models. \code{hglm2} is used to fit hierarchical generalized linear models. 
  It extends the \code{hglm} function by allowing for several random effects, where the model is specified in \code{lme4} convension, 
  and also by implementing sparse matrix techniques using the \code{Matrix} library. 
}
\usage{
hglm2(meanmodel, data = NULL, family = gaussian(link = identity),
      rand.family = gaussian(link = identity), method = "EQL", 
      conv = 1e-4, maxit = 20, startval = NULL,
      X.disp = NULL, disp = NULL, link.disp = "log", 
      weights = NULL, fix.disp = NULL, offset = NULL, 
      sparse = TRUE, vcovmat = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{meanmodel}{\code{formula}. A two sided formula specifying the fixed and random terms in \code{lme4} convension, e.g. \code{y ~ x1 + (1|id)} indicates \code{y} as response, \code{x1} as the fixed effect and \code{(1|id)} represent a random intercept for each level of \code{id}.}
  \item{data}{\code{data.frame}. An optional data frame from where the variables in the \code{meanmodel} (and possibly \code{disp}) are to be obtained. It is expected that the data frame does not contain any missing value.}
  \item{family}{\code{family}. The description of the error distribution and link function to be used in the mean part of the model. (See \code{\link{family}} for details of family functions.)}
  \item{rand.family}{\code{family}. The description of the distribution and link function to be used for the random effect.}
  \item{method}{\code{character}. Estimation method where \code{EQL} is the method of interconnected GLMs presented in Lee et al (2006). Currently only \code{EQL} is available as the estimation method.}
  \item{conv}{\code{numeric}. The convergence criteria (change in linear predictor between iterations).}
  \item{maxit}{\code{numeric}. Maximum number of iterations in the \code{hglm} algorithm.}
  \item{startval}{\code{numeric}. A vector of starting values in the following order: fixed effects, random effect, variance of random effects, variance of residuals.}
  \item{X.disp}{\code{matrix}. The design matrix for the fixed effects in the dispersion part of the model.}
  \item{disp}{\code{formula}. A one-sided \code{\link{formula}} specifying the fixed effects in the dispersion part of the model.}
  \item{link.disp}{\code{character}. The link function for the dispersion part of the model.}
  \item{weights}{\code{numeric}. Prior weights to be specified in weighted regression.}
  \item{fix.disp}{\code{numeric}. A numeric value if the dispersion parameter of the mean model is known, e.g., 1 for binomial and Poisson model.}
  \item{offset}{An offset for the linear predictor of the mean model.}
  \item{sparse}{\code{logical}. If \code{TRUE}, the computation is to be carried out by using sparse matrix technique.}
  \item{vcovmat}{logical. If \code{TRUE}, the variance-covariance matrix is exported.}
  \item{\dots}{not used.}
}
\details{
  Models for \code{hglm} are either specified symbolically using \code{\link{formula}} 
  or by specifying the design matrices ( \code{X}, \code{Z} and \code{X.disp}). Currently, only the extended quasi likelihood (EQL)
  method is available for the estimation of the model parameters. Only for the Gaussian-Gaussina linear mixed models, it
  is REML. It should be noted that the EQL estimator can be biased and inconsistent in some special cases e.g. binary pair matched response. A higher order correction 
  might be useful to correct the bias of EQL (Lee et al. 2006). But, those currections are not implemented in the current version. By default, the dispersion
  parameter is always estimated via EQL. If the dispersion parameter of the mean model is to be held constant, for example if it is
  desired to be 1 for binomial and Poisson family, then \code{fix.disp}=value where, value=1 for the above example, should be used. 
}
\value{
It returns an object of class \code{hglm} consiting of the following values.
  \item{fixef}{fixed effect estimates.}
  \item{ranef}{random effect estimates.}
  \item{RandC}{integers (possibly a vector) specified the number of column of Z to be used for each of the random-effect terms.}
  \item{varFix}{dispersion parameter of the mean model (residual variance for LMM).}
  \item{varRanef}{dispersion parameter of the random effects (variance of random effects for GLMM).}
  \item{iter}{number of iterations used.}
  \item{Converge}{specifies if the algorithm converged.}
  \item{SeFe}{standard errors of fixed effects.}
  \item{SeRe}{standard errors of random effects.}
  \item{dfReFe}{deviance degrees of freedom for the mean part of the model.}
  \item{SummVC1}{estimates and standard errors of the linear predictor in the dispersion model.}
  \item{SummVC2}{estimates and standard errors of the linear predictor for the dispersion parameter of the random effects.}
  \item{dev}{individual deviances for the mean part of the model.}
  \item{hv}{hatvalues for the mean part of the model.}
  \item{resid}{studentized residuals for the mean part of the model.}
  \item{fv}{fitted values for the mean part of the model.}
  \item{disp.fv}{fitted values for the dispersion part of the model.}
  \item{disp.resid}{standardized deviance residuals for the dispersion part of the model.}
  \item{link.disp}{link function for the dispersion part of the model.}
  \item{vcov}{the variance-covariance matrix.}
}
\references{Lee, Y., Nelder J. A. and Pawitan Y. (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.}
Chapman and Hall/CRC.}
\author{Moudud Alam, Xia Shen, Lars Ronnegard}

\seealso{ \code{\link{hglm}}}
\examples{
# --------------------- #
# semiconductor example #
# --------------------- #

data(semiconductor)

h.gamma.normal <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
                       random = ~ 1|Device,
                       family = Gamma(link = log),
                       disp = ~ x2 + x3, data = semiconductor)
summary(h.gamma.normal)
plot(h.gamma.normal, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))

# ------------------- #
# redo it using hglm2 #
# ------------------- #

m1 <- hglm2(y ~ x1 + x3 + x5 + x6 + (1|Device),
            family = Gamma(link = log),
            disp = ~ x2 + x3, data = semiconductor)
summary(m1)
plot(m1, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))

# --------------------------------------------- #  
# simulated example with 2 random effects terms #
# --------------------------------------------- #  
\dontrun{
set.seed(911)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
z1 <- factor(rep(LETTERS[1:10], rep(10, 10)))
z2 <- factor(rep(letters[1:5], rep(20, 5)))
Z1 <- model.matrix(~ 0 + z1)
Z2 <- model.matrix(~ 0 + z2)
u1 <- rnorm(10, 0, sqrt(2))
u2 <- rnorm(5, 0, sqrt(3))
y <- 1 + 2*x1 + 3*x2 + Z1\%*\%u1 + Z2\%*\%u2 + rnorm(100, 0, sqrt(exp(x3)))
dd <- data.frame(x1 = x1, x2 = x2, x3 = x3, z1 = z1, z2 = z2, y = y)

(m2.1 <- hglm(X = cbind(rep(1, 100), x1, x2), y = y, Z = cbind(Z1, Z2), 
              RandC = c(10, 5)))
summary(m2.1)
plot(m2.1)

(m2.2 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), data = dd, vcovmat = TRUE))
image(m2.2$vcov)
summary(m2.2)
plot(m2.2)

m3 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), disp = ~ x3, data = dd)
print (m3)
summary(m3)
plot(m3)
}
}

