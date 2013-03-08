\name{hglm}
\alias{hglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fitting Hierarchical Generalized Linear Models}
\description{
  \code{hglm} is used to fit hierarchical generalized linear models. It can be used for linear mixed models and 
  generalized linear models with random effects for a variety of links and a variety of distributions for both
   the outcomes and the random effects. Fixed effects can also be fitted in the dispersion part of the model.
    The function can be called either by specifying the design matrices or as a \code{\link{formula}}.
}
\usage{
hglm(X = NULL, y = NULL, Z = NULL, family = gaussian(link = identity),
     rand.family = gaussian(link = identity), method = "EQL",
     conv = 1e-6, maxit = 20, startval = NULL, fixed = NULL, random = NULL,
     X.disp = NULL, disp = NULL, link.disp = "log", data = NULL, 
     weights = NULL, fix.disp=NULL, offset = NULL, 
     RandC = NULL, sparse = TRUE, vcovmat = FALSE, calc.like = FALSE, 
     bigRR = FALSE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{\code{matrix}. The design matrix for the fixed effects.}
  \item{y}{\code{numeric}. The dependent variable.}
  \item{Z}{\code{matrix}. The design matrix for the random effects.}
  \item{family}{\code{family}. The description of the error distribution and link function to be used in the mean part of the model. (See \code{\link{family}} for details of family functions.)}
  \item{rand.family}{\code{family}. The description of the distribution and link function to be used for the random effect.}
  \item{method}{\code{character}. Estimation method where \code{EQL} is the method of interconnected GLMs presented in Lee et al (2006). Currently only \code{EQL} is available as the estimation method.}
  \item{conv}{\code{numeric}. The convergence criteria (change in linear predictor between iterations).}
  \item{maxit}{\code{numeric}. Maximum number of iterations in the \code{hglm} algorithm.}
  \item{startval}{\code{numeric}. A vector of starting values in the following order: fixed effects, random effect, variance of random effects, variance of residuals.}
  \item{fixed}{\code{formula}. A \code{\link{formula}} specifying the fixed effects part of the model.}
  \item{random}{\code{formula}. A one-sided \code{\link{formula}} specifying the random effects part of the model.}
  \item{X.disp}{\code{matrix}. The design matrix for the fixed effects in the dispersion part of the model.}
  \item{disp}{\code{formula}. A one-sided \code{\link{formula}} specifying the fixed effects in the dispersion part of the model.}
  \item{link.disp}{\code{character}. The link function for the dispersion part of the model.}
  \item{data}{\code{data.frame}. The data frame to be used together with \code{fixed} and \code{random}.}
  \item{weights}{\code{numeric}. Prior weights to be specified in weighted regression.}
  \item{fix.disp}{\code{numeric}. A numeric value if the dispersion parameter of the mean model is known, e.g., 1 for binomial and Poisson model.}
  \item{offset}{An offset for the linear predictor of the mean model.}
  \item{RandC}{\code{numeric}. Integers (possibly a vector) specifying the number of column of Z to be used for each of the random-effect terms.}
  \item{sparse}{\code{logical}. If \code{TRUE}, the computation is to be carried out by using sparse matrix technique.}
  \item{vcovmat}{\code{logical}. If \code{TRUE}, the variance-covariance matrix is returned.}
  \item{calc.like}{logical. If \code{TRUE}, likelihoods will be computed at convergence and will be shown via the print or summary methods on the output object.}
  \item{bigRR}{logical. If \code{TRUE}, and only for the Gaussian model with one random effect term, a specific algorithm will be used for fast fitting high-dimensional (p >> n) problems. See Shen et al. (2013) for more details of the method.}
  \item{verbose}{logical. If \code{TRUE}, more information is printed during model fitting process.}
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
  \item{likelihood}{a list of likelihood values for model selection purposes, where \code{$hlik} is the h-likelihood, \code{$pvh} the adjusted profile likelihood profiled over random effects, \code{$pbvh} the adjusted profile likelihood profiled over fixed and random effects, \code{$cAIC} the conditional AIC.}
}
\references{
Ronnegard, Lars, Xia Shen and Moudud Alam (2010). \bold{hglm: A Package for Fitting Hierarchical Generalized Linear Models}. \emph{The R Journal}, \bold{2}(2), 20-28.\cr\cr

Lee, Youngjo, John A Nelder and Yudi Pawitan (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.} Chapman and Hall/CRC.\cr\cr

Shen, Xia, Moudud Alam, Freddy Fikse and Lars Ronnegard (2013). \bold{A novel generalized ridge regression method for quantitative genetics}. \emph{Genetics}.
}
\author{Moudud Alam, Lars Ronnegard, Xia Shen}

\seealso{\code{\link{hglm2}}}
\examples{
require(hglm)

# --------------------- #
# semiconductor example #
# --------------------- #

data(semiconductor)

m11 <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
            random = ~ 1|Device,
            family = Gamma(link = log),
            disp = ~ x2 + x3, data = semiconductor)
summary(m11)
plot(m11, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))

# ------------------- #
# redo it using hglm2 #
# ------------------- #

m12 <- hglm2(y ~ x1 + x3 + x5 + x6 + (1|Device),
             family = Gamma(link = log),
             disp = ~ x2 + x3, data = semiconductor)
summary(m12)
     
# -------------------------- #
# redo it using matrix input #
# -------------------------- #

attach(semiconductor)
m13 <- hglm(y = y, X = model.matrix(~ x1 + x3 + x5 + x6),
            Z = kronecker(diag(16), rep(1, 4)),
            X.disp = model.matrix(~ x2 + x3), 
            family = Gamma(link = log))
summary(m13)
     
# --------------------- #
# verbose & likelihoods #
# --------------------- #
     
m14 <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
            random = ~ 1|Device,
            family = Gamma(link = log),
            disp = ~ x2 + x3, data = semiconductor,
            verbose = TRUE, calc.like = TRUE)
summary(m14)

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

(m21 <- hglm(X = cbind(rep(1, 100), x1, x2), y = y, Z = cbind(Z1, Z2), 
              RandC = c(10, 5)))
summary(m21)
plot(m21)

(m22 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), data = dd, vcovmat = TRUE))
image(m22$vcov, main = 'Variance-covariance Matrix')
summary(m22)
plot(m22)

m31 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), disp = ~ x3, data = dd)
print (m31)
summary(m31)
plot(m31)
}
}

