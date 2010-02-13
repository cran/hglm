\name{hglm.default}
\alias{hglm.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fitting Hierarchical Generalized Linear Models}
\description{
  \code{hglm} is used to fit hierarchical generalized linear models. It can be used for linear mixed models and 
  generalized linear models with random effects for a variety of links and a variety of distributions for both
   the outcomes and the random effects. Fixed effects can also be fitted in the dispersion part of the model.
    The function can be called either by specifying the design matrices or as a \code{\link{formula}}.
}
\usage{
\method{hglm}{default}(X=NULL,y=NULL,Z=NULL,family=gaussian(link=identity),
rand.family=gaussian(link=identity), method="HL",
conv=1e-4,maxit=20,startval=NULL,fixed=NULL,random=NULL,
X.disp=NULL, disp=NULL, link.disp="log",data=NULL,
weights=NULL,fix.disp=NULL, offset=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{a design matrix for the fixed effects}
  \item{y}{the dependent variable}
  \item{Z}{an incidence matrix for the random effects}
  \item{family}{a description of the error distribution and link function to be used in the mean part of the model. (See \code{\link{family}} for details of family functions.)}
  \item{rand.family}{a description of the error distribution and link function to be used in the variance part of the model.}
  \item{method}{estimation method where \code{HL} is the HL(0,0) method of Lee et al (2006)}
  \item{conv}{a convergence criteria (change in linear predictor between iterations)}
  \item{maxit}{maximum number of iterations in the hglm algorithm}
  \item{startval}{a vector of starting values in the following order: fixed effects, random effect, variance of random effects, variance of residuals}
  \item{fixed}{a \code{\link{formula}} specifying the fixed effects part of the model }
  \item{random}{a one-sided \code{\link{formula}} specifying the random effects part of the model }
  \item{X.disp}{a design matrix for the fixed effects in the dispersion part of the model}
  \item{disp}{a one-sided \code{\link{formula}} specifying the fixed effects in the dispersion part of the model }
  \item{link.disp}{the link function for the dispersion part of the model}
  \item{data}{the data frame to be used together with \code{fixed} and \code{random}}
  \item{weights}{prior weights to be specified in weighted regression }
  \item{fix.disp}{a numeric value if the dispersion parameter of the mean model is known for example 1 for binomial and Poisson model.}
  \item{offset}{an offset term for the mean model}
  \item{\dots}{ not used }
}
\details{
  Models for \code{hglm} are either specified symbolically using \code{\link{formula}} 
  or by specifying the design matrices ( \code{X}, \code{Z} and \code{X.disp}).Currently, only the extended quasi likelihood (EQL)
  method is available for the estimation of the model parameters. Only for the Gaussian-Gaussina linear mixed models, it
  is REML. It should be noted that the EQL estimator can be biased and inconsistent in some special cases e.g. binary pair matched response. A higher order correction 
  might be useful to correct the bias of EQL (Lee et al. 2006). But, those currections are not implemented in the current version. By default, the dispersion
  parameter is always estimated via EQL. If the dispersion parameter of the mean model is to be held constant, for example if it is
  desired to be 1 for binomial and Poisson family, then \code{fix.disp}=value where, value=1 for the above example, should be used. 
}
\value{
  \item{fixef}{fixed effect estimates}
  \item{ranef }{random effect estimates}
  \item{varFix}{dispersion parameter for the mean model (residual variance for LMM)}
  \item{varRanef}{dispersion parameter of the random effects (variance of random effects for GLMM)}
  \item{iter}{number of iterations used}
  \item{Converge}{specifies if the algorithm converged}
  \item{SeFe}{standard errors of fixed effects}
  \item{SeRe}{standard errors of random effects}
  \item{dfReFe}{deviance degrees of freedom for the mean part of the model}
  \item{SummVC1}{estimates and standard errors of the linear predictor in the dispersion model}
  \item{SummVC2}{estimates and standard errors of the linear predictor for the variance of the random effects}
  \item{dev}{individual deviances for the mean part of the model}
  \item{hv}{hatvalues for the mean part of the model}
  \item{resid}{studentized residuals for the mean part of the model}
  \item{fv}{fitted values for the mean part of the model}
  \item{disp.fv}{fitted values for the dispersion part of the model}
  \item{disp.resid}{standardized deviance residuals for the dispersion part of the model}
  \item{link.disp}{link function for the dispersion part of the model}
}
\references{Lee, Y., Nelder J. A. and Pawitan Y. (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.}
Chapman and Hall/CRC.}
\author{Moudud Alam, Lars Ronnegard, Xia Shen}

\seealso{ \code{\link{summary.hglm}}}
\examples{
data(semiconductor)
h.gamma.normal <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
                       random = ~ 1|Device,
                       family = Gamma(link = log),
                       disp = ~ x2 + x3, data = semiconductor )
summary(h.gamma.normal)
plot(h.gamma.normal, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))
}





