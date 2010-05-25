\name{hglm-package}
\alias{hglm-package}
\docType{package}
\title{
Hierarchical Generalized Linear Models
}
\description{
 \code{hglm} is used to fit hierarchical generalized linear models. It can be used for linear mixed models and 
  generalized linear models with random effects for a variety of links and a variety of distributions for both
   the outcomes and the random effects. Fixed effects can also be fitted in the dispersion part of the model.
    The function can be called either by specifying the design matrices or as a \code{\link{formula}}. Currently, 
    only the extended quasi likelihood (EQL; Lee et al., 2006) estimation method is available which is also the 
    REML for linear mixed models.   
}
\details{
\tabular{ll}{
Package: \tab hglm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-10-08\cr
License: \tab Unlimited\cr
LazyLoad: \tab yes\cr
}
This is version 1.0 of the hglm package. 
}
\author{
M. Alam, L. Ronnegard, X. Shen
Maintainer: Lars Ronnegard <lrn@du.se>
}
\references{
Lee, Y., Nelder J. A. and Pawitan Y. (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.}
Chapman and Hall/CRC.
}

\keyword{ package }
\seealso{
\code{\link{summary.hglm}}
}
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


