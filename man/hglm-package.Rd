\name{hglm-package}
\alias{hglm-package}
\docType{package}
\title{
Hierarchical Generalized Linear Models
}
\description{
The \code{hglm} package is used to fit hierarchical generalized linear models. It can be used for linear mixed models and 
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
Version: \tab 1.2-2\cr
Date: \tab 2011-07-01\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
Depends: \tab Matrix, MASS\cr
}
This is version 1.2-2 of the hglm package. 
}
\author{
Moudud Alam, Xia Shen, Lars Ronnegard\cr
Maintainer: Lars Ronnegard <lrn@du.se>
}
\references{
Lee, Y., Nelder J. A. and Pawitan Y. (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.}
Chapman and Hall/CRC.
}

\keyword{package}
\seealso{
\code{\link{hglm}}, \code{\link{hglm2}}, \code{\link{plot.hglm}}
}
