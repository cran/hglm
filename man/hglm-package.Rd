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
The function can be called either by specifying the design matrices or as a \code{\link{formula}}. The default estimation method is
extended quasi likelihood (EQL; Lee et al., 2006) but from version 2.0 the HL11 correction has been implemented as well.   
}
\details{
\tabular{ll}{
Package: \tab hglm\cr
Type: \tab Package\cr
Version: \tab 2.0-9\cr
Date: \tab 2014-09-26\cr
Discussion: \tab https://r-forge.r-project.org/forum/?group_id=558\cr
BugReports: \tab https://r-forge.r-project.org/tracker/?group_id=558\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
Depends: \tab R (>= 2.10), utils, Matrix, MASS, hglm.data\cr
}
}
\author{
Xia Shen, Moudud Alam, Lars Ronnegard\cr\cr
Maintainer: Xia Shen <xia.shen@ki.se>
}
\references{
Lars Ronnegard, Xia Shen and Moudud Alam (2010). \bold{hglm: A Package for Fitting Hierarchical Generalized Linear Models}. \emph{The R Journal}, \bold{2}(2), 20-28.\cr\cr

Youngjo Lee, John A Nelder and Yudi Pawitan (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.} Chapman and Hall/CRC.\cr\cr

Xia Shen, Moudud Alam, Freddy Fikse and Lars Ronnegard (2013). \bold{A novel generalized ridge regression method for quantitative genetics}. \emph{Genetics}.\cr\cr

Moudud Alam, Lars Ronnegard, Xia Shen (2014). \bold{Spatial modeling in hglm}. \emph{Submitted}.
}

\keyword{package}
\seealso{
\code{\link{hglm}}, \code{\link{hglm2}}, \code{\link{plot.hglm}}
}
