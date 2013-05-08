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
Version: \tab 1.2-8\cr
Date: \tab 2013-05-08\cr
Discussion: \tab https://r-forge.r-project.org/forum/?group_id=558\cr
BugReports: \tab https://r-forge.r-project.org/tracker/?group_id=558\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
Depends: \tab R (>= 2.10), utils, Matrix, MASS, dglm\cr
}
This is version 1.2-7 of the hglm package. 
}
\author{
Moudud Alam, Xia Shen, Lars Ronnegard\cr\cr
Maintainer: Lars Ronnegard <lrn@du.se>
}
\references{
Ronnegard, Lars, Xia Shen and Moudud Alam (2010). \bold{hglm: A Package for Fitting Hierarchical Generalized Linear Models}. \emph{The R Journal}, \bold{2}(2), 20-28.\cr\cr

Lee, Youngjo, John A Nelder and Yudi Pawitan (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.} Chapman and Hall/CRC.\cr\cr

Shen, Xia, Moudud Alam, Freddy Fikse and Lars Ronnegard (2013). \bold{A novel generalized ridge regression method for quantitative genetics}. \emph{Genetics}.
}

\keyword{package}
\seealso{
\code{\link{hglm}}, \code{\link{hglm2}}, \code{\link{plot.hglm}}
}
