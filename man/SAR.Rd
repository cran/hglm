\name{SAR}
\alias{SAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simultaneous Autoregressive Family}
\description{
  A function used in the \code{hglm} package which extends the usage of the SAR family.
}
\usage{
SAR(D, link = "identity", link.rand.disp = "inverse.sqrt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{the \bold{D} matrix of the SAR model.}
  \item{link}{the link function for the random effects.}
  \item{link.rand.disp}{the link function for the random effects dispersion parameter.}
}

\value{
  Output specific for \code{hglm} fit, including eigen values and vectors of \bold{D}.
}
\references{
Moudud Alam, Lars Ronnegard, Xia Shen (2014). \bold{Fitting conditional and simultaneous autoregressive spatial models in hglm}. \emph{Submitted}.
}
