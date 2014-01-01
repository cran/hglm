\name{CAR}
\alias{CAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conditional Auto Regressive Family}
\description{
  A function used in the \code{hglm} package which extends the usage of the CAR family.
}
\usage{
CAR(D, link = "identity", link.rand.disp = "inverse")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{the \bold{D} matrix of the Markov Random Field model.}
  \item{link}{the link function for the random effects.}
  \item{link.rand.disp}{the link function for the random effects dispersion parameter.}
}

\value{
  Output specific for \code{hglm} fit, including eigen values and vectors of \bold{D}.
}
\references{
Moudud Alam, Lars Ronnegard, Xia Shen (2014). \bold{Fitting spatial models in hglm}. \emph{Submitted}.
}
