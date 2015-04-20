\name{logLik.hglm}
\alias{logLik.hglm}
\alias{logLik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracts log-likelihood values}
\description{
  Extracts log-likelihood values from an existing hglm object \code{hglm.obj}.
}
\usage{
\method{logLik}{hglm}(object, REML=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A fitted \code{hglm} object.}
  \item{REML}{The default NULL returns all computed log-likelihoods. The option \code{REML=TRUE} returns only the adjusted profile log-likelihood profiled over fixed and random effects.}
  \item{...}{This argument is not used.}
}
\details{
  The use of log-likelihoods and cAIC is described in Lee, Nelder and Pawitan (2006).
}
\value{
  A list of log-likelihood values for model selection purposes, where $hlik is the log-h-likelihood, $pvh the adjusted profile log-likelihood profiled over random effects, $pbvh the adjusted profile log-likelihood profiled over fixed and random effects, and $cAIC the conditional AIC.
}
\references{
Youngjo Lee, John A Nelder and Yudi Pawitan (2006) \emph{Generalized Linear Models with Random Effect: a unified analysis via h-likelihood.} Chapman and Hall/CRC.
}

