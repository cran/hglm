\name{GLM.MME}
\alias{GLM.MME}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ An internal function to aid hglm estimation }
\description{
  An internal function to carry out IWLS estimation for hglm.
}
\usage{
GLM.MME(Augy, AugXZ, starting.delta, tau, phi, n.fixed, n.random, off, weights.sqrt, prior.weights, family, rand.family, maxit, conv, tol = 1e-07)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Augy}{Augmented response}
  \item{AugXZ}{Augmented design matrix}
  \item{starting.delta}{ Starting values }
  \item{tau}{Random effects dispersion parameter }
  \item{phi}{ Fixed effects dispersion parameter}
  \item{n.fixed}{ Number of fixed effects}
  \item{n.random}{Number of random effects}
  \item{off}{a numeric vector defining an offset}
  \item{weights.sqrt}{ IWLS weights }
  \item{prior.weights}{Prior weights }
  \item{family}{ Family of the response, possibly with a link }
  \item{rand.family}{ Family of the ranfom effects }
  \item{maxit}{ Maximu number of iteration }
  \item{conv}{Tolerence limit for convergence of estimates, default is 1e-4. }
  \item{tol}{ Tolerence limit used in the FORTRAN subroutin \code{dqrls} which performs the least square fitting.}
}
\details{
  A list of values returned by \code{dqrls} along with deviances and hat values are returned.
}
