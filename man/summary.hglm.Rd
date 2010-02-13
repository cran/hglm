\name{summary.hglm}
\alias{summary.hglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summary method for hglm object}
\description{
 It provides standard summary statistics for the fitted hglm objects.
}
\usage{
\method{summary}{hglm}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A hglm object. }
  \item{\dots}{ Unused urguments }
}
\details{
  When applied to a hglm object summary statistics are produced if the underlying model estimation converged. Otherwise, produces an error message.
}
\value{
  It returns a list of \code{summary.hglm} object containing the following statistics and tables
  \item{Method }{The estimation method used in the undelying \code{hglm} call.}
  \item{FixCoefMat }{A two-dimensional matrix of fixed effects parameter estimates, respective standard errors, t-values and p-palues. }
   \item{RandCoefMat }{A two-dimensional matrix of random effects estimates and respective standard errors.}
   \item{SummVC1}{Estimate and standard errors for fixed effects\' (common error term) dispersion model calculated in the linear predictor's scale.}
   \item{SummVC2}{Estimate and standard errors for random effects\' dispersion model calculated in the linear predictor's scale.}
   \item{iter}{Number of iteration used in \code{hglm} estimation.}
   \item{converge}{A text, \"converged\" if the hglm estimation converged and \"did not converge\" otherwise.}
   \item{ProfLogLik}{Profile Log-likelihood if the estimation method if \"REML\" for Gaussian \code{hglm}}
   \item{LogLik}{Currently not available}
   \item{varFix}{Dispersion parameter of the fixed effects (common errors for LMM) in true scale when it is not modelled or modelled with an intercept only }
   \item{varRanef}{Dispersion parameter of the random effects in true scale when it is not modelled or modelled with an intercept only }
   \item{disp.link}{Link function for the fixed effects in the (common error) dispersion parameter.}
}

