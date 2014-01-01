\name{lrt}
\alias{lrt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Likelihood-ratio test for variance components in \code{hglm}}
\description{
  Likelihood-ratio test for the estimated variance components (or other dipersion parameters) in \code{hglm}.
}
\usage{
lrt(hglm.obj1, hglm.obj2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hglm.obj1}{a fitted \code{hglm} object.}
  \item{hglm.obj2}{optional, another fitted \code{hglm} object to be tested against \code{hglm.obj1}.}
}
\details{
  When \code{hglm.obj2 = NULL}, all the random effects variance components in \code{hglm.obj1} are tested against
  the null model with only fixed effects. The degree of freedom is determined by comparing the number of random
  effects terms in \code{hglm.obj1} and \code{hglm.obj2} or the null fixed-effects-only model. Note that the likelihood-
  ratio test statistic for variance estimates, which are bounded above zero, follows a 50:50 mixture distribution
  of chi-square with 0 and 1 degree of freedom (Self and Liang 1987 JASA).
}
\value{
  Printout summary of the likelihood-ratio test results. Test statistic, p-value, etc. are returned.
}
\references{
Self, S. G., & Liang, K.-Y. (1987). Asymptotic Properties of Maximum Likelihood Estimators and Likelihood Ratio Tests Under Nonstandard Conditions. \emph{Journal of the American Statistical Association}, \bold{82}(398), 605-610.
}
\examples{
require(hglm)

\dontrun{
set.seed(911)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
z1 <- factor(rep(LETTERS[1:10], rep(10, 10)))
z2 <- factor(rep(letters[1:5], rep(20, 5)))
Z1 <- model.matrix(~ 0 + z1)
Z2 <- model.matrix(~ 0 + z2)
u1 <- rnorm(10, 0, sqrt(2))
u2 <- rnorm(5, 0, sqrt(3))
y <- 1 + 2*x1 + 3*x2 + Z1\%*\%u1 + Z2\%*\%u2 + rnorm(100, 0, sqrt(exp(x3)))
dd <- data.frame(x1 = x1, x2 = x2, x3 = x3, z1 = z1, z2 = z2, y = y)

m20 <- hglm(X = cbind(rep(1, 100), x1, x2), y = y, Z = Z1, 
            calc.like = TRUE)

lrt(m20)
         
m21 <- hglm(X = cbind(rep(1, 100), x1, x2), y = y, Z = cbind(Z1, Z2), 
             RandC = c(10, 5), calc.like = TRUE)

lrt(m20, m21)

}
}

