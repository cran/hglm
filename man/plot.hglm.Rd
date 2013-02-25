\name{plot.hglm}
\alias{plot.hglm}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Hierarchical Generalized Linear Model Objects}
\description{
 Plots residuals for the mean and dispersion models, individual deviances and hatvalues for \code{hglm} objects
}
\usage{
\method{plot}{hglm}(x, pch = "+", pcol = 4, lcol = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the \code{hglm} object to be plotted}
  \item{pch}{symbol used in the plots}
  \item{pcol}{color of points}
  \item{lcol}{color of lines}
  \item{\dots}{graphical parameters}
}
\details{
 A S3 generic plot method for \code{hglm} objects. It produces a set of diagnostic plots for a hierarchical model.
}
\author{Xia Shen}

\examples{
# --------------------- #
# semiconductor example #
# --------------------- #

data(semiconductor)

h.gamma.normal <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
                       random = ~ 1|Device,
                       family = Gamma(link = log),
                       disp = ~ x2 + x3, data = semiconductor)
summary(h.gamma.normal)
plot(h.gamma.normal, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))

# ------------------- #
# redo it using hglm2 #
# ------------------- #

m1 <- hglm2(y ~ x1 + x3 + x5 + x6 + (1|Device),
            family = Gamma(link = log),
            disp = ~ x2 + x3, data = semiconductor)
summary(m1)
plot(m1, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))

# --------------------------------------------- #  
# simulated example with 2 random effects terms #
# --------------------------------------------- #  
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

(m2.1 <- hglm(X = cbind(rep(1, 100), x1, x2), y = y, Z = cbind(Z1, Z2), 
              RandC = c(10, 5)))
summary(m2.1)
plot(m2.1)

(m2.2 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), data = dd, vcovmat = TRUE))
image(m2.2$vcov)
summary(m2.2)
plot(m2.2)

m3 <- hglm2(y ~ x1 + x2 + (1|z1) + (1|z2), disp = ~ x3, data = dd)
print (m3)
summary(m3)
plot(m3)
}
}