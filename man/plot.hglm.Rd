\name{plot.hglm}
\alias{plot.hglm}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Hierarchical Generalized Linear Model Objects}
\description{
 Plots residuals for the mean and dispersion models, individual deviances and hatvalues for \code{hglm} objects
}
\usage{
\method{plot}{hglm}(x, pch = "+", col.theme = "colorful", output = "screen", filename = "HGLMplot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the \code{hglm} object to be plotted}
  \item{pch}{symbol used in the plots}
  \item{col.theme}{either \code{colorful} or \code{blackwhite}}
  \item{output}{specifies whether the plots should be printed to \code{screen}, \code{pdf} or \code{postscript} files}
  \item{filename}{name of output file}
  \item{\dots}{graphical parameters}
}
\details{
 A basic plot method for \code{hglm} objects. It produces a set of diagnostic plots for a hierarchical mdel. Plots can be viewed on current scrin or may be saved in current working directore by specifying sutable file name in the \code{filename} argument. 
}

\examples{
data(semiconductor)
h.gamma.normal <- hglm(fixed = y ~ x1 + x3 + x5 + x6,
                       random = ~ 1|Device,
                       family = Gamma(link = log),
                       disp = ~ x2 + x3, data = semiconductor )
summary(h.gamma.normal)
plot(h.gamma.normal, cex = .6, pch = 1,
     cex.axis = 1/.6, cex.lab = 1/.6,
     cex.main = 1/.6, mar = c(3, 4.5, 0, 1.5))
}