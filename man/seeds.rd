\name{seeds}
\docType{data}
\alias{seeds}
\title{Seeds genrmination data set from Crowder (1978)}
\description{
The data set was initially presented in Corder (1978) to demonstrate the problem of over dispersion with binomial response and its solution via beta-binomial ANOVA. Latter, the data set is used by may others including Breslow and Clayton (1993) and Lee and Nelder (1996) to demonstrate the usefulness of the Generalized Linear Mixed (and hierarchical) model. The seeds data set was originally obtained from a 2 by 2 factorial layout. The experiment was conducted on two types of seeds, \emph{O. aegyptiaca 75} and \emph{O. aegyptica 73}, and two root extracts, bean and cucumber with an equal dilution, 1/125. Experimental units (plates) were prepared with the specific roots extracts and a batch of certain seeds was brushed into the plates. The outcome is the count of germinated seed out of the total number of seeds applied in each plate. 
}
\format{ The seeds data set contans 5 columns and 21 rows. A short description of the data columns are given below. 
 \describe{
 \item{plate}{Plate number.}
 \item{seed}{Seed type; 2 levels: O75 (\emph{O. aegyptiaca 75}) and O73 (\emph{O. aegyptica 73}).}
 \item{extract}{Type of roots extract; 2 levels: Bean and Cucumber.}
 \item{r}{Response; number of seeds germinated in each plate.}
 \item{n}{Total number of seeds applied in each plate.}
}
}
\source{Crowder, M. J. 1978. Beta-binomial Anova for proportions, \emph{Journal of the Royal Statistical Society (C, Applied Statistics)} 27(1), 34--37. }
\references{Breslow, N. E. and Clayton, D. G.  1993. Approximate inference in generalized linear mixed models, \emph{Journal of the Amrecian Statistical Association} 88, 9--25. \cr
Lee, Y. and Nelder, J. A. 1996. Hierarchical generalized linear models, \emph{Journal of the Royal Statistical Association (B, Theory and Methods)} 58(4), 619--678.}
\keyword{datasets}