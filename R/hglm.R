`hglm` <-
	function(X = NULL, y = NULL, Z = NULL, family = gaussian(link=identity),
	         rand.family = gaussian(link = identity), method = "EQL", 
	         conv = 1e-4, maxit = 20, startval = NULL,
	         fixed = NULL, random = NULL, X.disp = NULL, disp = NULL, 
	         link.disp = "log", data = NULL, weights = NULL, fix.disp = NULL, 
	         offset = NULL, RandC = NULL, sparse = TRUE, vcovmat = FALSE, ...) UseMethod("hglm")

.onAttach <- 
function(...)
{
 cat("hglm: Hierarchical Generalized Linear Models\n")
 cat('Version 1.2-2 installed\n')
 cat('\n')
 cat('Authors:    Moudud Alam - maa@du.se\n')
 cat('            Xia Shen - xia.shen@icm.uu.se\n')
 cat('            Lars Ronnegard - lrn@du.se\n')
 cat('\n')
 cat('Maintainer: Lars Ronnegard - lrn@du.se\n')
}