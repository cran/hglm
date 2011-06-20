`hglm2` <-
	function(meanmodel = NULL, data = NULL, family = gaussian(link = identity),
             rand.family = gaussian(link = identity), method = "EQL", 
             conv = 1e-4, maxit = 20, startval = NULL,
             X.disp = NULL, disp = NULL, link.disp = "log", 
             weights = NULL, fix.disp = NULL, offset = NULL, sparse = TRUE, vcovmat = FALSE, ...) UseMethod("hglm2")

