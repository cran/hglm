`lrt` <- function(hglm.obj1, hglm.obj2 = NULL) {
	if (is.null(summary(hglm.obj1)$likelihood) || is.null(summary(hglm.obj1)$likelihood)) 
		stop("Need to compute likelihood for LRT, for that, 
						switch on the calc.like argument when fitting your HGLM.")
	if (is.null(hglm.obj2)) {
		l0 <- as.numeric(logLik(hglm.obj1$null.model))
		DNAME <- deparse(substitute(hglm.obj1))
		test.stat <- 2*abs(summary(hglm.obj1)$likelihood$pbvh - l0)
		df <- length(hglm.obj1$varRanef)
	} else {
		DNAME <- paste(deparse(substitute(hglm.obj1)), 'v.s.', deparse(substitute(hglm.obj2)))
		test.stat <- 2*abs(summary(hglm.obj1)$likelihood$pbvh - summary(hglm.obj2)$likelihood$pbvh)
		df <- abs(length(hglm.obj1$varRanef) - length(hglm.obj2$varRanef))
	}
	STATISTIC <- test.stat
	names(STATISTIC) = 'LRT statistic' 
	p.value <- pchisq(test.stat, df, lower.tail = FALSE)/2
	structure(list(statistic = STATISTIC, p.value = p.value, 
				   method = "Likelihood-ratio test", data.name = DNAME), class = 'htest')
}


