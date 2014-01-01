`likelihood` <- function(hglm.obj, y, X, Z, family = gaussian(link = identity), weights = NULL) {
	n <- length(y)
	if (is.null(weights)) weights <- rep(1,n)
	
	const <- switch(family$family[1],
			gaussian = length(y) *log(2 * pi),
			poisson = 2 * sum(y - y * ifelse(y > 0,log(y), 0) + lgamma(y + 1)),
			Gamma = 2 * sum(log(y)),
			inverse.gaussian = sum(log(2 * pi * y^3)),
			binomial = 0
	)
	
	#linear.pred <- family$linkfun(hglm.obj$fv)
	#linear.pred_disp <- hglm.obj$disp.fv
	#h.dglm <- dglm(y ~ 0 + offset(linear.pred), ~ 0 + offset(linear.pred_disp), family = family, dlink="identity")
	#cond.lik <- summary(h.dglm)$m2loglik

	glm.phi <- hglm.obj$disp.fv
	d <- hglm.obj$dev[1:n]
	cond.lik <- const + sum(d/glm.phi + log(glm.phi/weights))

	p <- hglm.obj$dfReFe
	cAIC <- cond.lik + 2*p
	v <- hglm.obj$ranef
	W1 <- diag(1/hglm.obj$disp.fv)
	W2 <- diag(1/hglm.obj$phi)
	H <- t(Z)%*%W1%*%Z+W2
	A <- rbind(cbind(t(X)%*%W1%*%X,t(X)%*%W1%*%Z),cbind(t(Z)%*%W1%*%X,H))
	hlik <- cond.lik -2*(-0.5*t(v)%*%W2%*%v - 0.5*nrow(W2)*log(2*pi) - 0.5*log(abs(det(solve(W2)))))
	pvh <- hlik  + log(abs(det(H/(2*pi))))
	pbvh <- hlik + log(abs(det(A/(2*pi))))
	list(hlik = as.numeric(hlik), pvh = as.numeric(pvh), pbvh = as.numeric(pbvh), cAIC = cAIC)
}