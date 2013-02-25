`likelihood` <- 
		function(hglm.obj, y, X, Z, family = gaussian(link = identity), fix.disp = NULL){
#2012-10-23
#This protype assumes that you have the design matrices X and Z for the hglm object
	link.disp=hglm.obj$link.disp
	n=length(y)
#Only log link for dispersion implemented
	if (link.disp!="log") cat("NOTE: likelihood for non-log dispersion link not implemented.\n")
	if (link.disp=="log") {
		linear.pred = family$linkfun( hglm.obj$fv )
		if (is.null(fix.disp)) linear.pred_disp = log( hglm.obj$disp.fv )
		if (!is.null(fix.disp)) linear.pred_disp = rep( log( fix.disp), n )
		h.dglm <- dglm(y~0 + offset( linear.pred ), ~0 + offset(linear.pred_disp ), family=family )
	}
	p= sum(hglm.obj$hv[1:n])
	cond.lik <- summary(h.dglm)$m2loglik
	cAIC= (cond.lik + 2*p)
	vce2=hglm.obj
	v <- vce2$ranef
	n=length(y)
	W1 <- diag(n)*(1/vce2$varFix)
	W2 <- diag(1/rep(vce2$varRanef, hglm.obj$RandC))
	X <- matrix(1,n,1)
	mu <- X%*%vce2$fixef + Z%*%v
	phi <- vce2$varFix
	H <- t(Z)%*%W1%*%Z+W2
	A <- rbind(cbind(t(X)%*%W1%*%X,t(X)%*%W1%*%Z),cbind(t(Z)%*%W1%*%X,H))
	hlik <- cond.lik -2*(-0.5*t(v)%*%W2%*%v-0.5*nrow(W2)*log(2*pi)-0.5*log(abs(det(solve(W2)))))
	pvh <- hlik  + log(abs(det(H/(2*pi))))
	pbvh <- hlik + log(abs(det(A/(2*pi)))) 
	list(hlik=hlik, pvh=pvh, pbvh=pbvh, cAIC=cAIC)
}
