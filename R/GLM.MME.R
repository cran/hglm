`GLM.MME` <-
	function(Augy, AugXZ, starting.delta, tau, phi, n.fixed, n.random, off,
             weights.sqrt, prior.weights, family, rand.family, maxit, sparse = TRUE, tol = 1e-7) {

### Set constants and working variables
n <- length(Augy)
p <- ncol(AugXZ)
k <- n.random
nk <- n - k
if (n.random + n.fixed != ncol(AugXZ)) stop("Input error in GLM.MME: n.random + n.fixed is not equal to ncol(X) + ncol(Z)")
w <- weights.sqrt
if (!sparse) AugXZ <- as.matrix(AugXZ)
x <- as.matrix(AugXZ[1:nk,1:(p - k)])
if (n.random != 0) z <- AugXZ[1:nk,(p - k + 1):p] else z <- v.i <- NULL
y <- Augy[1:nk]
if (!is.null(z)) psi <- Augy[(nk + 1):n]
b.hat <- as.numeric(starting.delta[1:(p - k)])
if (!is.null(z)) v.hat <- starting.delta[(p - k + 1):p]

### Calculate working variable z
if (!is.null(z)) {
	ui <- rand.family$linkinv(v.hat)
    eta.i <- as.numeric(x%*%b.hat + z%*%ui) + off
    mu.i <- family$linkinv(eta.i)
    dmu_deta <- family$mu.eta(eta.i)
    zi <- as.numeric(eta.i - off + (y - mu.i)/dmu_deta)
    du_dv <- rand.family$mu.eta(ui)
    zmi <- as.numeric(v.hat + (psi - ui)/du_dv)
    Augz <- c(zi, zmi)
} else {
    eta.i <- as.numeric(x%*%b.hat) + off
    mu.i <- family$linkinv(eta.i)
    dmu_deta <- family$mu.eta(eta.i)
    zi <- eta.i - off + (y - mu.i)/dmu_deta
    Augz <- zi
}
eta0 <- eta.i

### Iterations starts here
maxmuit <- 1
while (maxmuit <= maxit){
	SQR <- qr(AugXZ*w)
    est <- as.numeric(qr.coef(SQR, y = Augz*w)) 
    if (!is.null(z)) { 
    	v.i <- est[(p - k + 1):p]
		b.hat <- est[1:(p - k)]
		eta.i <- as.numeric(x%*%b.hat + z%*%v.i + off)
    	mu.i <- family$linkinv(eta.i)
    	dmu_deta <- family$mu.eta(eta.i)
    	zi <- eta.i - off + (y - mu.i)/dmu_deta
    	ui <- rand.family$linkinv(v.i)
    	du_dv <- rand.family$mu.eta(v.i)
    	zmi <- as.numeric(v.i + (psi - ui)/du_dv)
    	Augz <- c(zi, zmi)
    	w <- sqrt(as.numeric(c((dmu_deta^2/family$variance(mu.i))*(1/tau), (du_dv^2/rand.family$variance(ui))*(1/phi)))*prior.weights)
    } else {
    	eta.i <- as.numeric(x%*%est[1:(p - k)]) + off
    	mu.i <- family$linkinv(eta.i)
    	dmu_deta <- family$mu.eta(eta.i)
    	zi <- eta.i - off + (y - mu.i)/dmu_deta
    	Augz <- zi
    	w <- sqrt(as.numeric(c((dmu_deta^2/family$variance(mu.i))*(1/tau))*prior.weights))
    }
    if (sum((eta0 - eta.i)^2) < tol*sum(eta.i^2)) break
    eta0 <- eta.i                                                           
    maxmuit <- maxmuit + 1
}
if (maxmuit > maxit) message(paste("GLM.MME did not converge in", maxit, ":th iteration"))
#qrs <- structure(fit[c("qr", "qraux", "pivot", "tol", "rank")],class="qr")
hv <- rowSums(qr.qy(SQR, diag(1, nrow = n, ncol = p))^2)

### Calculate deviances
if (!is.null(z)) { 
	dev <- c(as.numeric(family$dev.resids(y[1:nk], mu.i, prior.weights[1:nk])), as.numeric(rand.family$dev.resids(psi, ui, rep(1, k))))
} else {
    dev <- c(as.numeric(family$dev.resids(y[1:nk], mu.i, prior.weights[1:nk])))
}
resid <- (y - mu.i)/sqrt(sum(dev)/(n - p))/sqrt(1 - hv[1:nk])

GLM.out <- list(Augz = Augz, eta.i = eta.i, v.i = v.i, b.hat = b.hat, dev = dev, hv = hv, resid = resid, fv = mu.i, qr = SQR, wt = w)

return(GLM.out)

}

