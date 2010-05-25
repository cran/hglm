`GLM.MME` <-
function(Augy, AugXZ, starting.delta, tau, phi, n.fixed, n.random, off,
      weights.sqrt, prior.weights, family, rand.family, maxit, conv, tol=1e-7) {
  #Set constants and working variables
  n <- length(Augy)
  ny<-1
  p <- NCOL(AugXZ)
  k <- n.random
  nk <- n-k
  if ((n.random+n.fixed)!=(ncol(AugXZ))) stop("Input error in GLM.MME")
  w <- weights.sqrt
  x <- as.matrix(AugXZ[1:(n-k),(1:(p-k))])
  if (n.random!=0) { 
    z <- AugXZ[1:(n-k),(p-k+1):p]
  } else {
    z <- NULL
    v.i <- NULL
  }
  y <- Augy[1:(n-k)]
  if (!is.null(z)) psi <- Augy[(n-k+1):n]
  b.hat<-as.numeric(starting.delta[1:(p-k)])
  if (!is.null(z)) v.hat<-starting.delta[(p-k+1):p]
  #Calculate working variable z
  if (!is.null(z)) {
    ui<-rand.family$linkinv(v.hat)
    eta.i<-as.numeric(x%*%b.hat+z%*%ui)+off
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    zi<-(eta.i-off)+(y-mu.i)/dmu_deta
    du_dv<-rand.family$mu.eta(ui)
    zmi<-v.hat+(psi-ui)/du_dv
    Augz<-c(zi,zmi)
  } else {
    eta.i<-as.numeric(x%*%b.hat)+off
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    zi<-(eta.i-off)+(y-mu.i)/dmu_deta
    Augz<-zi
  }
  eta0<-eta.i
  storage.mode(AugXZ) <- "double"
  #Iterations starts here
  maxmuit<-1
  while(maxmuit<=maxit){
    storage.mode(Augz) <- "double"
    storage.mode(w) <- "double"
    fit<-.Fortran("dqrls", qr = AugXZ*w, n = as.integer(n), p = as.integer(p), y = Augz*w, ny = as.integer(ny),
     tol = as.double(tol), coefficients = mat.or.vec(p, ny),
      residuals = Augz, effects = Augz, rank = integer(1), pivot = 1:p,
      qraux = double(p), work = double(2 * p), PACKAGE = "base")
    est<-fit$coefficients 
    if (!is.null(z)) { 
      eta.i<-as.numeric(x%*%est[1:(p-k)]+z%*%est[(p-k+1):p])+off
      mu.i<-family$linkinv(eta.i)
      dmu_deta<-family$mu.eta(eta.i)
      zi<-(eta.i-off)+(y-mu.i)/dmu_deta
      v.i<-est[(p-k+1):p]
      ui<-rand.family$linkinv(v.i)
      du_dv<-rand.family$mu.eta(v.i)
      zmi<-v.i+(psi-ui)/du_dv
      Augz<-c(zi,zmi)
      w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau),
      ((du_dv)^2/rand.family$variance(ui))*(1/phi)))*prior.weights)
    } else {
      eta.i<-as.numeric(x%*%est[1:(p-k)])+off
      mu.i<-family$linkinv(eta.i)
      dmu_deta<-family$mu.eta(eta.i)
      zi<-(eta.i-off)+(y-mu.i)/dmu_deta
      Augz<-zi
      w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau))*prior.weights))
    }
    if(sum((eta0-eta.i)^2)<conv*sum(eta.i^2)) break
    eta0<-eta.i                                                           
    b.hat<-est[1:(p-k)]
    if (!is.null(z)) v.hat<-v.i
    maxmuit<-maxmuit+1
  }
  if(maxmuit>=maxit) message(paste("GLM.MME did not converge in", maxit,":th iteration"))
  qrs <- structure(fit[c("qr", "qraux", "pivot", "tol", "rank")],class="qr")
  hv<-hat(qrs,intercept=FALSE)
  #Calculate deviances
  if (!is.null(z)) { 
    dev<-c(as.numeric(family$dev.resids(y[1:nk],mu.i,prior.weights[1:nk])), as.numeric(rand.family$dev.resids(psi,ui,rep(1,k))))
  } else {
    dev<-c(as.numeric(family$dev.resids(y[1:nk],mu.i,prior.weights[1:nk])))
  }
  ## updated by Xia 2010-03-01 ##
  resid <- (y - mu.i)/sqrt(sum(dev)/(n - p))/sqrt(1 - hv[1:nk])
  GLM.out<-list(Augz=Augz, eta.i=eta.i, v.i=v.i, b.hat=b.hat, dev=dev, hv=hv, resid=resid, fv=mu.i, qr=qrs,wt=w )
  return(GLM.out)
}

