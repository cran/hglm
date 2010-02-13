`hglm.default` <-
function(X=NULL,y=NULL,Z=NULL,family=gaussian(link=identity),
rand.family=gaussian(link=identity), method="HL",conv=1e-4,maxit=20,startval=NULL,
fixed=NULL,random=NULL,X.disp=NULL,disp=NULL,link.disp="log",data=NULL, weights=NULL,
fix.disp=NULL,offset=NULL,...){
  Call<-match.call()
  x<-as.matrix(X)
  y<-as.numeric(y)
  if(nrow(x)!=length(y)) {stop("Length of X and y differ.")} else{
    nobs<-nrow(x)
  }
  ### Check data consistency #######################################
  if(!is.null(Z)){
    z<-as.matrix(Z)
    if(nrow(x)!=nrow(z)) stop("Length of X and Z differ.")
    k<-ncol(z) ### gets number of clusters
  } else {
    stop("Random effects are missing with no default.")
  }
  if (!is.null(X.disp)) {
    x.disp<-as.matrix(X.disp)
   }  else {
    x.disp<-NULL
    }
  #### Check prior weights ########
  if(is.null(weights)){
    prior.weights<-rep(1,(nobs+k))
  } else {
  if(!is.numeric(weights) || any(weights<=0)) stop("Weights must be a numeric vector of positive values")
  if(length(weights)<nobs) stop("Length of the weights differ from the length of the data")
    prior.weights<-c(weights,rep(1,k))
  }
  ##### Check offset ########
  if(is.null(offset)){ off<-rep(0,nobs)
  } else{
  if(!is.numeric(offset)||length(offset)!=nobs) stop("Offset must be a numeric vector of the same length as the data")
  off<-as.numeric(offset)
  }
  ##### Data consistency checked ######################################

	GAMMA<-function (link = "log")
	{
		linktemp <- substitute(link)
		if (!is.character(linktemp)) {
			linktemp <- deparse(linktemp)
			if (linktemp == "link") {
				warning("use of GAMMA(link=link) is deprecated\n",
						domain = NA)
				linktemp <- eval(link)
				if (!is.character(linktemp) || length(linktemp) !=
					1)
				stop("'link' is invalid", domain = NA)
			}
		}
		okLinks <- c("inverse", "log", "identity")
		if (linktemp %in% okLinks)
		stats <- make.link(linktemp)
		else if (is.character(link))
		stats <- make.link(link)
		else {
			if (inherits(link, "link-glm")) {
				stats <- link
				if (!is.null(stats$name))
				linktemp <- stats$name
			}
			else {
				stop(gettextf("link \"%s\" not available for gamma family; available links are %s",
							  linktemp, paste(sQuote(okLinks), collapse = ", ")),
					 domain = NA)
			}
		}
#Note this variance function
		variance <- function(mu) mu
		validmu <- function(mu) all(mu > 0)
		dev.resids <- function(y, mu, wt) -2 * wt * (log(ifelse(y ==
																0, 1, y/mu)) - (y - mu)/mu)
		structure(list(family = "GAMMA", link = linktemp, linkfun = stats$linkfun,
					   linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, mu.eta = stats$mu.eta),
				  class = "family")
	}
	
	
  ##### Get GLM family and link #######################################
  
  if (is.character(family)) family <- get(family)
  if (is.function(family)) family <- eval(family)
     
  ###
  if (is.character(rand.family)) rand.family <- get(rand.family)
  if (is.function(rand.family)) rand.family <- eval(rand.family)
  #Note this defintion of random Gamma effects
  if (rand.family$family=="Gamma") rand.family<-eval(GAMMA()) 

  ##### GLM family and link are checked #################################
  ##### Only the GLM families (Lee et al. 2006) will pass this test ############
  #### Get augmented response, psi (Lee et al. (2006)) ###########
  if(rand.family$family=="gaussian"){
    psi<-rep(0,k)
  } else if(rand.family$family=="GAMMA"||rand.family$family=="inverse.gamma"){
    psi<-rep(1,k)
  } else if(rand.family$family=="Beta") {
    psi<-rep(1/2,k)
  } else {
    stop(paste("random.family=",rand.family$family," is not recognized as a member of the GLM family"))
  }
  if(!is.character(link.disp)) link.disp<-deparse(link.disp)
  if (link.disp=="log") {
    DispFamily<-Gamma(link="log")
  } 
  else if (link.disp=="identity") {
    DispFamily<-Gamma(link="identity")
  } 
  else if (link.disp=="inverse") {
    DispFamily<-Gamma(link="inverse")
  } 
  else {  
    stop("link.disp must be a valid link for the Gamma family GLM") 
  }
  #### Check starting values ################################################
  if(!is.null(startval)){
    if(!is.numeric(startval)) stop("Non-numeric starting value is not allowed")
    if(length(startval)<(ncol(x)+k)) stop("Too few starting values. See the documentation of hglm")
    if(length(startval)<(ncol(x)+k+1)) stop("Too few starting values")
    if(((family$family=="gaussian")||(family$family=="Gamma")) & 
      (length(startval)<(ncol(x)+k+2))) stop("Too few starting values. See the documentation of hglm")
    b.hat<-startval[1:ncol(x)]
    if(length(startval)>(ncol(x)+k+1)){
    init.sig.e<-as.numeric(startval[(ncol(x)+k+2)])
    } else{
    init.sig.e<-1
    }
      init.u<-startval[(ncol(x)+1):(ncol(x)+k)]
      init.sig.u<-as.numeric(startval[(ncol(x)+k+1)])
      if(min(init.sig.e,init.sig.u)<1e-4) stop("Unacceptable initial value is supplied for the variance parameter")
    
  } else{
  ### Generate default initial values of the fixed effects via a GLM ############
    g1<-glm(y~x-1,family=family,weights=weights,offset=off)
    b.hat<-as.numeric(coef(g1))
    init.sig.u<-(init.sig.e<-as.numeric(0.6*deviance(g1)/g1$df.residual))*.66
    if(!is.null(fix.disp)){
      if(!is.numeric(fix.disp) |fix.disp<=0 ) stop("\"fix.disp\" must be numeric and greater than 1e-4.")
       init.sig.e<-as.numeric(fix.disp)
    }
    rm(g1)
    if(init.sig.u<1e-4){
    init.sig.u<.1
    message("0.1 is chosen as the initial values for the dispersion parameter of the random effects.")
      }
      if(init.sig.e<1e-4){
    init.sig.u<-init.sig.e<-.1
    message("0.1 is chosen as the initial values for the dispersion parameter of the mean model.")
      }
    init.u<-rep(0,k)
    }

  #### Create Augmented data ##############################################
  if(!is.null(colnames(x))){
    x.names<-colnames(x)
    colnames(x)<-NULL
  } else {
    x.names<-paste("X.",1:ncol(x),sep="")
  }
  if (!is.null(z)) {
    if(!is.null(colnames(z))){
      z.names<-colnames(z)
      colnames(z)<-NULL
    } else {
      z.names<-paste("Z.",1:ncol(z),sep="")
    }
    Augy<-c(y,psi)
    AugXZ<-cbind(rbind(x,matrix(rep(0,ncol(x)*k),nrow=k,byrow=TRUE)),rbind(z,diag(k)))
    colnames(AugXZ)<-c(x.names,z.names)
  } else {
    Augy<-y
    AugXZ<-x
  }
  ##############################################################################
  ###### Begin Parameter estimation ############################################
  iter<-1
  if (!is.null(z)) phi<-rep(init.sig.u,k)  ## Random effects variance
    tau<-rep(init.sig.e,nobs)
    v.i<-rand.family$linkinv(init.u)
    eta.i<-as.numeric(x%*%b.hat)+off
    eta0<-eta.i
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    zi<-eta.i-off+(y-mu.i)/dmu_deta
    if (!is.null(z)) {
      zmi<-psi
      Augz<-c(zi,zmi)
      du_dv<-rand.family$mu.eta(psi)
      w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau),
      ((du_dv)^2/rand.family$variance(psi))*(1/phi)))*prior.weights)
    } else {
      w<-sqrt(as.numeric(((dmu_deta)^2/family$variance(mu.i))*(1/tau))*prior.weights)
    }
    n<-NROW(Augy)
    p<-NCOL(AugXZ)
    ny<-NCOL(Augy)
  while(iter<=maxit){
    g.mme<-GLM.MME(Augy, AugXZ, starting.delta=c(b.hat,v.i), tau, phi, 
      n.fixed=ncol(x), n.random=k, weights.sqrt=w, prior.weights, family, 
      rand.family, maxit, conv, off=off,tol=1e-7)
    b.hat<- g.mme$b.hat
    eta.i<- g.mme$eta.i
    v.i<- g.mme$v.i
    Augz<- g.mme$Augz
    dev<- g.mme$dev
    hv<- g.mme$hv
    ## 3 lines by Xia 2010-03-01 ##
	resid <- g.mme$resid
	fv <- g.mme$fv
    hv<- g.mme$hv
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    if (!is.null(z)) {
      ui<-rand.family$linkinv(v.i)
      du_dv<-rand.family$mu.eta(v.i)
    }
    if(is.null(fix.disp)){
    if (is.null(x.disp)) {
      g11<-glm((as.numeric(dev[1:nobs]/(1-hv[1:nobs])))~1,family=DispFamily,
        weights=as.numeric((1-hv[1:nobs])/2))
        if(length(g11$coef)==1){
      sigma2e <-DispFamily$linkinv(as.numeric(g11$coef[1]))
      } else{
      sigma2e<-NULL
      }
      tau<-as.numeric(g11$fitted) 
    } else {
      g11<-glm((as.numeric(dev[1:nobs]/(1-hv[1:nobs])))~x.disp-1,
        family=DispFamily,weights=as.numeric((1-hv[1:nobs])/2))
        if(length(g11$coef)==1){
      sigma2e <-DispFamily$linkinv(as.numeric(g11$coef[1]))
      }  else{
      sigma2e<-NULL
      }
      tau<-as.numeric(g11$fitted) #### Error variance updated
    }
   	disp.fv <- g11$fitted.values
	 disp.resid <- residuals(g11)/sqrt(1 - hatvalues(g11))
    } else{
    sigma2e<-as.numeric(fix.disp)
    disp.fv<-disp.resid<-NULL
    }
   
      g12<-glm((as.numeric(dev[(nobs+1):n]/(1-hv[(nobs+1):n])))~1,
        family=Gamma(link=log),weights=as.numeric((1-hv[(nobs+1):n])/2))
      sigma2u <-exp(as.numeric(g12$coef[1])) 
      phi<-rep(sigma2u,k)  ## Random effects variance updated
  
    if(sum((eta0-eta.i)^2)<conv*sum(eta.i^2) & iter>1) break
    eta0 <- eta.i
    if (!is.null(z)) w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau),
    ((du_dv)^2/rand.family$variance(ui))*(1/phi)))*prior.weights)
    if (is.null(z)) w<-sqrt(as.numeric(((dmu_deta)^2/family$variance(mu.i))*(1/tau))*prior.weights)
    iter<-iter+1
  }
  names(b.hat)=x.names
  if (!is.null(z)) names(ui)=z.names
  fixef<-b.hat                        
  if (!is.null(z)) {
    ranef<-ui
  } else {
    ranef<-NULL
    phi<-NULL
  }
  val<-list(call=Call, fixef=fixef, ranef=ranef, varFix=sigma2e, 
  varRanef=sigma2u, iter=iter, Converge="did not converge", SeFe=NULL, SeRe=NULL,
     dfReFe=NULL, SummVC1=NULL, SummVC2=NULL, method=method, dev=dev, hv=hv, 
	 resid = resid, fv = fv, disp.fv = disp.fv, disp.resid = disp.resid, link.disp=link.disp)
  if(iter<maxit){
   val$Converge<-"converged"
   ##### Calculate the standard errors of the fixed and random effects ########
   p1<-1:p
   QR<-g.mme$qr
   covmat<-chol2inv(QR$qr[p1,p1,drop=FALSE])
   SeFeRe<-sqrt(diag(covmat))
   val$SeFe<-SeFeRe[1:NCOL(x)]
   val$SeRe<-SeFeRe[(NCOL(x)+1):p]
   ### Calculate the deviance degrees of freedom (Lee et al. 2006, pp 193)#####
      Sigma0<-as.numeric(g.mme$wt)[1:nobs]
      Pd<-sum(diag(covmat%*%crossprod((AugXZ[1:nobs,]*Sigma0))))
      val$dfReFe<-round(nobs-Pd)
   if(is.null(fix.disp)){
   SummVC1<-summary(g11,dispersion=1)
   SummVC1<-SummVC1$coefficients[,1:2]
   if(!is.null(row.names(SummVC1))){
   dnames<-row.names(SummVC1)
   row.names(SummVC1)<-sub("x.disp",'',dnames)
   }
   val$SummVC1<-SummVC1
   } else{
   val$SummVC1<-fix.disp
   }
   if(!is.null(z)){
   SummVC2<-summary(g12,dispersion=1)
   val$SummVC2<-SummVC2$coefficients[,1:2]
   }
    ##### Calculate Profile Likelihood ##########
    Sigma <- diag(tau)
    if (!is.null(z)) {
      D <- diag(phi)
      V <- z%*%D%*%t(z)+Sigma
    } else {
      V <- Sigma
    }
    V.inv <- solve(V)
    temp <- y-x%*%fixef
    logdet.V <- sum(log(eigen(V,only.values=TRUE)$values))
    profile <- as.numeric(-nobs/2*log(2*pi)-1/2*logdet.V-1/2*t(temp)%*%V.inv%*%temp-1/2*log(det(t(x)%*%V.inv%*%x))+k/2*log(2*pi))
    #loglihood <- as.numeric(-nobs/2*log(2*pi)-1/2*logdet.V-1/2*t(temp)%*%V.inv%*%temp)
    if(method=="REML"){
      val$ProfLogLik<-profile
    }
 
  } else {
    val$iter<-iter-1
  }
  class(val)<-"hglm"
  return(val)
}