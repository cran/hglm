`summary.hglm` <-
function(object,...){
  Call<-object$call
  Method<-object$method
  if(object$Converge=="did not converge") stop("There is no valid estimate to produce summary statistics")
  if(is.null(object$fixef)) stop("There in no valid estimate to produce summary statistics")
  if(is.null(object$SeFe)) stop("There in no valid standard error estimate to produce summary statistics")
  Nfix<-length(object$fixef)
  FixCoefMat<-matrix(numeric(4*Nfix),nrow=Nfix,byrow=TRUE)
  dimnames(FixCoefMat)<-list(names(object$fixef),c("Estimate", "Std. Error",
   "t value", "Pr(>|t|)"))
    FixCoefMat[,1]<-as.numeric(object$fixef)
    FixCoefMat[,2]<-as.numeric(object$SeFe)
    FixCoefMat[,3]<-as.numeric(FixCoefMat[,1]/FixCoefMat[,2])
    FixCoefMat[,4]<-2 * pt(abs(as.numeric(FixCoefMat[,3])),object$dfReFe, lower.tail = FALSE)
  if(!is.null(object$ranef)){
  Nran<-length(object$ranef)
  RandCoefMat<-matrix(numeric(2*Nran),nrow=Nran,byrow=TRUE)
  dimnames(RandCoefMat)<-list(names(object$ranef),c("Estimate", "Std. Error"))
  RandCoefMat[,1]<-as.numeric(object$ranef)
  RandCoefMat[,2]<-as.numeric(object$SeRe)
  } else{
  RandCoefMat<-NULL
  }
  smst<-list(Method=Method,FixCoefMat=FixCoefMat,RandCoefMat=RandCoefMat,
  SummVC1=object$SummVC1, SummVC2=object$SummVC2, iter=object$iter,
  converge=object$Converge,call=Call,ProfLogLik=object$ProfLogLik, devdf=object$dfReFe,
  LogLik=object$LogLik, varFix=object$varFix,varRanef=object$varRanef,link.disp=object$link.disp)
  class(smst)<-"summary.hglm"
  return(smst)
}

