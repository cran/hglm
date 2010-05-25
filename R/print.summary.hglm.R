`print.summary.hglm` <-
function(x,digits=4,...){
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat("DISPERSION MODEL")
  cat("\n")
  if(x$Method=="REML"){
    cat(paste("-2*Log(Profile-Likelihood) =",round(-2*x$ProfLogLik,digits),"\n"))
  } else{
    cat("WARNING: h-likelihood estimates through EQL can be biased.")
    }
  if(!is.null(x$varFix)){
  if(is.null(x$SummVC1)){
   cat("\n") 
  cat(paste("Dispersion parameter held Constant at:", x$varFix))
  } else{
  cat("\n") 
  cat("Model estimates for the dispersion term:")
  print(x$varFix)
  }
  } 
  if(!is.null(x$SummVC1)){
  cat("\n")
  cat(paste("Model estimates for the dispersion term:","\n","Link =", 
  x$link.disp,"\n", "Effects:\n"))
  print(round(x$SummVC1,digits))
  cat("\n")
  cat("Dispersion = 1 is used in Gamma model on deviances to calculate the standard error(s).")
  }
  if(!is.null(x$varRanef)){
  cat("\n")
  cat("Dispersion parameter for the random effects\n")
  print(x$varRanef,digits=digits)
  cat("\n")
  cat(paste("Dispersion model for the random effects:","\n","Link = log","\n","Effects:\n"))
  print(round(x$SummVC2,digits))
  cat("\n")
  cat("Dispersion = 1 is used in Gamma model on deviances to calculate the standard error(s).")
  }
  cat("\n")
  cat("MEAN MODEL")
  cat("\n")
  cat("Summary of the fixed effects estimates \n")
  printCoefmat(x$FixCoefMat,digits=digits,P.value=TRUE,has.Pvalue=TRUE)
  cat(paste("Note: P-values are based on",x$devdf,"degrees of freedom"))
  if(!is.null(x$RandCoefMat)){
  cat("\n")
  cat("Summary of the random effects estimate \n")
  print(round(x$RandCoefMat,digits))
  }
  cat("\n")
  cat(paste(x$Method,"estimation",x$converge,"in",x$iter,"iterations. \n"))
}

