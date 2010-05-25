`print.hglm` <-
function(x,...){
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat("Fixed effects:\n")
  print(x$fixef)
  cat("Random effects: \n")
  print(x$ranef)
  if(!is.null(x$varFix)){
  cat("\n Dispersion parameter for the mean model:")
  print(x$varFix)
  } else{
  cat(paste("\n", "Estimates of the dispersion model:","\n", "Link=", 
  x$link.disp,"\n", "Effects:\n"))
  print(x$SummVC1[,1])
  }
  cat("Dispersion parameter for the random effects:")
  print(x$varRanef)
  cat(paste("\n","Estimation",x$Converge,"in",x$iter,"iterations \n"))
}

