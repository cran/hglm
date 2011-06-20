`print.hglm` <-
	function(x, ...) {

x$nRand <- cumsum(x$RandC)
cat("Call: \n")
print(x$call)
cat("\n---------------------------")
cat("\nEstimates of the mean model\n")
cat("---------------------------\n")
cat("\n")
cat("Fixed effects:\n")
print(x$fixef)
cat('\n')
if (length(x$RandC) == 1) {
	cat("Random effects:\n")
	print(x$ranef)
	cat('\n')
} else {
	cat("Random effects:\n")
	print(x$ranef[1:x$nRand[1]])
	cat('\n')
	for (J in 2:length(x$RandC)) {
		cat("Random effects:\n")
		print(x$ranef[(x$nRand[J - 1] + 1):x$nRand[J]])
		cat('\n')
	}
}
if (!is.null(x$varFix)) {
	cat("Dispersion parameter for the mean model:", x$varFix, '\n')
} else {
	cat("---------------------------------")
	cat("\nEstimates of the dispersion model\n")
	cat("---------------------------------\n")
	cat("\nLink =", x$link.disp, "\n")
	cat("\nEffects:\n")
	print(x$SummVC1[,1])
}
cat("\nDispersion parameter for the random effects:", x$varRanef, '\n')
cat(paste("\nEstimation", x$Converge, "in", x$iter, "iterations\n"))

}

