`plot.hglm` <-
function(x, pch = "+", col.theme = "colorful", output = "screen",
filename = "HGLMplot", ...) {
	residuals <- x$resid
	fitted.values <- x$fv
	disp.residuals <- x$disp.resid
	disp.fitted.values <- x$disp.fv
	hatvalues <- x$hv
	deviances <- x$dev
#p <- x$p
#cook.dis <- deviances/(p*sum(deviances))*hatvalues/(1 - hatvalues)**2
	if (col.theme == "colorful") {
		pcol <- 4
		lcol <- 2
	}
	else {
		if (col.theme == "blackwhite") {
			pcol <- lcol <- 1
		}
		else {
			stop("Incorrect color theme is specified!")
		}
	}
	if (is.null(disp.fitted.values)) idx <- c(1,3) else idx <- 1:3
	for (i in idx) {
		if (output == "screen") {
			if (i > 1) {
				dev.new()
			}
		}
		else {
			if (output == "postscript") {
				postscript(paste(filename, i, ".ps", sep = ""))
			}
			else {
				if (output == "pdf") {
					pdf(paste(filename, i, ".pdf", sep = ""))
				}
				else {
					stop("Incorrect output format specified!")
				}
			}
		}
		if (i == 1) {
			par(mfrow = c(2,2), pty = "s", ...)
			loess.fit <- loess.smooth(fitted.values, residuals)
			plot(fitted.values, residuals, xlab = "Fitted Values", 
				 ylab = "Studentized Residuals", pch = pch, col = pcol, bty = "n", main = "Mean Model (a)")
			lines(loess.fit$x, loess.fit$y, col = lcol)
			loess.fit <- loess.smooth(fitted.values, abs(residuals))
			plot(fitted.values, abs(residuals), xlab = "Fitted Values", 
				 ylab = "|Studentized Residuals|", pch = pch, col = pcol, bty = "n", main = "Mean Model (b)")
			lines(loess.fit$x, loess.fit$y, col = lcol)
			qqnorm(residuals, col = pcol, pch = pch, bty = "n", 
				   xlab = "Normal Quantiles", ylab = "Residual Quantiles", main = "Mean Model (c)")
			qqline(residuals, col = lcol)
			hist(residuals, density = 15, xlab = "Studentized Residuals", main = "Mean Model (d)", col = pcol)
		}
		else {
			if (i == 2) {
				par(mfrow = c(2,2), pty = "s", ...)
				loess.fit <- loess.smooth(disp.fitted.values, disp.residuals)
				plot(disp.fitted.values, disp.residuals, xlab = "Fitted Values", 
					 ylab = "Standardized Deviance Residuals", pch = pch, col = pcol, bty = "n", main = "Dispersion Model (a)")
				lines(loess.fit$x, loess.fit$y, col = lcol)
				loess.fit <- loess.smooth(disp.fitted.values, abs(disp.residuals))
				plot(disp.fitted.values, abs(disp.residuals), xlab = "Fitted Values", 
					 ylab = "|Standardized Deviance Residuals|", pch = pch, col = pcol, bty = "n", main = "Dispersion Model (b)")
				lines(loess.fit$x, loess.fit$y, col = lcol)
				qqnorm(disp.residuals, col = pcol, pch = pch, bty = "n", 
					   xlab = "Normal Quantiles", ylab = "Residual Quantiles", main = "Dispersion Model (c)")
				qqline(disp.residuals, col = lcol)
				hist(disp.residuals, density = 15, xlab = "Standardized Deviance Residuals", main = "Dispersion Model (d)", col = pcol)
			}
			else {
				if (i == 3) {
					par(mfrow = c(2,2), pty = "s", ...)
					plot(hatvalues, ylab = "Hat-values", pch = pch, col = pcol, bty = "n")
					plot(deviances, ylab = "Deviances", pch = pch, col = pcol, bty = "n")
					beta <- var(deviances)/mean(deviances)
					alpha <- mean(deviances)/beta
					qqplot(rgamma(9999, alpha, 1/beta), deviances, col = pcol, 
						   pch = pch, xlab = "Gamma Quantiles", ylab = "Deviance Quantiles", 
						   bty = "n")
					abline(0, 1, col = lcol)
					hist(deviances, density = 15, xlab = "Deviances", main = "", col = pcol)
				}
			}
		}
		if (output != "screen") {
			dev.off()
		}
	}
}

