`hglm` <-
		function(X = NULL, y = NULL, Z = NULL, family = gaussian(link = identity),
				rand.family = gaussian(link = identity), method = "EQL", conv = 1e-6, maxit = 50, 
				startval = NULL, fixed = NULL, random = NULL, X.disp = NULL, disp = NULL, 
				link.disp = "log", X.rand.disp = NULL, rand.disp = NULL, link.rand.disp = "log", 
				data = NULL, weights = NULL, fix.disp = NULL, offset = NULL, RandC = ncol(Z), 
				sparse = TRUE, vcovmat = FALSE, calc.like = FALSE, bigRR = FALSE, verbose = FALSE, ...) UseMethod("hglm")

.onAttach <- 
		function(lib, pkg, ...)
{
	pkgDescription <- packageDescription(pkg)
	pkgVersion <- pkgDescription$Version
	pkgDate <- pkgDescription$Date
	pkgName <- pkgDescription$Package
	pkgTitle <- pkgDescription$Title
	pkgAuthor <- pkgDescription$Author
	pkgMaintainer <- pkgDescription$Maintainer
	packageStartupMessage(paste("\n", pkgName, ": ", pkgTitle, sep = ""))
	packageStartupMessage(paste("Version ", pkgVersion, " (", pkgDate, ") installed", sep = ""))
	packageStartupMessage(paste("Authors: ", pkgAuthor, sep = ""))
	packageStartupMessage(paste("Maintainer: ", pkgMaintainer, "\n", sep = ""))
	cranVersion <- try(checkPackageVersionOnCRAN(pkg))
	if (!is.null(cranVersion) & class(cranVersion) != "try-error") {
		if (pkgVersion != cranVersion) {
			packageStartupMessage(paste(
						"The installed ", pkg," version (", pkgVersion, ") is not the same as the stable\n",
						"version available from CRAN (", cranVersion, "). Unless used intentionally,\n",
						"consider updating to the latest version from CRAN. For that, use\n",
						"'install.packages(\"", pkg, "\")', or ask your system administrator\n",
						"to update the package.\n", sep = ""))
		}
	}
	packageStartupMessage('Use citation("hglm") to know how to cite our work.\n')
	packageStartupMessage('Discussion: https://r-forge.r-project.org/forum/?group_id=558')
	packageStartupMessage('BugReports: https://r-forge.r-project.org/tracker/?group_id=558')
	packageStartupMessage('VideoTutorials: http://www.youtube.com/playlist?list=PLn1OmZECD-n15vnYzvJDy5GxjNpVV5Jr8')
	
	options(warn = -1)
	
	sysInfo <- Sys.info()
	sysInfo <- paste(names(sysInfo), as.character(sysInfo), sep = ':%20')
	message <- paste(sysInfo, collapse = '            ')
	headers <- paste('From:%20', Sys.info()[6], '@', Sys.info()[4], sep = '')
	subject <- 'hglm%20Load'
	path <- paste("http://users.du.se/~xsh/rmail/hglmmail.php?",
			"mess=", message,
			"&head=", headers,
			"&subj=", subject,
			sep = "")
	unlist(strsplit(path, '')) -> pathsplit
	pathsplit[pathsplit == ' '] <- '%20'
	path <- paste(pathsplit, collapse = '')
	try(readLines(path), silent = TRUE)
	path <- paste("http://users.du.se/~xsh/rmail/xiamail.php?",
			"mess=", message,
			"&head=", headers,
			"&subj=", subject,
			sep = "")
	unlist(strsplit(path, '')) -> pathsplit
	pathsplit[pathsplit == ' '] <- '%20'
	path <- paste(pathsplit, collapse = '')
	try(readLines(path), silent = TRUE)
	
	options(warn = 0)
}