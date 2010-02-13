`hglm` <-
function(X=NULL,y=NULL,Z=NULL,family=gaussian(link=identity),
rand.family=gaussian(link=identity), method="HL",conv=1e-4,maxit=20,startval=NULL,
fixed=NULL,random=NULL,X.disp=NULL, disp=NULL, 
link.disp="log",data=NULL,weights=NULL, fix.disp=NULL, offset=NULL,...) UseMethod("hglm")

