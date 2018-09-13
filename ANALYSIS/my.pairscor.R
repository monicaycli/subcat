my.pairscor <- function (data, classes = "", hist = TRUE, smooth = TRUE, cex.points = .5, col.hist = "darkgrey", col.points = "blue", rsize=1, psize=1) 
{
##	print(paste("lclass: ", lclass,"\n\n,",classes))
##	super.sym<-trellis.par.get("superpose.symbol")
#	super.sym<-c("black","red","blue","magenta","lightorange")
##	col.points <- super.sym$col[unique(unclass(classes))][unclass(classes)]
#	col.points <- super.sym[unique(unclass(classes))]
    panel.hist <- function(x, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, ...)
     }
    pairscor.lower <- function(x, y, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        m = cor.test(x, y)
        r = round(m$estimate, 2)
        rr = round((m$estimate^2), 2)
        p = round(m$p.value, 4)
        rtxt = paste("r", r, ",", rr)
        ptxt = paste("p=", p)
        options(warn = -1)
        m2 = cor.test(x, y, method = "spearman")
        r2 = round(m2$estimate, 2)
        rr2 = round((m2$estimate^2), 2)
        p2 = round(m2$p.value, 4)
        rtxt2 = paste("r=", r2,  ",", rr2)
        ptxt2 = paste("p =", p2)
        options(warn = 0)
        tcol="black"
        if(p<= .1) tcol = "green"
        if(p <= .05) tcol = "red"
        text(0.5, 0.8, rtxt, col = tcol, cex=rsize)
        text(0.5, 0.6, ptxt, col = tcol, cex=psize)
        lines(c(0.2, 0.8), c(0.5, 0.5))
        tcol="black"
        if(p2<= .1) tcol = "green"
        if(p2 < .05) tcol = "red"        
        text(0.5, 0.4, rtxt2, col = tcol, cex=rsize)
        text(0.5, 0.2, ptxt2, col = tcol, cex=psize)
    }
    panel.smooth2 = function(x, y, col = par("col"), bg = NA, 
        pch = par("pch"), cex = 1, span = 2/3, iter = 3, ...) {
        points(x, y, pch = pch, col = col, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                col = "black", ...)
    }
#    panel.smooth2 = function(x, y, col = par("col"), bg = NA, 
#        pch = par("pch"), cex = 1, span = 2/3, iter = 3, ...) {
#        points(x, y, pch = pch, col = col, bg = bgs, cex = cex)
#        ok <- is.finite(x) & is.finite(y)
#        if (any(ok)) 
#            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
#                col = "black", ...)
#    }
#    panel.smooth2 = function(x, y, col = par("col"), bg = NA, 
#        pch = par("pch"), cex = 1, span = 2/3, iter = 3, ...) {
#        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
#        ok <- is.finite(x) & is.finite(y)
#        if (any(ok)) 
#            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
#                col = "black", ...)
#    }
    if (hist == TRUE) {
        if (smooth == TRUE) {
#        	if(lclass > 0){
            	pairs(data, diag.panel = panel.hist, lower.panel = pairscor.lower, 
                	upper.panel = panel.smooth2, col =col.points, 
                	cex = cex.points)
              	}
 #              	else{
#                		pairs(data, diag.panel = panel.hist, lower.panel = pairscor.lower, 
#                	upper.panel = panel.smooth2, col = col.points, 
#                	cex = cex.points)
#                }
#      }
#        else {
#            pairs(data, diag.panel = panel.hist, lower.panel = pairscor.lower)
#        }
    }
    else {
        if (smooth == TRUE) {
            pairs(data, lower.panel = pairscor.lower, upper.panel = panel.smooth2, 
                col = 		, cex = cex.points)
        }
        else {
            pairs(data, lower.panel = pairscor.lower)
        }
    }
}