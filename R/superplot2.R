superplot2 <-
function(x,y) {
  xhist <- hist(x, plot=FALSE)
  yhist <- hist(y, plot=FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  xrange <- range(x)
  yrange <- range(y)
  nf <- layout(matrix(c(2,1,0,3),2,2,byrow=TRUE), c(1,3), c(3,1), TRUE)
  layout.show(nf)
  par(mar=c(1,1,1,1))
  plot(x, y, xlim=xrange, ylim=yrange, xlab="", ylab="")
  par(mar=c(1,1,1,1))
  barplot(yhist$counts, xlim=c(top, 0), space=0, horiz=TRUE) # !
  par(mar=c(1,1,1,1))
  barplot(xhist$counts, ylim=c(top,0), space=0) #!ylim!
  invisible()
}
