superplot <-
function(x,y) {
  par(mar=c(1.1,1.1,1,1))
  nf <- layout(array(c(2,4,1,3), dim=c(2,2)), c(1,3), c(3, 1), TRUE)
  plot(x,y)
  hist.inv(y,2)
  hist.inv(x) ; frame()
}
