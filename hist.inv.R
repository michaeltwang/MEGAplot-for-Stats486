hist.inv <-
function(x,side=1) {
  gg <- hist(x,plot=F)
  con <- max(gg$counts)*1.05
  if( side==1) {
    plot(range(gg$breaks),c(0,con),type="n", axes=F,xlab="",ylab="")
    axis(2,con -( i <- pretty(gg$counts)),i)
    for(i in 1:length(gg$counts)) rect(gg$breaks[i],con-gg$counts[i],gg$breaks[i+1],con)
  }
  if(side==2) {
    plot(c(0,con),range(gg$breaks),type="n", axes=F,xlab="",ylab="")
    axis(1,con -( i <- pretty(gg$counts)),i)
    for(i in 1:length(gg$counts)) rect(con-gg$counts[i],gg$breaks[i],con,gg$breaks[i+1])
  }
}
