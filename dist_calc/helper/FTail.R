FTail <-
function(U=NULL, df_n=100, df_d = 100, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
	if(U <= 5){xlim <- c(0,5)}
  if(U > 5){xlim <- c(0,U+0.01*U)}
	temp <- diff(range(xlim))
	x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
	y    <- df(x, df_n, df_d)
	ylim <- range(c(0,y))
	plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
	these <- (x >= U)
	X <- c(x[these][1], x[these], rev(x[these])[1])
	Y <- c(0, y[these], 0)
	polygon(X, Y, border=border, col=col)
	abline(h=0)
	axis(1, at = c(0,U), label = c(NA,round(U,4)))
}
