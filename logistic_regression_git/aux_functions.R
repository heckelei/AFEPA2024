
plot_prior_dens <- function(x, y=NULL, perc=.99, ...) {
  
  range_dens <- apply(x, 1, function(x) range(density(x)$y))[2,]
  sel_obs <- range_dens<quantile(range_dens, perc)
  x <- x[sel_obs,]
  
  if(is.null(y)) {
    
    par(pty="s")
    plot(density(x[1,]), lwd=2, col=rgb(0,0,0,.1), xlab="y", 
         ylim=c(0, max(range_dens[sel_obs])),
         xlim=range(x), ...)
    for (s in 2:ncol(x)) {
      lines(density(x[s,]), col=rgb(0,0,0,.1))
    }
    par(pty="m")
    
  } else {
    
    par(pty="s")
    plot(density(x[1,]), lwd=2, col=rgb(0,0,0,.1), xlab="y", 
         ylim=c(0, max(range_dens[sel_obs])),
         xlim=range(x), ...)
    for (s in 2:ncol(x)) {
      lines(density(x[s,]), col=rgb(0,0,0,.1))
    }
    lines(density(y), col = "red", lwd = 2)
    par(pty="m")
    
  }
  
}

plot_prior_point <- function(x,subs=F,...) {

  if (subs==T) {
    
    S <- sample(1:nrow(x), round(nrow(x)/2))
    
    par(pty="s")
    plot(x[1,], pch=16, cex=.3, col=rgb(0,0,0,.01), ylim=c(0,1), ...)
    for(s in S) {
      points(x[s,], pch=16, cex=.3, col=rgb(0,0,0,.01))
    }
    par(pty="m")
    
  } else {
    
    par(pty="s")
    plot(x[1,], pch=16, cex=.3, col=rgb(0,0,0,.01), ylim=c(0,1), ...)
    for(s in 2:nrow(x)) {
      points(x[s,], pch=16, cex=.3, col=rgb(0,0,0,.01))
    }
    par(pty="m")
    
  }
  
}

plot_prior_lines <- function(x, a, b, y, ...) {
  
  par(pty="s")
  plot(x, colMeans(y), col="white", ylab="y", ...)
  abline(a = a[1], b = b[1], col=rgb(0,0,0,.1))
  for (s in 2:length(a)) {
    abline(a = a[s], b = b[s], col=rgb(0,0,0,.1))
  }
  abline(h = mean(y), col = "red")
  par(pty="s")
  
}
