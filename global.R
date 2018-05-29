library(quantmod)
library(shiny)

# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol,src="yahoo",
                                  
                                  auto.assign = FALSE)
  }
  
  envir[[symbol]]
}


t_radio <- function(price,t.margin=0.02,time=9){
  v <- apply(HLC(price), 1, mean)
  r <- matrix(NA,ncol=time,nrow=NROW(price))
  for (x in 1:time) r[,x] <- Next(Delt(v,k=x),x)
  x <- apply(r,1,function(x) sum(x[x>t.margin|x< -t.margin]))
  if (is.xts(price))
    xts(x,time(price)) else x
}

