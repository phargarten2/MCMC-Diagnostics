#' Extends `traceplot()` function in **coda** package by plotting traceplots on a panel and saving them as a tiff file. A traceplot "Displays a plot of iterations vs. sampled values for each variable in the chain, with a separate plot per variable."

#' @import coda

#' @inheritParams traceplot
x
An mcmc or mcmc.list object
smooth
draw smooth line through trace plot
col
graphical parameter (see par)
type
graphical parameter (see plot)
xlab
graphical parameter (see plot)
ylab
graphical parameter (see plot)
...
further graphical parameters
 #'@param filename: the name of the file to store the plots in tiff format.  

 
many.traceplots <- function(x, ..., filename ="TracePlot"){
 #number of parameters
   p <- nvar(x)

  #Depending on number of variables...plot 
    if(1<=p && p<=6) {
      tiff( filename =paste0( getwd(), "/", filename, "1.tiff") )
      #Sets up the graphical display.
      if (p==2)         {  par(mfrow=c(2,1)) }
      if (p==3 || p==4) {  par(mfrow=c(2,2)) }
      if (p==5 || p==6) {  par(mfrow=c(3,2)) }

      traceplot(x, ...)
      #print( "Gelman Plots" ,line=line)
      dev.off()

      } else { #if(6<p){
      times<-ceiling(p/9);  #Calculates the multiple of 9
      #For each multiple of 9 creates a new graph;
      for(j in 1:times){
        start<- (9*(j-1)+1) #Start at 1,10,1 ...
        end<- min( 9*j , p) #End at multiple of 9 (or p if not exact multiple)
        tiff( filename =paste0( getwd(), "/", filename, j, ".tiff") )
        par(mfrow=c(3,3))
        traceplot(x[,start:end],  ...)
        dev.off()
        #print(paste("Gelman Plots(",j,")",sep=""),line=line)
      }
      }
    
    cat("#> Plots saved in", getwd() )
 
 return(invisible())
 }
 
