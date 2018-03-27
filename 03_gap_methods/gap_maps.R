#Julian Ramirez-Villegas
#March 2018

#make figure for historical (yield mean)
#tplot <- gap_class_plot(gap_class,cols=c('grey70', 'goldenrod3', 'red2'),new_ext=extent(-130,-25,-45,35))
#pdf("~/nfs/phaseolus_landrace_gaps.pdf", height=7,width=10)
#print(tplot)
#dev.off()

#######
#plotting function
gap_class_plot <- function(rsin,cols=c('grey70', 'goldenrod3', 'red2'),new_ext=NULL) {
  #produce gap maps
  library(raster); library(rasterVis); library(maptools)
  data(wrld_simpl)
  
  #prepare raster
  if (!is.null(new_ext)) {rsin <- crop(rsin, new_ext)}
  rsin <- ratify(rsin)
  rat <- levels(rsin)[[1]]
  rat$level <- c('Well conserved', 'Medium priority', 'High priority')
  levels(rsin) <- rat
  
  #figure details
  ht <- 12
  fct <- (rsin@extent@xmin-rsin@extent@xmax)/(rsin@extent@ymin-rsin@extent@ymax)
  wt <- ht*(fct+.1)
  grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=10), norths=seq(-90,90,by=15))
  
  #produce levelplot
  p <- rasterVis:::levelplot(rsin, att='level', margin=F, col.regions=cols,
                             maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  print(p)
  
  #return object
  return(p)
}

