
pacs<-c("pracma","lme4","lmerTest","predictmeans","lubridate","data.table","roxygen2",
        "deSolve","dplyr","gdata","ggplot2","gimms","graphics","grid","gtools",
        "Kendall","KernSmooth","labeling","lattice",
        "lazyeval","Matrix","matrixStats","ncdf4","numDeriv",
        "parallel","pbkrtest","pkgconfig","plyr","raster","Rcpp","RcppEigen",
        "RCurl","reshape2","rgdal",
        "rgeos","rlang","rpart","RSQLite","rstudioapi","segmented","sp","spatial",
        "splines","stats","stats4","stringdist","stringi",
        "utils","viridisLite","ZeBook","zyp",
        "plyr","geosphere","stringdist","data.table","nlme","broom","forcats",
        "dismo","rgeos","RColorBrewer","reshape2","roxygen2","docstring","here")
for(i in 1:length(pacs)){
  pac<-pacs[i]
  if(length((find.package(pac,quiet=TRUE)))==0){
    install.packages(pac)
  }else{
    print(paste0("Package ",pac, " was already installed"))
  }
}
# # load pacs
# invisible(lapply(pacs, function(x)require(x,character.only = T,quietly = T)))



