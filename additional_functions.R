


## This function calculates matching metrics between different series 
## organized in a rwl object. The sup argument sets the minimum number of year of overlap 


calculate_matching <- function(s, sup = 20) {
  s <- as.data.frame(s)
  ts <- combn(s,2,
              function(couple) {
                one <- na.omit(couple)
                lng <- nrow(one)
                if (lng < sup) return(c("lng" = 0, "cor" = NA))               
                cors <- cor(one[,1],one[,2])
                out <- c(lng,cors)
                names(out) <- c("lng","cor")
                return(out)
              }, simplify = FALSE)
  ts <- as.data.frame(do.call(rbind,ts))
  
  ts$series <- combn(colnames(s),2, function(x) paste0(x[1],"__",x[2]))
  
  return(ts)
  
}


## averages a series of series present in one rwl object
averager <- function(rwl,             ## the rwl object
                     id = ""          ## the name of the averaged serie
                     ) {
  if(ncol(rwl) == 1) {
    colnames(rwl) <- id
    return(rwl)
  }
  one <- chron(rwl)
  one$samp.depth <- NULL
  one$std[is.nan(one$std)] <- NA
  colnames(one) <- id
  return(as.rwl(one))
  
}


## used to add a log grid to ggplots
log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}


