# zwei Punkte linear interpolieren
interpolate <- function(p1, p2, t1, t2, t){
  # linear die Koordinaten für zeitpunkt t interpolieren.
  lambda <- (t - t1)/(t2 - t1)
  stopifnot(lambda >=  0 & lambda <= 1)
    cbind(lambda * p2[1] + (1-lambda) * p1[1] ,
          lambda * p2[2] + (1-lambda) * p1[2]) # wenn z.B. t = t2, dann wird der Wert p2 angenommen, bei t = t1, wird p1 angemommen
}



# Testfall: Dreiecke interpolieren:
polys <- list(
  poly1 = list(timestamp = 0, points= list(c(0,0), c(1,1), c(1,-1))), # Dreieck mit timestamp
  poly2 = list(timestamp = 1, points= list(c(2,1), c(2,3), c(4,1))), # noch eins ...
  poly3 = list(timestamp = 2, points= list(c(4,6), c(5,4), c(5,3)))
)


interpolate_poly_list <- function(polys, t){
  timestamps <- sapply(polys, function(x)x$timestamp)
  if(min(t) < min(timestamps) | max(t) > max(timestamps)){stop("cannot interpolate outside of timestamp range")}
  if(!all(diff(timestamps) > 0)){
    stop("timestamps of polygons must be increasing")
  }
  out <- list()
  for(tt in t){
    # nächste timestamps finden
    idx_before_tt <- which.max((timestamps - tt)[timestamps - tt <= 0])
    if(idx_before_tt == length(polys)){ # if this is true, we have reached the last one
      next_poly <- polys[[idx_before_tt]]
    }else{
      poly_before <- polys[[idx_before_tt]]
      poly_after <- polys[[idx_before_tt + 1]]
      points_next_poly <- list()
      k <- length(poly_before$points)
      for(kk in 1:k){
        points_next_poly[[kk]] <- interpolate(poly_before$points[[kk]],
                                              poly_after$points[[kk]],
                                              poly_before$timestamp,
                                              poly_after$timestamp,
                                              tt)
      }
    next_poly <- list(timestamp = tt, 
                      points = points_next_poly)
    }
    out <- c(out,list(next_poly))
  }
  return(out)
}

interpolate_poly_list(polys, t =seq(0,2, length.out = 10))

# check output graphically
plot_pl <- function(polys){
  allxcoords <- sapply(polys,function(x)sapply(x$points,function(y)y[1]))
  allycoords <- sapply(polys,function(x)sapply(x$points,function(y)y[2]))
  plot(x= Inf, y = Inf, xlim = range(allxcoords), ylim = range(allycoords))
  k <- length(polys)
  for(kk in 1:k){
    polygon(sapply(polys[[kk]]$points,function(y)y[1]),sapply(polys[[kk]]$points,function(y)y[2]), col =kk)
  }
  return(invisible(NULL))
}
plot_pl(polys)

plot_pl(interpolate_poly_list(polys, t = seq(0,2,length.out = 10)))

