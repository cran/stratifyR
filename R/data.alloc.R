#' Allocate data
#' To calculate the stratum sample sizes (nh) for a fixed sample size (n) 
#' directly based on the data
#'
#' @param data Input dataset.
#' @param my_env Environment object.
#' @return ...
#'
#' Uses OSB to compute stratum weights (Wh), sample variances (Vh from data),
#' and Neyman allocations (nh). Builds the summary table once at the end.
data.alloc <- function(data, my_env)
{
  # -------- unpack inputs / setup --------
  h       <- my_env$h
  initval <- my_env$initval * my_env$maxval          # back to real (unscaled) units
  n       <- my_env$n
  x       <- c(initval, my_env$df$x * my_env$maxval) # OSB (including left edge) on real scale
  ch      <- my_env$ch                               # per–stratum unit costs
  
  # preallocate result vectors (faster than growing them)
  var  <- numeric(h)  # stratum sample variances from raw data
  Wh   <- numeric(h)  # stratum weights from raw data
  nume <- numeric(h)  # numerator term in Neyman allocation: Wh * Sh * sqrt(cost)
  nh   <- numeric(h)  # sample sizes per stratum
  Nh   <- numeric(h)  # population counts per stratum
  fh   <- numeric(h)  # sampling fractions per stratum
  deno <- 0           # sum of 'nume' across strata
  
  # -------- compute Wh, Vh, Nh from data --------
  data <- sort(data)
  Ntot <- length(data)
  
  for (i in 1:(length(x) - 1L)) {
    # interval follows your original convention: [x[i], x[i+1]] inclusive
    slice   <- data[data >= x[i] & data <= x[i + 1L]]
    Nh[i]   <- length(slice)
    Wh[i]   <- Nh[i] / Ntot
    var[i]  <- try(stats::var(slice), silent = TRUE)
    nume[i] <- Wh[i] * sqrt(var[i]) * sqrt(ch[i])
    deno    <- deno + nume[i]
  }
  
  # -------- build summary table ONCE (moved out of the loop) --------
  my_env$output <- data.frame(
    Wh   = round(Wh,  2),
    Vh   = round(var, 2),
    WhSh = round(nume, 3)
  )
  my_env$Nh <- Nh
  
  # -------- initial Neyman allocation (sum to n) --------
  for (i in 1:(length(x) - 1L)) {
    nh[i] <- n * nume[i] / deno
  }
  
  # -------- fix oversampling if any nh > Nh (your existing logic) --------
  realloc(h, x, nh, Nh, nume, my_env)
  nh <- my_env$nh
  
  for (i in 1:(length(x) - 1L)) {
    if (nh[i] > Nh[i]) {
      realloc(h, x, nh, Nh, nume, my_env)
      nh <- my_env$nh
    } else {
      nh <- my_env$nh
    }
  }
  
  # -------- assemble user-facing outputs and totals --------
  fh <- round(nh / Nh, 2)
  my_env$out <- data.frame(nh = round(nh), Nh = Nh, fh = fh)
  
  my_env$deno  <- round(deno, 3)
  my_env$WhTot <- round(sum(Wh), 0)
  my_env$NhTot <- sum(Nh)
  my_env$nhTot <- round(sum(nh))
  my_env$VhTot <- round(sum(var), 2)
  my_env$fhTot <- round(my_env$nhTot / my_env$NhTot, 2)
}