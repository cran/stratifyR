#' To organize and print the output to R console
#'
#' This function is called at the final stage after OSB and all other
#' associated calculations have been done. It organizes and prints all
#' the results and outputs to the R console as the final set of results
#'
#' @param data A vector: provided as an input to the function
#' @param my_env The environment my_env has all the solutions stored
#' in different variables returned by various functions
#'
#' @return \code{} returns the quantities such as OSB, Wh, Vh, WhSh,
#' nh, Nh and fh
#'
#' @author Karuna Reddy <reddy_k@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#'@examples
#' \dontrun{
#' data.res()
#' }
#'
data.res <- function(data, my_env)
  {
  h <- data.frame("Strata" = 1:my_env$h)

  #get outputs from strata.data(), rescale to original scale
  ObjFV <- my_env$ObjFV #this is for scaled data
  OSB <- round(((my_env$maxval)*(my_env$df)$x), digits=2) #convert from scaled osb to real osb

  distr <- as.character(my_env$obj["distr"])
  #if max==mode, triangle is actually rtriangle
  max <- my_env$obj[["params"]]["max"]
  mode <- my_env$obj[["params"]]["mode"]

  if(distr=="triangle"){
    if(round(max, digits=1) == round(mode, digits=1)){
      distr <- "rtriangle"
    }
    else{
      distr <- distr
    }
  }

  #some outputs from data.alloc()
  data.alloc(data, my_env)
  Output <- my_env$output  #is a df with Wh, Vh & WhSh
  out <- my_env$out #is a df with nh, Nh & sf
  deno <- my_env$deno #total of WhSh
  WhTot <- my_env$WhTot #tot of stratum weights
  NhTot <- my_env$NhTot #tot pop units in strata h
  nhTot <- my_env$nhTot #tot sample units in strata h
  fhTot <- my_env$fhTot #stratum sampling fraction
  VhTot <- my_env$VhTot #tot stratum variances

  #re-fit into real data
  if(distr == "pareto")
  {
     fit <- suppressWarnings(try(fitdist(data, distr="pareto",
               start = list(shape = 1, scale = 1), lower=c(0, 0)), silent=TRUE))
     }
  else if(distr == ("triangle"))
  {
     eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
     fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
              start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
  }
  else if(distr == ("rtriangle"))
  {
    eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
    fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
              start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
  }
  else #gamma onwards
  {
     fit <- suppressWarnings(try(fitdist(data, distr = distr, method="mle",
                  lower = c(0,0)), silent=TRUE)) #re-fit into real data
  }

  DF1 <- data.frame(h, OSB, Output, out)
  DF2 <- data.frame("Strata"="Total","OSB"="", "Wh"=WhTot,"Vh"=VhTot,"WhSh"=deno,
                    "nh"=nhTot,"Nh"=NhTot, "fh"=fhTot)
  DF <- rbind(DF1, DF2)

  #now writing the outputs
  cat("_____________________________________________\n")
  cat("Optimum Strata Boundaries for h =",my_env$h, "\n", sep = " ")
  cat("Data Range: [",(my_env$maxval)*my_env$initval,", ",
      (my_env$maxval)*my_env$finval,"]", " with d = ",
      (my_env$maxval)*my_env$dist, "\n", sep = "")
  cat("Best-fit Frequency Distribution: ", paste(distr),
      "\n", sep = " ")
  cat("Parameter estimate(s): ", "\n")
  print(fit$estimate) #params of original data
  cat("____________________________________________________\n")
  print(DF, row.names = FALSE)
  cat("____________________________________________________\n")
}
###################################################################
