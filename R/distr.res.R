#' To organize and print the output to R console
#'
#' This function is called at the final stage after all OSB and other
#' associated calculations have been done. It organizes and prints all
#' the results and outputs to the R console as the final results
#'
#' @param my_env My environment my_env has all the solutions stored
#' in different variables returned by various functions
#'
#' @return \code{} returns the quantities such as OSB, Wh, Vh, WhSh,
#' nh, Nh and fh
#'
#' @author Karuna Reddy <reddy_k@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
#'@examples
#' \dontrun{
#' distr.res()
#' }
#'
distr.res <- function(my_env)
  {
  h <- data.frame("Strata" = 1:my_env$h)

  #some outputs from strata.distr()
  ObjFV <- my_env$ObjFV #this is for scaled data

  Distance <- (my_env$maxval)*(my_env$df)$d
  OSB <- round(((my_env$maxval)*(my_env$df)$x), digits=2) #convert from scaled osb to real osb by *max(data)

  distr <- as.character(my_env$obj["distr"])

  #some outputs from distr.alloc()
  #a <- b <- g <- gam <- lambda <- m <- maxx <- minn <- mu <- r <- rate <- s <- sig <- theta <- NULL

  distr.alloc(my_env)
  Output <- my_env$output  #is a df with Wh, Vh & WhSh
  out <- my_env$out #is a df with nh, Nh & sf
  deno <- my_env$deno #total of WhSh
  WhTot <- my_env$WhTot #tot of stratum weights
  NhTot <- my_env$NhTot #tot pop units in strata h
  nhTot <- my_env$nhTot #tot sample units in strata h
  fhTot <- my_env$fhTot #stratum sampling fraction
  VhTot <- my_env$VhTot #tot stratum variances

  DF1 <- data.frame(h, OSB, Output, out)
  DF2 <- data.frame("Strata"="Total","OSB"="",
                    "Wh"=WhTot,"Vh"=VhTot,"WhSh"=deno,
                    "nh"=nhTot,"Nh"=NhTot, "fh"=fhTot)
  DF <- rbind(DF1, DF2)

  #now writing the outputs
  cat("_____________________________________________\n")
  cat("Optimum Strata Boundaries for h =",my_env$h, "\n", sep = " ")
  cat("Data Range: [",(my_env$maxval)*my_env$initval,", ",
      (my_env$maxval)*my_env$finval,"]",
      " with d = ", (my_env$maxval)*my_env$dist, "\n", sep = "")
  cat("Best-fit Frequency Distribution: ", paste(distr), "\n", sep = " ")
  cat("Parameter estimate(s): ", "\n")
  print(my_env$obj["params"]$params)

  cat("______________________________________________________\n")
  print(DF, row.names = FALSE)
  cat("______________________________________________________\n")
}
#########################################################################
