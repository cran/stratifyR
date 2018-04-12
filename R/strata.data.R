#' Stratification of Univariate Survey Population Using the Data
#'
#' This function takes in the univariate population data
#' (argument \code{data}) and a fixed sample size (n)
#' to compute the optimum stratum boundaries (OSB) for a
#' given number of strata (L), optimum sample sizes (nh),
#' etc directly from the data. The main idea used is from
#' Khan et al (2008) whereby the problem of stratification
#' is formulated into a Mathematical Programming Problem (MPP)
#' using the best-fit frequency distribution and its parameters
#' estimated from the data. This MPP is then solved for the
#' OSB using a Dynamic Programming (DP) solution procedure.
#'
#' @param data A vector of values of the survey variable y for
#' which the OSB are determined
#' @param h A numeric: denotes the number of strata to be created.
#' @param n A numeric: denotes a fixed total sample size.
#' @export
#'
#' @return \code{strata.distr} returns Optimum Strata Boundaries (OSB),
#' stratum weights (Wh), stratum variances (Vh), Optimum Sample Sizes
#' (nh), stratum population sizes (Nh).
#'
#' @author Karuna Reddy <reddy_k@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#'@seealso \code{strata.distr}
#'
#' @examples
#' \dontrun{
#' data(anaemia)
#' Iron <- anaemia$Iron
#' strata.data(Iron, h = 2, n=350)
#' #--------------------------------------------------------
#' data(SHS) #Household Spending data from stratification package
#' weight <- SHS$WEIGHT
#' hist(weight)
#' length(weight)
#' strata.data(weight, h = 2, n=500)
#' #------------------------------------------
#' data(sugarcane)
#' Production <- sugarcane$Production
#' hist(Production)
#' strata.data(Production, h = 2, n=1000)
#' #------------------------------------------
#' }
#'
strata.data <- function(data, h, n)
{
   #checking if args are correct
   if (missing(data))
      stop("'data' must be specified")
   if (missing(h))
      stop("'h' must be specified")
   if (missing(n))
      stop("'n' must be specified")
  if (n > length(data))
    stop("Your 'n' cannot be greater than 'N'")

   #create a new env and put params in it, this env is used to
   #store other variables as well that are used by other functions
   my_env <- new.env(parent = emptyenv())
   my_env$h <- h
   my_env$n <- n

   #if 0's exist, replace them with very small vals
   if(any(data == 0)){
      data[data == 0] <- eps <- 1e-5 #replace 0's with eps
   }
   else{data <- data}

   #compute max value from real data & store in my_env
   my_env$maxval <- max(data)
   #scale the data
   scaled_data <- data/my_env$maxval

   #compute values from scaled data, store in my_env
   my_env$initval <- min(scaled_data) #x0
   my_env$finval <- max(scaled_data) #xL
   my_env$dist <- max(scaled_data)-min(scaled_data) #d

   my_env$obj <- get.dist(scaled_data, my_env)  # data is scaled in get.dist()

   #initialize some constants in the new env
   my_env$z <- 100
   my_env$factor <- 4
   my_env$inc <- 0.001
   my_env$inc2 <- 0.00001
   my_env$points <- 1000
   my_env$stages <- h+1
   my_env$ylimits <- integer(h+1)
   my_env$p <- as.integer(my_env$dist*my_env$points)
   my_env$e <- as.integer(my_env$dist*(my_env$points)*(my_env$z)+1)

   #create the matrix to store results
   create.mat(my_env)
   dk2 <- my_env$dk2

   #access the constants to be used
   z <- my_env$z
   factor <- my_env$factor
   inc <- my_env$inc
   inc2 <- my_env$inc2
   points <- my_env$points
   ylimits <- my_env$ylimits
   p <- my_env$p
   e <- my_env$e
   dist <- my_env$dist
   initval <- my_env$initval

   #create osb for base case of k=1
   my_env$ObjFV <- data.optim(k=h, n=p, incf=inc, minYk=0, maxYk=p, isFirstRun=TRUE, my_env)

   #create vectors to store results
   d <- double(h)
   y <- double(h)
   x <- double(h)

   temp <- 0

   #3dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- dist
         y[i] <- my_env$dk2[i+1, p+1]
         x[i] <- initval + dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }

   for(i in h:1)
   {
      my_env$ylimits[i+1] <- as.integer(y[i]*points*z)
   }

   #create osb for base case of k=2
   my_env$ObjFV <- data.optim(k=h, n=e-1, incf=inc2,
                             minYk=my_env$ylimits[h+1]-my_env$factor*my_env$z,
                             maxYk=my_env$ylimits[h+1]+my_env$factor*my_env$z,
                             isFirstRun=FALSE, my_env)  # for k>=2
   #6dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- dist
         y[i] <- my_env$dk2[i+1, e]
         x[i] <- initval + dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points*z)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }
   #organize outputs
   my_env$df <- data.frame(h, d, y, x)
   data.res(data, my_env)
}
#########################################################################
