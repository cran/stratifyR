#' Stratification of Univariate Survey Population Using the Distribution
#'
#' This function takes in the underlying hypothetical distribution and
#' its parameter(s) of the survey variable, the initial value and
#' the range of the population, the fixed sample size (n) and the
#' fixed population size (N) to compute the optimum stratum boundaries
#' (OSB) for a given number of strata (L), optimum sample sizes (nh),
#' etc. The main idea used is from Khan et al (2008) whereby the
#' problem of stratification is fromulated into a Mathematical Programming
#' Problem (MPP) using the best-fit frequency distribution and its
#' parameter estimates of the data. This MPP is then solved for the
#' optimal solutions using the Dynamic Programming (DP) solution procedure.
#'
#' @param h A numeric: denotes the number of strata to be created.
#' @param initval A numeric: denotes the initial value of the population
#' @param dist A numeric: denotes distance (or range) of the population
#' @param distr A character: denotes the name of the distribution that
#' characterizes the population
#' @param params A list: contains the values of all parameters of the distribution
#' @param n A numeric: denotes the fixed total sample size.
#' @param N A numeric: denotes the fixed total population size.
#' @export
#'
#' @return \code{strata.distr} returns Optimum Strata Boundaries (OSB),
#' stratum weights (Wh), stratum variances (Vh), Optimum Sample Sizes
#' (nh), stratum population sizes (Nh).
#'
#' @author Karuna Reddy <reddy_k@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#' @seealso \code{strata.data}
#'
#' @examples
#' \dontrun{
#' #Assume data has initial value of 1.5, distance of 33 and follows
#' #weibull distribution with estimated parameters as shape=2.15 and scale=13.5
#' #To compute the OSB, OSS, etc. with fixed sample n=500:
#' strata.distr(h=2, initval=1.5, dist=33, distr = "weibull",
#' params = c(shape=2.15, scale=13.5), n=500, N=2000)
#'
#' #-------------------------------------------------------------
#' #Assume data has initial value of 1, distance of 10415 and follows
#' #lnorm distribution with estimated parameters as meanlog=5.5 and sdlog=1.5
#' #To compute the OSB, OSS, etc. with fixed sample n=500:
#' strata.distr(h=2, initval=1, dist=10415, distr = "lnorm",
#' params = c(meanlog=5.5, sdlog=1.5), n=500, N=12000)
#'
#' #-------------------------------------------------------------
#' #Assume data has initial value of 2, distance of 68 and follows
#' #gamma distribution with estimated parameters as shape=3.8 and rate=0.55
#' #To compute the OSB, OSS, etc. with fixed sample n=500:
#' strata.distr(h=2, initval=0.65, dist=68, distr = "gamma",
#' params = c(shape=3.8, rate=0.55), n=500, N=10000)
#' #-------------------------------------------------------------

#'}
#'
strata.distr <- function(h, initval, dist,
                          distr = c("pareto", "triangle", "rtriangle",
                                    "weibull", "gamma", "exp", "unif",
                                    "norm", "lnorm", "cauchy"),
                          params = c(shape=0, scale=0, rate=0, gamma=0,
                                     location=0, mean=0, sd=0, meanlog=0,
                                     sdlog=0, min=0, max=0, mode=0), n, N)
{
  #checking if args are correct
  if (missing(h))
    stop("Number of strata ('h') must be specified")
  if (missing(initval))
    stop("Initial value ('initval') must be specified")
  if (missing(dist))
    stop("Range of data ('dist') must be specified")
  if (missing(distr))
    stop("Distribution of data ('distr') must be specified")
  if (missing(params))
    stop("The parameters of the distribution must be specified")
  if (missing(n) & missing(N))
    stop("'n' or 'N' must be specified")

  #create a new env and put params in it to be used by other functions
  my_env <- new.env(parent = emptyenv())

  #store values entered by user in my_env
  my_env$h <- h
  my_env$n <- n
  my_env$N <- N

  #evaluate some values to scale data
  my_env$maxval <- initval + dist #max value of original data
  my_env$initval <- initval/my_env$maxval #min val of scaled data
  my_env$finval <- my_env$maxval/my_env$maxval #finval = 1 for scaled data
  my_env$dist <- my_env$finval - my_env$initval #dist of scaled data (< 1)

  #store distr and params in a list called obj
  my_env$obj <- list("distr" = distr, "params" = params)

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

  dist <- my_env$dist #scaled dist on which osb is created
  initval <- my_env$initval #scaled intival

  #create osb for base case of k=1
  my_env$ObjFV <- distr.optim(k=h, n=p, incf=inc, minYk=0, maxYk=p, isFirstRun=TRUE, my_env)

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
    my_env$ylimits[i+1] <- (y[i]*points*z)
  }
  #create osb for base case of k=2
  my_env$ObjFV <- distr.optim(k=h, n=e-1, incf=inc2,
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
  distr.res(my_env)
}
###################################################################
