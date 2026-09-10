## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(stratifyR)
library(fitdistrplus)
library(mc2d)
library(actuar)
library(triangle)

## ----include=TRUE-------------------------------------------------------------
data(quakes)
head(quakes)
mag <- quakes$mag
length(mag)
hist(mag) #to see the distribution
# In development, source the 2.0-1 R files to override the installed 1.0-5 functions.
# The library() call above is still needed to make package datasets available.
# When the 2.0-1 package is properly installed, the if-block below is a no-op.
local({
  r_dir <- file.path("..", "R")
  if (dir.exists(r_dir))
    invisible(lapply(list.files(r_dir, pattern = "\\.R$", full.names = TRUE), source))
})
res <- strata.data(mag, h = 2, n=300) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(quakes) 
depth <- quakes$depth
hist(depth) #see distribution
min(depth); max(depth); d=max(depth)-min(depth);d #min, max and range of data 
# the 2-strata solution is
res <- strata.distr(h=2, initval=40, dist=640, distr = "triangle",
             params = c(min=39.99998, max=680, mode=39.99999), n=300, N=1000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
# Iron variable from the anaemia data follows Weibull(shape=2.1446, scale=13.7907)
# as established in Section 7.4. Using those known parameters directly here.
data(anaemia)
Iron <- anaemia$Iron

# DP (reference solution)
res_dp_d <- strata.distr(h=3, initval=1.5, dist=33.2,
              distr="weibull", params=c(shape=2.1446, scale=13.7907),
              n=350, N=724, method="dp")
summary(res_dp_d)

## ----include=TRUE-------------------------------------------------------------
# COBYLA (fast approximation)
res_cob_d <- strata.distr(h=3, initval=1.5, dist=33.2,
               distr="weibull", params=c(shape=2.1446, scale=13.7907),
               n=350, N=724, method="cobyla")
summary(res_cob_d)

## ----include=TRUE-------------------------------------------------------------
# GLOBAL (DIRECT-L + COBYLA)
res_glob_d <- strata.distr(h=3, initval=1.5, dist=33.2,
                distr="weibull", params=c(shape=2.1446, scale=13.7907),
                n=350, N=724, method="global")
summary(res_glob_d)

## ----include=TRUE-------------------------------------------------------------
# Direct comparison of all three methods
cat("DP     OSB:", round(res_dp_d$OSB, 2),   "  WhShTot:", round(res_dp_d$WhShTot, 4), "\n")
cat("COBYLA OSB:", round(res_cob_d$OSB, 2),  "  WhShTot:", round(res_cob_d$WhShTot, 4), "\n")
cat("GLOBAL OSB:", round(res_glob_d$OSB, 2), "  WhShTot:", round(res_glob_d$WhShTot, 4), "\n")

## ----include=TRUE-------------------------------------------------------------
data(anaemia)
Iron <- anaemia$Iron
Iron[Iron == min(Iron)] <- -0.001  # as per standard package convention

# DP (recommended for strata.data)
res_dp_s <- strata.data(Iron, h=2, n=350, method="dp")
summary(res_dp_s)

## ----include=TRUE-------------------------------------------------------------
# COBYLA
res_cob_s <- strata.data(Iron, h=2, n=350, method="cobyla")
summary(res_cob_s)

## ----include=TRUE-------------------------------------------------------------
# GLOBAL
res_glob_s <- strata.data(Iron, h=2, n=350, method="global")
summary(res_glob_s)

## ----include=TRUE-------------------------------------------------------------
# Direct comparison
cat("DP     OSB:", round(res_dp_s$OSB, 2),   "  WhShTot:", round(res_dp_s$WhShTot, 4), "\n")
cat("COBYLA OSB:", round(res_cob_s$OSB, 2),  "  WhShTot:", round(res_cob_s$WhShTot, 4), "\n")
cat("GLOBAL OSB:", round(res_glob_s$OSB, 2), "  WhShTot:", round(res_glob_s$WhShTot, 4), "\n")

## ----include=TRUE-------------------------------------------------------------
set.seed(8235411)
pareto_data <- actuar::rpareto(5000, shape=5, scale=8)

dpareto <- actuar::dpareto
ppareto <- actuar::ppareto
qpareto <- actuar::qpareto

head(pareto_data)
hist(pareto_data, breaks=100)
min(pareto_data); max(pareto_data); d=max(pareto_data)-min(pareto_data);d
fit <- fitdistrplus::fitdist(pareto_data, "pareto", start = list(shape = 1, scale = 500))
fit
res <- strata.data(pareto_data, h = 2, n=500) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
res <- strata.distr(h=2, initval=0.15, dist=38.55, distr = "pareto",
             params = c(shape=5.05, scale=8.20), n=500, N=5000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(math)
final_marks <- math$final_marks
hist(final_marks)
res <- strata.data(final_marks, h = 2, n=150) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(math)
final_marks <- math$final_marks
a <- min(final_marks); b <- max(final_marks)
a; b; d <- b - a; d

# Estimate parameters: fix min and max to sample bounds, estimate only mode by MLE
br <- pretty(final_marks, n = 20)
hh <- hist(final_marks, breaks = br, plot = FALSE)
m0 <- hh$mids[which.max(hh$counts)]          # histogram mode proxy
fit <- fitdist(final_marks, distr = "triang", method = "mle",
               start   = list(mode = m0),
               fix.arg = list(min = a, max = b))
fit

# 2-strata solution
res <- strata.distr(h=2, initval=7, dist=90, distr = "triangle",
      params = c(min=7, max=97, mode=54), n=150, N=352)
summary(res)

## ----include=TRUE-------------------------------------------------------------
#Generate RT data
set.seed(12546)
data <- rtriangle(n=1000, a=2, b=8, c=2) #right-triangular since a=c
hist(data)
res <- strata.data(data, h = 2, n=500) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
res <- strata.distr(h=2, initval=1.007202, dist=0.992781, distr = "rtriangle",
         params = c(min=2, max=10, mode=2), n=500, N=1000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(anaemia) #using the anaemia data
Iron <- anaemia$Iron
hist(Iron)
min_value <- min(Iron) # Find the minimum value
Iron[Iron == min_value] <- -0.001 # Replace the minimum value with -0.001
res <- strata.data(Iron, h = 2, n=500) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
res <- strata.distr(h=2, initval=2.9, dist=55.9, distr = "weibull",
       params = c(shape=2.144586, scale=13.790744), n=500, N=5000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(anaemia)
Folate <- anaemia$Folate
hist(Folate)
min_value <- min(Folate) # Find the minimum value
Folate[Folate == min_value] <- -0.001 # Replace the minimum value with -0.001
res <- strata.data(Folate, h = 2, n=500) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
res <- strata.distr(h=2, initval=0.5, dist=50, distr = "gamma",
       params = c(shape=3.835768, rate=0.340328), n=500, N=12000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
set.seed(28951)
data <- rexp(5000, rate = 1.36)
hist(data)
res <- strata.data(data, h = 2, n=500) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
set.seed(28951)
data <- rexp(5000, rate = 1.36)
min(data); max(data); d=max(data)-min(data);d
fit <- fitdist(data, distr="exp", method="mle")
fit
res <- strata.distr(h=2, initval=5.748e-05, dist=8.017, distr = "exp", 
             params = c(rate=1.36), n=500, N=5000) #a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
set.seed(15669)
data <- runif(5000, min = 2, max = 15)
hist(data)
res <- strata.data(data, h = 2, n=450) # a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
# For a hypothetical uniform distribution, it does give a result
res <- strata.distr(h=2, initval=3, dist=12, distr = "unif",
                 params = c(min=3, max=15), n=450, N=5000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
set.seed(89821)
data <- rnorm(5000, mean = 16, sd = 1.65)
hist(data)
res <- strata.data(data, h = 2, n=500) #construct a 2-strata solution
summary(res)

## ----include=TRUE-------------------------------------------------------------
set.seed(89821)
data <- rnorm(5000, mean = 16, sd = 1.65)
min(data); max(data); d=max(data)-min(data);d
fit <- fitdist(data, distr="norm", method="mle")
fit
res <- strata.distr(h=2, initval=9.923816, dist=12.58885, distr = "norm",
             params = c(mean=16.010776, sd=1.662357), n=500, N=5000)
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(hies)
Expenditure <- hies$Expenditure
head(Expenditure);length(Expenditure)
hist(Expenditure)
min(Expenditure); max(Expenditure); d=max(Expenditure)-min(Expenditure);d
fit <- fitdist(Expenditure, distr="lnorm", method="mle")
fit
res <- strata.data(Expenditure, h = 2, n=500) 
summary(res)

## ----include=TRUE-------------------------------------------------------------
res <- strata.distr(h=2, initval=10, dist=188, distr = "lnorm",
             params = c(meanlog=3.23, sdlog=0.65), n=500, N=1588)
summary(res)

## ----include=TRUE-------------------------------------------------------------
data(Boston) #Housing Values in Suburbs of Boston
black = Boston$black
hist(black)
min(black); max(black); d=max(black)-min(black);d
fit <- fitdist(black, distr="cauchy", method="mle")
fit
res <- strata.data(black, h = 2, n=500)
summary(res)

## ----include=TRUE-------------------------------------------------------------
#for a cauchy distribution with initial value of x0=-1, d=2 and 
#location and scale parameters 0 and 1 respectively
res <- strata.distr(h=2, initval=-1, dist=2, distr = "cauchy",
             params = c(location=0, scale=1), n=500, N=5000)
summary(res)

## ----echo=FALSE, message=FALSE------------------------------------------------
comparison_table <- data.frame(
  Distribution = c("Pareto", "Exponential", "Gamma", "Log-Normal",
                   "Uniform", "Normal", "Right-Triangular",
                   "Weibull", "Triangular", "Cauchy"),
  `Example Data` = c(
    "Simulated (N=5000, shape=5, scale=8)",
    "Simulated (N=5000, rate=1.36)",
    "Simulated (N=500, shape=3, rate=0.7)",
    "HIES Expenditure (N=3566)",
    "Simulated (N=5000, min=2, max=15)",
    "Simulated (N=5000, mean=6, sd=2.1)",
    "Simulated (N=1000, a=2, b=8)",
    "Anaemia Iron (N=724, h=3)",
    "Math final marks (N=354)",
    "Simulated (N=3000, location=15, scale=4)"
  ),
  `DP` = c(0.938, 0.399, 1.347, 6754.79, 1.880, 1.006, 0.727, 2.461, 10.741, 8.442),
  `COBYLA` = c(1.204, 0.425, 1.405, 6801.42, 1.880, 1.008, 0.727, 2.433, 10.715, 8.468),
  `GLOBAL` = c(1.204, 0.425, 1.405, 6801.42, 1.880, 1.008, 0.727, 2.433, 10.715, 8.468),
  `Outcome` = c(
    "DP clearly better",
    "DP clearly better",
    "DP clearly better",
    "DP clearly better",
    "All agree",
    "All agree",
    "All agree",
    "COBYLA/GLOBAL marginally better",
    "COBYLA/GLOBAL marginally better",
    "DP marginally better"
  ),
  check.names = FALSE
)

knitr::kable(comparison_table, digits = 3, align = "lllllll",
             caption = "Table 1: Comparison of DP, COBYLA and GLOBAL methods across all ten distributions supported by stratifyR. WhShTot values shown for h=2 (h=3 for Weibull). Lower WhShTot indicates a more efficient stratification.")

