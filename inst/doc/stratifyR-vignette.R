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
# Weibull(shape = 2.1446, scale = 13.7907), as fitted to the Iron variable
data(anaemia)
Iron <- anaemia$Iron

# DP (reference solution)
res_dp_d <- strata.distr(h=3, initval=1.5, dist=33.2,
              distr="weibull", params=c(shape=2.1446, scale=13.7907),
              n=350, N=724, method="dp")
summary(res_dp_d)

## ----include=TRUE-------------------------------------------------------------
# COBYLA
res_cob_d <- strata.distr(h=3, initval=1.5, dist=33.2,
               distr="weibull", params=c(shape=2.1446, scale=13.7907),
               n=350, N=724, method="cobyla")

# GLOBAL (DIRECT-L + BOBYQA)
res_glob_d <- strata.distr(h=3, initval=1.5, dist=33.2,
                distr="weibull", params=c(shape=2.1446, scale=13.7907),
                n=350, N=724, method="global")

# Direct comparison of the three methods
cat("DP     OSB:", round(res_dp_d$OSB, 3),   "  WhShTot:", res_dp_d$WhShTot, "\n")
cat("COBYLA OSB:", round(res_cob_d$OSB, 3),  "  WhShTot:", res_cob_d$WhShTot, "\n")
cat("GLOBAL OSB:", round(res_glob_d$OSB, 3), "  WhShTot:", res_glob_d$WhShTot, "\n")

## ----include=TRUE-------------------------------------------------------------
data(anaemia)
Iron <- anaemia$Iron

res_dp_s    <- strata.data(Iron, h=2, n=350, method="dp")
res_cob_s   <- strata.data(Iron, h=2, n=350, method="cobyla")
res_glob_s  <- strata.data(Iron, h=2, n=350, method="global")
res_exact_s <- strata.data(Iron, h=2, n=350, method="exact")
summary(res_exact_s)

# Direct comparison
cat("DP     OSB:", round(res_dp_s$OSB, 3),    "  WhShTot:", res_dp_s$WhShTot, "\n")
cat("COBYLA OSB:", round(res_cob_s$OSB, 3),   "  WhShTot:", res_cob_s$WhShTot, "\n")
cat("GLOBAL OSB:", round(res_glob_s$OSB, 3),  "  WhShTot:", res_glob_s$WhShTot, "\n")
cat("EXACT  OSB:", round(res_exact_s$OSB, 3), "  WhShTot:", res_exact_s$WhShTot, "\n")

## ----include=TRUE-------------------------------------------------------------
data(hies)
expend <- hies$Expenditure
summary(expend)

## ----include=TRUE-------------------------------------------------------------
res <- strata.data(expend, h = 4, n = 400, method = "exact")
res
res$OSB
res$nh
sum(res$nh)    # the stratum sample sizes always add up to n

## ----include=TRUE, fig.width=5, fig.height=3.5--------------------------------
h_values <- 2:7
cost_h <- sapply(h_values, function(h)
  strata.data(expend, h = h, n = 400, method = "cobyla")$WhShTot)
round(cost_h, 1)
round(100 * (1 - cost_h[-1] / cost_h[-length(cost_h)]), 1)   # % drop at each step
plot(h_values, cost_h, type = "b", pch = 19,
     xlab = "Number of strata (h)", ylab = "WhShTot",
     main = "Neyman cost by number of strata")

## ----include=TRUE-------------------------------------------------------------
compare_designs(res)

## ----include=TRUE-------------------------------------------------------------
ch <- c(1, 1, 2, 4)    # relative cost of one interview in each stratum
res_cost <- strata.data(expend, h = 4, n = 400, method = "exact",
                        cost = TRUE, ch = ch)
rbind(equal_costs = res$nh, with_costs = res_cost$nh)
c(equal_costs = sum(res$nh * ch), with_costs = sum(res_cost$nh * ch))   # field cost

## ----include=TRUE, fig.width=6, fig.height=4----------------------------------
plot(res, data = expend)

## ----eval=FALSE---------------------------------------------------------------
# plot(res, type = "3d")            # needs plotly
# plot(res, type = "interactive")   # needs plotly

## ----eval=FALSE---------------------------------------------------------------
# options(stratifyR.exact_max_cand = 50000)
# res_big <- strata.data(big_frame, h = 5, n = 2000, method = "exact")
# res_big$exact_full

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

# Estimate the mode by maximum likelihood, with min and max fixed just outside
# the sample range (the density is zero at min and max, so they cannot equal
# sample values). The likelihood of a triangular mode is maximised at one of
# the data values, so we simply evaluate it at each distinct value. This is
# exact and gives the same answer on every platform.
lo <- a - 0.01 * d; hi <- b + 0.01 * d
loglik <- function(m) sum(log(mc2d::dtriang(final_marks, min = lo, mode = m, max = hi)))
cand <- sort(unique(final_marks))
mode_hat <- cand[which.max(sapply(cand, loglik))]
mode_hat

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
res <- strata.distr(h=2, initval=2, dist=8, distr = "rtriangle",
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
  Section = c("Pareto", "Triangular", "Right-triangular", "Weibull", "Gamma",
              "Exponential", "Uniform", "Normal", "Log-normal", "Cauchy"),
  Data = c("Simulated", "math final marks", "Simulated", "anaemia Iron",
           "anaemia Folate", "Simulated", "Simulated", "Simulated",
           "hies Expenditure", "Boston black"),
  N = c(5000, 487, 1000, 724, 724, 5000, 5000, 5000, 3566, 506),
  `AIC family` = c("pareto", "weibull", "rtriangle", "norm", "norm",
                   "exp", "unif", "norm", "lnorm", "cauchy"),
  DP     = c(0.9276, 7.1172, 0.4889, 2.4638, 2.9357, 0.27360, 1.2471, 0.71698, 4678.5, 20.672),
  COBYLA = c(0.9271, 7.0916, 0.4885, 2.4434, 2.8985, 0.27341, 1.2470, 0.71685, 4598.9, 14.921),
  GLOBAL = c(0.9271, 7.0916, 0.4885, 2.4434, 2.8985, 0.27341, 1.2470, 0.71685, 4598.9, 14.921),
  EXACT  = c(0.9271, 7.0916, 0.4885, 2.4434, 2.8985, 0.27341, 1.2470, 0.71685, 4598.9, 14.921),
  `DP gap (%)` = c(0.05, 0.36, 0.08, 0.84, 1.28, 0.07, 0.01, 0.02, 1.73, 38.5),
  check.names = FALSE
)
knitr::kable(comparison_table, align = "llrlrrrrr",
             caption = "Table 1: Neyman cost (WhShTot) of the four solvers with h = 3 on the examples of Section 9. 'AIC family' is the distribution chosen by AIC, which the DP optimises. 'DP gap' is the DP cost above the exact optimum. Lower WhShTot means a more efficient design.")

## ----eval=FALSE---------------------------------------------------------------
# library(MASS); library(triangle); library(actuar)
# pops <- list()
# set.seed(8235411); pops$Pareto <- actuar::rpareto(5000, shape=5, scale=8)
# data(math); pops$Triangular <- math$final_marks
# set.seed(12546); pops$`Right-triangular` <- rtriangle(n=1000, a=2, b=8, c=2)
# data(anaemia)
# x <- anaemia$Iron;   x[x == min(x)] <- -0.001; pops$Weibull <- x
# x <- anaemia$Folate; x[x == min(x)] <- -0.001; pops$Gamma   <- x
# set.seed(28951); pops$Exponential <- rexp(5000, rate=1.36)
# set.seed(15669); pops$Uniform <- runif(5000, min=2, max=15)
# set.seed(89821); pops$Normal <- rnorm(5000, mean=16, sd=1.65)
# data(hies); pops$`Log-normal` <- hies$Expenditure
# pops$Cauchy <- Boston$black
# 
# tab <- t(sapply(pops, function(d) {
#   n <- min(500, floor(length(d) / 2))
#   sapply(c("dp", "cobyla", "global", "exact"),
#          function(m) strata.data(d, h=3, n=n, method=m)$WhShTot)
# }))
# tab

