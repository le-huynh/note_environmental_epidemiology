#'---
#' title: Extensions of `dlnm` package
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        dlnm,
        splines,
        skimr,
        survival,
        mgcv
)

#' # Data
# data #-------------------
#' ## `drug`: A Trial on the Effect of Time-Varying Doses of a Drug
## drug #----------------------------
#' - `id`: subject ID.  
#' - `out`: the outcome level measured at day 28.  
#' - `sex`: the sex of the subject.  
#' - `day1.7`, `day8.14`, `day15.21`, `day22.28`: daily dose for 
#' the 1st, 2nd, 3rd, 4th week.  

tibble(drug)

skimr::skim(drug)

#' ## `nested`: Nested Case-Control Study with a Time-Varying Exposure and a Cancer Outcome
## nested #------------------------
#' - `id`: subject ID.  
#' - `case`: indicator for case (1) or control (0).  
#' - `age`: age of each subject.  
#' - `riskset`: risk set id.  
#' - `exp15`, `exp20`, ..., `exp60`: yearly exposure in the age period 
#' (15-19), (20-24), ..., (60-64) year.

tibble(nested)

nested %>% nest(.by = id)

nested %>% nest(.by = id) %>% pluck("data", 5)

skimr::skim(nested)

#' # Matrix of exposure history
# matrix of exposure history #-----------------------
#' ## `drug`
## drug #--------------------
#' **Note:** exposure at lag0 → lag27: day 28th → day 1st

#---
#' ### Reference code
### reference code #--------------
rep(7:4, each = 7)

drug[, rep(7:4, each = 7)] %>% tibble()

Qdrug <- as.matrix(drug[, rep(7:4, each = 7)])
Qdrug[1:3, 1:9]

colnames(Qdrug) <- paste("lag", 0:27, sep = "")
Qdrug[1:3, 1:14]

#' ### `tidyverse` style
### tidyverse style #--------------------

(week4 <- map_dfc(1:7, ~ drug$day22.28) %>% 
        rename_with(~paste0("lag", 0:6), everything()))

(week3 <- map_dfc(1:7, ~ drug$day15.21) %>% 
                rename_with(~paste0("lag", 7:13), everything()))

(week2 <- map_dfc(1:7, ~ drug$day8.14) %>% 
                rename_with(~paste0("lag", 14:20), everything()))

(week1 <- map_dfc(1:7, ~ drug$day1.7) %>% 
                rename_with(~paste0("lag", 21:27), everything()))

Qdrug_tidy <- bind_cols(week4, week3, week2, week1) %>% 
        as.matrix()
Qdrug_tidy[1:3, 1:14]

#' ## `nested`
## nested #--------------------
#' **Note:** read vignette `dlnmExtended` in `dlnm` package for details.  
Qnest <- t(apply(nested, 
                 1, 
                 function(sub) exphist(rep(c(0, 0, 0, sub[5:14]), each = 5), 
                                       sub["age"], 
                                       lag = c(3, 40)))
           )

colnames(Qnest) <- paste0("lag", 3:40)
Qnest[1:3,1:16]

#' # Simple DLM
# simple DLM #-------------------
#' Find temporal dependency between daily dose of drug and health outcome.  

#' **Step1**: cross-basis matrix of dose and lag time
# step1: cross-basis matrix
cbdrug <- crossbasis(Qdrug, # matrix of exposure history
                     # lag period, minimum lag = 0 by default
                     # MUST be consistent with dimension (i.e. number of columns)
                     # of exposure history matrix
                     lag = 27, 
                     # assume dose and outcome have linear association
                     argvar = list("lin"),
                     # lag: natural cubic spline with 2 knots at lag9 and lag18
                     arglag = list(fun = "ns", knots = c(9, 18)))

summary(cbdrug)

#' **Step2**: model fitting. Include cross-basis matrix formula of regression model
# step2: model fitting
mod_drug <- lm(out ~ cbdrug + sex, # controlling for effect of sex
               data = drug)

summary(mod_drug)

#' **Step3**: Interpretation. Predict specific effect of dose and lag on outcome.
# step3: prediction
# reference value: set by default to 0 for `lin()`
pdrug <- crosspred(cbdrug, mod_drug, at = 0:20*5)

#' `all-`: Overall cumulative effects associated with exposure to 50.  
#' - `backward` perspective: total effect today after being exposed to 50 for the past 28 days.  
#' - `forward` perspective: total effect over the next 28 days after being exposed to 50 today.

with(pdrug, cbind(allfit, alllow, allhigh)["50", ])

#' `mat-`: specific combinations of exposure levels and lag.  
# increase in outcome associated with dose 20, three days earlier
pdrug$matfit["20","lag3"]

#' **Step4**: Plot  
#' Effect of a dose of the drug is pronounced in the 1st days after the intake 
#' and then tends to disappear after 15-20 days
plot(pdrug, zlab = "Effect", xlab = "Dose", ylab = "Lag (days)")

#' Lag-response curve specific to exposure 60: exponential decay in effects
plot(pdrug, var = 60, ylab = "Effect at dose 60", xlab = "Lag (days)", ylim = c(-1,5))

#' Exposure-response curve specific to lag 10
plot(pdrug, lag = 10, ylab = "Effect at lag 10", xlab = "Dose", ylim = c(-1,5))

#' # DLNM
# DLNM #-------------------
#' How long-term exposures to an agent at work affect the risk of developing cancer.  

#' **Step1**: Cross-basis
cbnest <- crossbasis(Qnest,
                     lag = c(3, 40),
                     # quadratic splines, single knot at median by default
                     argvar = list(fun = "bs", degree = 2, df = 3),
                     # natural cubic splines
                     # exclude intercept: null effect at beginning of lag period
                     arglag = list(fun = "ns", knots = c(10, 30), intercept = F))

summary(cbnest)

#' **Step2**: Model fitting: conditional logistic regression
mod_nest <- survival::clogit(case ~ cbnest + strata(riskset), data = nested)

summary(mod_nest)

#' **Step3**: Interpretation  
#' Note: MUST provide `cen =`, as no straightforward reference exist for 
#' non-linear functions (e.g. `bs()`).  
pnest <- crosspred(cbnest, mod_nest, cen = 0, at = 0:20*5)

#' **Step4**: Plot  
#' Initial increase in risk, measured as odds ratio (OR), followed by a decrease.
plot(pnest, zlab = "Odd ratio", xlab = "Exposure", ylab = "Lag (years)")

#' Estimated lag-response curve displays a peak in risk 10 to 15 years after 
#' the exposure. 
#' Then 30 years after the exposure, the risk returns to the baseline level 
#' (wide confidence intervals). 
plot(pnest,
     var = 50, 
     ylab = "Odd ratio for exposure 50",
     xlab = "Lag (years)",
     xlim = c(0, 40))

#' Exposure-response curve suggests that the relationship between exposure and 
#' outcome becomes weaker at higher exposure levels. 
plot(pnest,
     lag = 5,
     ylab = "Odd ratio at lag 5",
     xlab = "Exposure",
     ylim = c(0.95, 1.15))

#' # User-defined functions in `onebasis()` and `crossbasis()`
# user-defined function #------------------------
#' ## Example1
## example1 #-----------
mylog <- function(x) log(x+1)

# cross-basis
cbnest2 <- crossbasis(Qnest,
                     lag = c(3, 40),
                     # user-defined function
                     argvar = list(fun = "mylog"),
                     # natural cubic splines
                     # exclude intercept: null effect at beginning of lag period
                     arglag = list(fun = "ns", knots = c(10, 30), intercept = F))

summary(cbnest2)

# model fitting
mod_nest2 <- clogit(case ~ cbnest2 + strata(riskset), data = nested)
summary(mod_nest2)

# prediction
pnest2 <- crosspred(cbnest2, mod_nest2, cen = 0, at = 0:20*5)

# plot
plot(pnest2, zlab = "OR", xlab = "Exposure", ylab = "Lag (years)")

plot(pnest2, var = 50, ylab = "OR for exposure 50", xlab = "Lag (years)", xlim = c(0, 40))
lines(pnest, var = 50, lty = 2)

plot(pnest2, lag = 5, ylab = "OR at lag 5", xlab = "Exposure", ylim = c(0.95, 1.15))
lines(pnest, lag = 5, lty = 2)

#' ## Example2
## example2 #-----------
fdecay <- function(x, scale = 5) { 
        
        # scale: control degree of decay
        basis <- exp(-x/scale) 
        
        # must include attribute in the returned vector
        attributes(basis)$scale <- scale 
        return(basis) 
        }

# cross-basis
cbdrug2 <- crossbasis(Qdrug,
                      lag = 27,
                      argvar = list("lin"),
                      arglag = list(fun = "fdecay", scale = 6))
summary(cbdrug2)

# model fitting
mod_drug2 <- lm(out ~ cbdrug2 + sex, data = drug)
summary(mod_drug2)

# prediction
pdrug2 <- crosspred(cbdrug2, mod_drug2, at = 0:20*5)

# plot
plot(pdrug2, zlab = "Effect", xlab = "Dose", ylab = "Lag (days)")

plot(pdrug2, 
     var = 60, 
     ylab = "Effect at dose 60", 
     xlab = "Lag (days)", 
     ylim=c(-1,5))
lines(pdrug, var = 60, lty = 2)

plot(pdrug2,
     lag = 10,
     ylab = "Effect at lag 10",
     xlab = "Dose",
     ylim = c(-1,5))
lines(pdrug, lag = 10, lty = 2)

#' # `dlnm` as a general tool for regression analysis
# general tool #------------------------
#' ## Linear regression
## linear regression #-----------------
# data
tibble(women)

# cross-basis
oneheight <- onebasis(women$height, "ns", df = 5)
summary(oneheight)

# model fitting
mod_women <- lm(weight ~ oneheight, data = women)
summary(mod_women)

# interpretation
pwomen <- crosspred(oneheight, mod_women, cen = 65, at = 58:72)
# `all-`: estimated association
with(pwomen, cbind(allfit, alllow, allhigh)["70",])

# plot
plot(pwomen,
     ci = "l",
     ylab = "Weight (lb) difference", 
     xlab = "Height (in)", 
     col = 4)

#' ## GAM using `mgcv`
## mgcv::gam #-----------------
# data
dat <- gamSim(1, n = 200, dist = "poisson", scale = .1)
tibble(dat)

# model fitting
b2 <- gam(y ~ s(x0,bs = "cr") + 
                  s(x1, bs = "cr") + 
                  s(x2, bs = "cr") + 
                  s(x3, bs = "cr"), 
          family = poisson, 
          data = dat, 
          method = "REML")

summary(b2)

plot(b2, select = 3)

# prediction
pgam <- crosspred("x2", b2, cen = 0, at = 0:100/100)
with(pgam, cbind(allRRfit, allRRlow, allRRhigh)["0.7",])

# plot
plot(pgam, ylim = c(0, 3), ylab = "RR", xlab = "x2", col = 2)

