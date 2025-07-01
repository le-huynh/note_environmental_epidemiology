#'---
#' title: DLNM for time series data 01
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
        skimr
)

#' ## Data
# data #-----------
chicagoNMMAPS %>% tibble()

skimr::skim(chicagoNMMAPS)

#' ## Example 1: simple DLM
# eg1: simple DLM #------------------------
#' **Goal:** Assessing effect of PM10 on mortality, while adjusting for effect of temperature.  
#' 
#' ### **Step1**: build 2 cross-basis matrices for 2 predictors
## step1: cross-basis #--------------------------
#' #### PM10
cb1_pm <- crossbasis(chicagoNMMAPS$pm10,
                     # lagged effect of PM10 up to 15 days, minimum lag = 0 by default
                     lag = 15,
                     # assume linear effect for PM10 
                     argvar = list(fun = "lin"),
                     # lagged effect:  4th degree polynomial function
                     arglag = list(fun = "poly", degree = 4))

summary(cb1_pm)

cb1_pm

#' #### temperature
cb1_temp <- crossbasis(chicagoNMMAPS$temp,
                     lag = 3,
                     # non-linear effect of temperature, natural cubic splines
                     argvar = list(fun = "ns", df = 5),
                     arglag = list(fun = "strata", 
                                   # lower boundary of the second interval
                                   breaks = 1))

summary(cb1_temp)

cb1_temp

#' ### **Step2**: fit model
## step2: fit model #--------------------------
model1 <- glm(death ~ cb1_pm + cb1_temp + ns(time, 7*14) + dow,
              family = quasipoisson(),
              data = chicagoNMMAPS)

summary(model1)

#' ### **Step3**: estimated association with specific levels of PM10 on mortality
## step3: interpretation #--------------------------
pred1_pm <- crosspred(cb1_pm,
                      model1,
                      # compute prediction for each integer value from 0 to 20 (ugr/m3)
                      at = 0:20,
                      # compute prediction along lag space with increment of 0.2
                      bylag = 0.2,
                      # include incremental cumulative associations along lags
                      # note: only returned for integer lags
                      cumul = TRUE)

#' ### **Step4**: visualization
## step4: visualization #--------------------------
#' Note: This association is defined using the reference value of 0 µgr/m3, 
#' thus providing the predictor-specific association for a 10-unit increase.
plot(pred1_pm, 
     # relationship corresponding to specific values of predictor and lag
     ptype = "slices",
     # lag-response relationship for a specific value of PM10 (10 ugr/m3)
     var = 10,
     col = 3, 
     ylab = "RR",
     ci.arg = list(density = 15, lwd = 2),
     main = "Association with a 10-unit increase in PM10")

plot(pred1_pm, 
     ptype = "slices",
     var = 10, 
     col = 2, 
     cumul = TRUE, 
     ylab = "Cumulative RR",
     main = "Cumulative association with a 10-unit increase in PM10")

#' **Interpretation:**  
#' - **lag curve** represents increase in risk in each future day following 
#' an increase of 10 µgr/m3 in PM10 in a specific day.  
#' - initial increase in risk of PM10 is reversed at longer lags.  

#---
#' ## Example 2: seasonal analysis
# eg2: seasonal analysis #------------------------
#' Data are restricted to a specific season:  
#' - not represent a single continuous series.  
#' - be composed by multiple equally-spaced and ordered series of 
#' multiple seasons in different years.  

(chicagoNMMAPSseas <- chicagoNMMAPS %>% 
        filter(month %in% 6:9) %>% 
        tibble())

#' ### **Step1**: build 2 cross-basis matrices for 2 predictors
## step1: cross-basis #--------------------------
#' #### ozone
cb2_o3 <- crossbasis(chicagoNMMAPSseas$o3, 
                     lag = 5,
                     # assumption: effect of O3 is null up to 40.3 µgr/m3, then linear
                     argvar = list(fun = "thr", # high threshold parameterization
                                   thr = 40.3),
                     # one parameter for each lag(??),  up to 5 days (minimum-lag = 0 by default)
                     arglag = list(fun = "integer"),
                     group = chicagoNMMAPSseas$year)
summary(cb2_o3)

#' #### temperature
cb2_temp <- crossbasis(chicagoNMMAPSseas$temp,
                       lag = 10,
                       # assumption: temp effect is linear below 15◦C and above 25◦C, 
                       # null in between
                       argvar = list(fun = "thr", thr = c(15, 25)),
                       # 3 strata intervals at lag 0-1, 2-5, 6-10
                       arglag = list(fun = "strata", breaks = c(2, 6)),
                       group = chicagoNMMAPSseas$year)
summary(cb2_temp)

#' ### **Step2**: fit model
## step2: fit model #--------------------------
model2 <- glm(death ~ cb2_o3 + cb2_temp + ns(doy, 4) + ns(time, 3) + dow,
              family = quasipoisson(), 
              data = chicagoNMMAPSseas)

summary(model2)

#' ### **Step3**: estimated association with specific levels of ozone on mortality
## step3: interpretation #--------------------------
#' Note: reference value is automatically selected
pred2_o3 <- crosspred(cb2_o3,
                      model2,
                      # compute prediction from 0 to 65 µgr/m3 (approx. range of ozone)
                      # + threshold 40.3 µgr/m3
                      # + value 50.3 µgr/m3 → 10-unit increase above threshold
                      at = c(0:65, 40.3, 50.3))

#' ### **Step4**: visualization
## step4: visualization #--------------------------
plot(pred2_o3, 
     ptype = "slices",
     var = 50.3, 
     ci = "bars", 
     type = "p", # points, not default line
     col = 2, 
     pch = 19,
     ci.level = 0.80, 
     main = "Lag-response a 10-unit increase above threshold (80CI)")

plot(pred2_o3,
     ptype = "overall", # overall cumulative association
     xlab = "Ozone", 
     ci = "l", # lines
     col = 3, 
     ylim = c(0.9, 1.3), # range of y-axis
     lwd = 2,
     ci.arg = list(col = 1, lty = 3), 
     main = "Overall cumulative association for 5 lags")

#' ## Example 3:  bi-dimensional DLNM
# eg3:  bi-dimensional DLNM #------------------------

#' ### **Step1**: build 2 cross-basis matrices for 2 predictors
## step1: cross-basis #--------------------------
#' #### PM10
cb3_pm <- crossbasis(chicagoNMMAPS$pm10,
                     lag = 1, 
                     argvar = list(fun = "lin"),
                     # single strata up to lag 1, minimum lag = 0 by default
                     arglag = list(fun = "strata"))
summary(cb3_pm)

#' #### temperature
# knots placed by default at equally spaced value in the space of the predictor
(varknots <- equalknots(chicagoNMMAPS$temp,
                       fun = "bs", 
                       df = 5,
                       degree = 2))

# knots for spline for lags are placed at equally-spaced values in log scale of lags
(lagknots <- logknots(30, 3))

cb3_temp <- crossbasis(chicagoNMMAPS$temp, 
                       lag = 30, 
                       argvar = list(fun = "bs", # quadratic B-spline
                                     knots = varknots), 
                       arglag = list(fun = "ns",
                                     knots = lagknots))

summary(cb3_temp)

#' ### **Step2**: fit model
## step2: fit model #--------------------------
model3 <- glm(death ~ cb3_pm + cb3_temp + ns(time, 7*14) + dow,
              family = quasipoisson(), 
              data = chicagoNMMAPS)

summary(model3)

#' ### **Step3**: estimated association with specific temperature on mortality
## step3: interpretation #--------------------------
pred3_temp <- crosspred(cb3_temp, 
                        model3, 
                        # prediction values are centered at 21◦C
                        # reference point for the interpretation of estimated effects
                        cen = 21, 
                        # predict all integer values within predictor range
                        by = 1)

#' ### **Step4**: visualization
## step4: visualization #--------------------------
#' #### 3D plot
plot(pred3_temp, 
     xlab = "Temperature", 
     zlab = "RR", 
     theta = 200, 
     phi = 40, 
     lphi = 30,
     main = "3D graph of temperature effect")

#' #### Contour plot
#' Lag-response curves specific to mild, extreme cold, extreme hot temperatures 
#' of -20◦C, 0◦C, 27◦C, and 33◦C (with reference at 21◦C).
plot(pred3_temp, 
     ptype = "contour", 
     xlab = "Temperature", 
     key.title = title("RR"),
     plot.title = title("Contour plot",
                        xlab = "Temperature",
                        ylab = "Lag"))

plot(pred3_temp,
     ptype = "slices",
     var = -20, 
     ci = "n", # none
     col = 1, 
     ylim = c(0.95, 1.25), 
     lwd = 1.5,
     main = "Lag-response curves for different temperatures, ref. 21C")

for(i in 1:3) lines(pred3_temp,
                    "slices",
                    var = c(0, 27, 33)[i], 
                    col = i+1, 
                    lwd = 1.5)

legend("topright",
       paste("Temperature =", c(-20, 0, 27, 33)), 
       col = 1:4, 
       lwd = 1.5)

plot(pred3_temp, 
     "slices", 
     var = c(-20, 33), 
     lag = c(0, 5), 
     col = 4,
     ci.arg = list(density = 40, col = grey(0.7)))

#' **Interpretation:** Cold temperatures are associated with longer mortality 
#' risk than heat, but not immediate, showing a "protective" effect at lag 0.

#---
#' ## Example 4: reducing a DLNM
# eg4: reducing a DLNM #------------------------
#' cross-basis
cb4 <- crossbasis(chicagoNMMAPS$temp,
                  lag = 30,
                  # double-threshold functions with cut-off points at 10oC, 25oC
                  argvar = list(fun = "thr",
                                thr = c(10, 25)), 
                  # natural cubic splines with knots at equally-spaced values 
                  # in log scale for lags
                  arglag = list(fun = "ns",
                                knots = lagknots))
summary(cb4)

#' fit model
model4 <- glm(death ~ cb4 + ns(time, 7*14) + dow,
              family = quasipoisson(), 
              data = chicagoNMMAPS)
summary(model4)

#' interpretation
pred4 <- crosspred(cb4, model4, by = 1)

#' reduction for overall cumulative summary (exposure-response relationship)
redall <- crossreduce(cb4, model4)

#' reduction for lag-specific summary (exposure-response relationship)
redlag <- crossreduce(cb4, model4, type = "lag", value = 5)

#' reduction for predictor-specific summary (lag-response relationship)
redvar <- crossreduce(cb4, model4, type = "var", value = 33)

#' number of parameters
length(coef(pred4))

#' number of parameters has been reduced to 2 for the space of the predictor
length(coef(redall))
length(coef(redlag))

#' number of parameters has been reduced to 5 for the space of lag
length(coef(redvar))

#' Note:  prediction from the original and reduced fit is identical
plot(pred4, 
     "overall", 
     xlab = "Temperature", 
     ylab = "RR",
     ylim = c(0.8, 1.6), 
     main = "Overall cumulative association")

lines(redall, 
      ci = "lines",
      col = 4,
      lty = 2)

legend("top",
       c("Original", "Reduced"),
       col = c(2, 4),
       lty = 1:2,
       ins = 0.1)

