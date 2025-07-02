#'---
#' title: Test contour plot
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
        tidyverse,      # data management and visualization
        dlnm,
        mgcv,
        splines
)

#' ## Fit example models
# fit example models #---------------------
# pm10, lag = 1
cb1.pm <- crossbasis(chicagoNMMAPS$pm10,
                     lag = 1, 
                     argvar = list(fun = "lin"),
                     arglag = list(fun = "strata"))

# pm10, lag = 7
cb2.pm <- crossbasis(chicagoNMMAPS$pm10,
                     lag = 7, 
                     argvar = list(fun = "lin"),
                     arglag = list(fun = "strata"))
                          
varknots <- equalknots(chicagoNMMAPS$temp, fun = "bs", df = 5, degree = 2)
lagknots <- logknots(30, 3)
cb.temp <- crossbasis(chicagoNMMAPS$temp,
                       lag = 30,
                       argvar = list(fun = "bs", knots = varknots),
                       arglag = list(knots = lagknots))

# model with lag = 1
model1 <- glm(death ~ cb1.pm + cb.temp + ns(time, 7 * 14) + dow,
              family = quasipoisson(),
              data = chicagoNMMAPS)
pred1.temp <- crosspred(cb.temp, model1, cen = 21, by = 1)

# model with lag = 7
model2 <- glm(death ~ cb2.pm + cb.temp + ns(time, 7 * 14) + dow,
              family = quasipoisson(),
              data = chicagoNMMAPS)
pred2.temp <- crosspred(cb.temp, model2, cen = 21, by = 1)

#' ## Default contour plot in `dlnm` package
# default dlnm #----------------------
#' By default, "Arguments `x-y-z` and `col-level` are automatically set and 
#' CAN NOT BE SPECIFIED BY THE USER.  
plot(pred1.temp,
     "contour",
     xlab = "Temperature",
     key.title = title("RR"),
     plot.title = title("Contour plot, lag=1", xlab = "Temperature", ylab = "Lag"))

plot(pred2.temp,
     "contour",
     xlab = "Temperature",
     key.title = title("RR"),
     plot.title = title("Contour plot, lag=7", xlab = "Temperature", ylab = "Lag"))

#' ## Modified plot using `graphics` package
# graphics:: #------------------
#' The function `plot.contour()` below is modified version of 
#' `dlnm::plot.crosspred()` to allow user-defined color levels through 
#' argument `levels`.  

plot.contour <- function(x,
                         z_range = NULL,
                         nlevels = 20,
                         color1 = "blue",
                         color2 = "white",
                         color3 = "red",
                         ci.level = x$ci.level,
                         exp = NULL, 
                         ...) {
                
                if(all(class(x)!="crosspred")) stop("'x' must be of class 'crosspred'")

                if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0) {
                        stop("'ci.level' must be numeric and between 0 and 1")
                }
                if(!is.null(exp)&&!is.logical(exp)) stop("'exp' must be logical")
                
        # COMPUTE OUTCOMES
                # SET THE Z LEVEL EQUAL TO THAT STORED IN OBJECT IF NOT PROVIDED
                z <- qnorm(1-(1-ci.level)/2)
                x$mathigh <- x$matfit+z*x$matse
                x$matlow <- x$matfit-z*x$matse
                x$allhigh <- x$allfit+z*x$allse
                x$alllow <- x$allfit-z*x$allse
                noeff <- 0

                # EXPONENTIAL
                if((is.null(exp)&&!is.null(x$model.link)&&x$model.link%in%c("log","logit"))||
                   (!is.null(exp)&&exp==TRUE)) {
                        x$matfit <- exp(x$matfit)
                        x$mathigh <- exp(x$mathigh)
                        x$matlow <- exp(x$matlow)
                        x$allfit <- exp(x$allfit)
                        x$allhigh <- exp(x$allhigh)
                        x$alllow <- exp(x$alllow)
                        noeff <- 1
                }

         # CONTOURPLOT
                if(x$lag[2]==0) stop("contour plot not conceivable for unlagged associations")
                
                if(is.null(z_range)) {
                        levels <- pretty(x$matfit, 20)
                } else {
                        levels <- pretty(z_range, nlevels)
                }
                
                col1 <- colorRampPalette(c(color1, color2))
                col2 <- colorRampPalette(c(color2,color3))
                col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
                filled.contour(
                        x = x$predvar,
                        y = dlnm:::seqlag(x$lag, x$bylag),
                        z = x$matfit,
                        col = col,
                        levels = levels,
                        nlevels = nlevels,
                        ...)
                }

#' ### Test function `plot.contour()`  
#' When `z_range` is not provided, the returned plot is contour plot from 
#' `dlnm::plot.crosspred()`.

plot.contour(pred1.temp,
             xlab = "Temperature",
             key.title = title("RR"),
             plot.title = title("Contour plot, lag=1", xlab = "Temperature", ylab = "Lag"))

plot.contour(pred2.temp,
             xlab = "Temperature",
             key.title = title("RR"),
             plot.title = title("Contour plot, lag=7", xlab = "Temperature", ylab = "Lag"))

#' Change `z_range` and `nlevels` to get user-defined color levels.
rr_range <- c(0.8, 1.3)
nlevels <- 30

plot.contour(x = pred1.temp,
             z_range = rr_range,
             nlevels = nlevels,
             xlab = "Temperature",
             key.title = title("RR"),
             plot.title = title("Contour plot, lag=1", xlab = "Temperature", ylab = "Lag"))

plot.contour(x = pred2.temp,
             z_range = rr_range,
             nlevels = nlevels,
             xlab = "Temperature",
             key.title = title("RR"),
             plot.title = title("Contour plot, lag=7", xlab = "Temperature", ylab = "Lag"))

#' Change colors
plot.contour(x = pred2.temp,
             z_range = rr_range,
             nlevels = nlevels,
             color1 = "red",
             color3 = "blue",
             xlab = "Temperature",
             key.title = title("RR"),
             plot.title = title("Contour plot, lag=7", xlab = "Temperature", ylab = "Lag"))

