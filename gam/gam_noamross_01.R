#'---
#' title: GAM in R - Noam Ross 01
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras,    # supplementary functions
        gamair,         # load mpg data
        mgcv
)

#' ## Data
# data #-----------
data("mpg", package = "gamair")

(mpg <- mpg %>% 
        tibble() %>% 
        mutate(fuel = as.factor(fuel)))

#' ## GAM
# gam #----------------

#' ### 01. Single predictor
model1 <- gam(hw.mpg ~ s(weight),
              data = mpg,
              method = "REML")

plot(model1,
     residuals = TRUE,
     pch = 1,
     shade = TRUE)

#' ### 02. Multiple predictor, smooth term - continuous predictor
model2 <- gam(hw.mpg ~ s(weight) + s(length),
              data = mpg,
              method = "REML")
plot(model2,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1)

#' #### 02.1 Multiple predictor, linear term - continuos predictor
model2a <- gam(hw.mpg ~ s(weight) + length, 
              data = mpg,
              method = "REML")
plot(model2a,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1,
     all.terms = TRUE)

#' #### 02.2 Multiple predictor, smooth term - continuous predictor, large lambda
model2b <- gam(hw.mpg ~ s(weight) + s(length, sp = 1000), 
               data = mpg,
               method = "REML")

plot(model2b,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1)

#' ### 03. Multiple predictor, linear term - categorical predictor
model3 <- gam(hw.mpg ~ s(weight) + fuel, 
              data = mpg,
              method = "REML")

plot(model3,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1,
     all.terms = TRUE)

#' #### 03.1 Multiple predictor, linear term - categorical predictor
model3a <- gam(hw.mpg ~ s(weight, by = fuel),
              data = mpg,
              method = "REML")

plot(model3a,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1)

#' #### 03.2 Multiple predictor, linear term - categorical predictor
model3b <- gam(hw.mpg ~ s(weight, by = fuel) + fuel,
               data = mpg,
               method = "REML")

plot(model3b,
     residuals = TRUE,
     pch = 1,
     shade = TRUE,
     pages = 1,
     all.terms = TRUE)

