#'---
#' title: GAM in R - Noam Ross 02
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
        here,           # locate files 
        tidyverse,      # data management and visualization
        gamair,         # load mpg data
        mgcv
)

#' ## Data
# data #-----------
data("mpg", package = "gamair")

(mpg <- mpg %>% 
                tibble() %>% 
                mutate(fuel = as.factor(fuel)))

#' ## Model
# model #----------------
mod_hwy <- gam(hw.mpg ~ s(weight) + 
                       s(rpm) + 
                       s(price) + 
                       s(comp.ratio) +
                       s(width) + 
                       fuel,
               data = mpg, 
               method = "REML")

summary(mod_hwy)

#' ## Visualize
# visualize #---------------------------------
#' ### All panels
plot(mod_hwy,
     pages = 1,
     all.terms = TRUE)

plot(mod_hwy,
     pages = 1)

plot(mod_hwy,
     select = 2)

#' ### Data points
plot(mod_hwy, residuals = TRUE, pages = 1)

plot(mod_hwy,
     select = 1,
     rug = TRUE,
     residuals = TRUE,
     pch = 3,
     cex = 1)

#' ### Standard Errors
plot(mod_hwy,
     select = 1,
     se = TRUE)

plot(mod_hwy,
     select = 1,
     shade = TRUE)

plot(mod_hwy,
     select = 1,
     shade = TRUE,
     shade.col = "lightblue")

plot(mod_hwy,
     select = 1,
     seWithMean = TRUE)

plot(mod_hwy, 
     select = 1,
     seWithMean = TRUE, 
     shift = coef(mod_hwy)[1])

#' ## Model validation
# validate #-----------------
par(mfrow = c(2, 2))
gam.check(mod_hwy)

concurvity(mod_hwy, full = TRUE)

concurvity(mod_hwy, full = FALSE)

