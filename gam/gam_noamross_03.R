#'---
#' title: GAM in R - Noam Ross 03
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

#' ## Model: factor-smooths interaction
# factor-smooth #----------------
data("mpg", package = "gamair")

(mpg <- mpg %>% 
                tibble() %>% 
                mutate(fuel = as.factor(fuel)))

mod <- gam(hw.mpg ~ s(weight, fuel, bs = "fs"),
           data = mpg,
           method = "REML")

summary(mod)

plot(mod)

# vis.gam(mod, theta = 125, plot.type = "persp")

#' ## Model: tensor smooths
# tensor #----------------
data(meuse, package="sp")

# Fit the model
tensor_mod <- gam(cadmium ~ te(x, y, elev), 
                  data = meuse, method = "REML")

# Summarize and plot
summary(tensor_mod)
plot(tensor_mod)

#' ## Model: tensor interaction
# tensor interaction #----------------
tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev), 
                   data = meuse, method = "REML")

summary(tensor_mod2)
plot(tensor_mod2, pages = 1)

