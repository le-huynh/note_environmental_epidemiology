#'---
#' title: GAM using R
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mgcv,           # GAM modeling
        psych,
        skimr,
        ggeffects,
        gratia
)

#' ## Data
# data #------------------------
#' - **Overall** Science Score (average score for 15 year olds)  
#' - **Interest** in science  
#' - Identifying scientific **Issues**  
#' - **Explaining** phenomena scientifically  
#' - **Support** for scientific inquiry  
#' - **Income** Index  
#' - **Health** Index  
#' - **Edu**cation Index  
#' - **H**uman **D**evelopment **I**ndex (composed of the Income index, Health Index, 
#' and Education Index)  

(pisa <- rio::import("https://raw.githubusercontent.com/m-clark/generalized-additive-models/master/data/pisasci2006.csv") %>% 
        tibble())

#' ## EDA
# eda #---------------------
# skim data
skimr::skim(pisa)

pisa %>% 
        select(Overall, Interest, Support, Income, Health, Edu, HDI) %>% 
        psych::pairs.panels(ellipses = FALSE, scale = TRUE)

pisa %>%
        select(Overall, Interest, Support, Income, Health, Edu, HDI) %>%
        pivot_longer(cols = -Overall,
                     names_to = "var_name",
                     values_to = "value") %>% 
        ggplot(aes(x = value,
                   y = Overall)) + 
        geom_point(color = '#D55E00',
                   alpha = .5) +
        geom_smooth(#method = "gam",
                    color = '#56B4E9',
                    se = FALSE) +
        facet_wrap(~var_name,
                   ncol = 3,
                   scales = 'free_x') +
        labs(x = NULL) +
        theme_bw() +
        theme(panel.grid = element_blank())

#' ## GAM modeling
# gam #------------------
#' ### Single predictor
## single predictor #------------------
### linear model #------------
mod_lm = gam(Overall ~ Income, data = pisa)
summary(mod_lm)

### gam model #-------------
mod_gam1 <- gam(Overall ~ s(Income, bs = "cr"), data = pisa)
summary(mod_gam1)

### visualize #---------------------
plot(mod_gam1)

### model comparison #-----------------
AIC(mod_lm)
AIC(mod_gam1)

summary(mod_lm)$sp.criterion
summary(mod_gam1)$sp.criterion

summary(mod_lm)$r.sq
summary(mod_gam1)$r.sq

# use with caution!
anova(mod_lm, mod_gam1, test = "Chisq")


#' ### Multiple predictors
## multiple predictors #------------------
### linear model #------------------
mod_lm2 = gam(Overall ~ Income + Edu + Health, data = pisa)
summary(mod_lm2)

### GAM model #-----------------
mod_gam2 = gam(Overall ~ s(Income) + s(Edu) + s(Health), data = pisa)
summary(mod_gam2)

mod_gam2B = update(mod_gam2, . ~ . - s(Health) + Health)
summary(mod_gam2B)

### visualize #----------------
plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)

gratia::draw(mod_gam2)

# 2d smooths
vis.gam(mod_gam2, type = 'response', plot.type = 'contour')

#' - High Income → highest Overall science scores.  
#' - Education has less effect.  
#' - Low Income + low Educatio → poor Overall science scores.  

### model comparison #----------------
anova(mod_lm2, mod_gam2, test = "Chisq")

#' ## Model diagnostics
# diagnostics #-----------------------
# gam.check
par(mfrow = c(2, 2))
gam.check(mod_gam2, k.rep = 1000)
par(mfrow = c(1, 1))

# concurvity: 
# 0 = no problem; 
# 1 = a smooth term lies entirely in space of >=1 other smooth terms
concurvity(mod_gam2)

# slope changes
fd_inc = gratia::derivatives(mod_gam1, n = 500)

fd_inc = fd_inc %>%
        mutate(Income = seq(
                min(mod_gam1$model$Income),
                max(mod_gam1$model$Income),
                length.out = 500
        )) %>%
        rename(Estimate = .derivative)

fig <- fd_inc %>% 
        ggplot(aes(x = Income, y = Estimate)) +
        geom_hline(yintercept = 0) +
        geom_line(size = 1,
                  color = '#56B4E9') +
        geom_point(
                size = 3,
                alpha = 1,
                data = fd_inc %>% 
                        filter(Estimate == max(Estimate) | 
                                       Estimate == min(Estimate)),
                color = "red") +
        theme_bw() +
        theme(panel.grid = element_blank())

fig




