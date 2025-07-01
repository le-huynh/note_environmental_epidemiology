#'---
#' title: Dose-response meta-analysis
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
        mixmeta,
        skimr,
        dosresmeta,
        splines
)

#' ## Data
# data #-----------
tibble(alcohol)

skimr::skim(alcohol)

alcohol %>% count(id)

alcohol %>% count(type)

#' ## Reproduce example in `mixmeta::bcg`
# example #------------------------

### REPRODUCE THE RESULTS IN CRIPPA ET AL (2016) AND ORSINI ET AL (2012)

# COMPUTE THE WITHIN-STUDY CORRELATIONS EXCLUDING THE REFERENCE
addS <- lapply(split(alcohol, alcohol$id), function(x)
        # computes the covariance matrix for a set of log relative risks
        covar.logrr(y=logrr, 
                    v=se^2, 
                    cases=cases, 
                    n=peryears, 
                    type=type,
                    data=x))

addS %>% enframe()

addS %>% enframe() %>% pluck("value", 1)
addS %>% enframe() %>% pluck("value", 2)

(sub <- tibble(alcohol) %>% filter(!is.na(se)))

# NOT ACCOUNTING FOR WITHIN-STUDY CORRELATIONS
nocor <- mixmeta(logrr ~ 0 + dose, S=se^2, 
                 random= ~ 0 + dose|id, 
                 data=sub,
                 method="ml")
summary(nocor)

# ACCOUNTING FOR WITHIN-STUDY CORRELATIONS
lin <- mixmeta(logrr ~ 0 + dose, 
               random= ~ 0 + dose|id, 
               data=sub, 
               method="ml",
               control=list(addSlist=addS))
summary(lin)

# ALLOWING NON-LINEARITY IN BOTH FIXED AND RANDOM PARTS
nonlin <- mixmeta(logrr ~ 0 + ns(dose, knots=c(10,25)), 
                  data=sub, 
                  random= ~ 0 + ns(dose, knots=c(10,25))|id, 
                  method="ml",
                  control=list(addSlist=addS))
summary(nonlin)

# SIMPLIFY THE MODEL BY ASSUMING LINEARITY IN THE RANDOM PART
nonlin2 <- update(nonlin, 
                  random= ~ 0 + dose|id)
summary(nonlin2)

# FIXED-EFFECTS MODEL (TRICK: random TO DEFINE THE GROUPING, THEN FIX IT TO 0)
nonlinfix <- mixmeta(logrr ~ 0 + ns(dose, knots=c(10,25)), 
                     random= ~ 1|id,
                     data=sub, 
                     method="ml",
                     bscov="fixed", 
                     control=list(addSlist=addS, Psifix=0))
summary(nonlinfix)

# COMPARE THE MODELS
AIC(nocor, lin, nonlin, nonlin2, nonlinfix)

# PREDICT THE RR FOR 12g/day FROM TWO MODELS
exp(predict(nocor, 
            newdata=data.frame(dose=12), 
            ci=TRUE))
exp(predict(lin, 
            newdata=data.frame(dose=12), 
            ci=TRUE))

# PREDICT (RECREATE SPLINES FOR EASY CODING)
(predlin <- exp(predict(lin, 
                       newdata=data.frame(dose=0:60), 
                       ci=TRUE)))

(prednonlin <- exp(predict(nonlin, 
                          newdata=data.frame(dose=0:60), 
                          ci=TRUE)))

# DISPLAY THE NON-LINEAR EFFECT
(col1 <- do.call(rgb, c(as.list(col2rgb("blue") / 255), list(0.2))))
(col2 <- do.call(rgb, c(as.list(col2rgb("green") / 255), list(0.2))))

plot(0:60, 
     predlin[,1], 
     type="l", 
     ylim=c(0.85,1.9), 
     ylab="RR",
     xlab="Alcohol intake (gr/day)", 
     main="Dose-response")

polygon(c(0:60,60:0), 
        c(predlin[,2], rev(predlin[,3])), 
        col=col1, 
        border=NA)

lines(0:60,
      prednonlin[,1], 
      lty=5)

polygon(c(0:60,60:0), 
        c(prednonlin[,2], rev(prednonlin[,3])), 
        col=col2, 
        border=NA)


