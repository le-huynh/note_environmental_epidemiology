#'---
#' title: Standard meta-analysis
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
        skimr
)

#' ## Data
# data #-----------
tibble(bcg)

skimr::skim(bcg)

#' ## Reproduce example in `mixmeta::bcg`
# example #------------------------

### REPRODUCE THE RESULTS IN VAN HOUWELINGEN ET AL (2002)

# FIXED-EFFECTS META-ANALYSIS (SECTION 3.1.1)
unifix <- mixmeta(logor, logorvar, data=bcg, method="fixed")

print(summary(unifix), digits=3)

# RANDOM-EFFECTS META-ANALYSIS WITH MAXIMUM LIKELIHOOD (SECTION 3.1.2)
uniran <- mixmeta(logor, logorvar, data=bcg, method="ml")

print(summary(uniran), digits=3, report="var")

# ORIGINAL ESTIMATES AND BEST-LINEAR UNBIASED PREDICTIONS (FIGURE 3)
pred <- with(bcg, cbind(logor, logor-1.96*sqrt(logorvar),
                        logor+1.96*sqrt(logorvar)))

# Best Linear Unbiased Predictions
blup <- blup(uniran, pi=TRUE)

plot(pred[,1], rev(bcg$trial)+0.2, xlim=c(-3,3), ylim=c(0,14), pch=18,
     axes=FALSE, xlab="Log odds ratio", ylab="Trial", main="Forest plot")
axis(1)
axis(2, at=bcg$trial, labels=rev(bcg$trial), lty=0, las=1)
abline(v=coef(uniran))
segments(pred[,2], rev(bcg$trial)+0.2, pred[,3], rev(bcg$trial)+0.2, lty=5)
points(blup[,1], rev(bcg$trial)-0.2, pch=19)
segments(blup[,2], rev(bcg$trial)-0.2, blup[,3], rev(bcg$trial)-0.2)

# COMPUTE THE OUTCOME SEPARATELY FOR TREATMENT AND CONTROL GROUPS
y <- with(bcg, log(cbind(tpos/tneg, cpos/cneg)))
S <- with(bcg, cbind(1/tpos+1/tneg, 1/cpos+1/cneg))

# BIVARIATE RANDOM-EFFECTS META-ANALYSIS (SECTION 4)
mvran <- mixmeta(y, S, method="ml")
print(summary(mvran), digits=3, report="var")

# META-REGRESSION (SECTION 5)
uniranlat <- update(uniran, .~. + ablat)
print(summary(uniranlat), digits=3, report="var")
drop1(uniranlat, test="Chisq")


