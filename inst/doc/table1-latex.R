## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------
library(kableExtra, quietly=TRUE)
library(flextable, quietly=TRUE)
library(table1, quietly=TRUE)
try(detach('package:printr', unload = TRUE), silent=TRUE) # Make sure printr is not loaded

f <- function(x, n, ...) factor(sample(x, n, replace=TRUE, ...), levels=x)
set.seed(427)

n <- 146
dat <- data.frame(id=1:n)
dat$treat <- f(c("Placebo", "Treated"), n, prob=c(1, 2)) # 2:1 randomization
dat$age   <- sample(18:65, n, replace=TRUE)
dat$sex   <- f(c("Female", "Male"), n, prob=c(.6, .4))  # 60% female
dat$wt    <- round(exp(rnorm(n, log(70), 0.23)), 1)

# Add some missing data
dat$wt[sample.int(n, 5)] <- NA

label(dat$age)   <- "Age"
label(dat$sex)   <- "Sex"
label(dat$wt)    <- "Weight"
label(dat$treat) <- "Treatment Group"

units(dat$age)   <- "years"
units(dat$wt)    <- "kg"

## -----------------------------------------------------------------------------
x <- table1(~ age + sex + wt | treat, data=dat)
as.data.frame(x)

## -----------------------------------------------------------------------------
library(printr, quietly=TRUE)
as.data.frame(x)

## -----------------------------------------------------------------------------
kable(as.data.frame(x), booktabs=TRUE)

## -----------------------------------------------------------------------------
t1kable(x)

## -----------------------------------------------------------------------------
t1flex(x)

## -----------------------------------------------------------------------------
x2 <- table1(~ age + wt | treat*sex, data=dat, overall=FALSE)
t1kable(x2)

## -----------------------------------------------------------------------------
x <- table1(~ age + sex + wt | treat, data=dat,
    caption="Test caption", footnote="Test footnote")
t1kable(x)

