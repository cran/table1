## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------
library(table1, quietly=TRUE)

## -----------------------------------------------------------------------------
library(boot) 

melanoma2 <- melanoma
 
# Factor the basic variables that
# we're interested in
melanoma2$status <- 
  factor(melanoma2$status, 
         levels=c(2,1,3),
         labels=c("Alive", # Reference
                  "Melanoma death", 
                  "Non-melanoma death"))

## -----------------------------------------------------------------------------
table1(~ factor(sex) + age + factor(ulcer) + thickness | status, data=melanoma2)

## -----------------------------------------------------------------------------
melanoma2$sex <- 
  factor(melanoma2$sex, levels=c(1,0),
         labels=c("Male", 
                  "Female"))
 
melanoma2$ulcer <- 
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent", 
                  "Present"))

label(melanoma2$sex)       <- "Sex"
label(melanoma2$age)       <- "Age"
label(melanoma2$ulcer)     <- "Ulceration"
label(melanoma2$thickness) <- "Thicknessᵃ"

units(melanoma2$age)       <- "years"
units(melanoma2$thickness) <- "mm"

caption  <- "Basic stats"
footnote <- "ᵃ Also known as Breslow thickness"

table1(~ sex + age + ulcer + thickness | status, data=melanoma2,
    overall=c(left="Total"), caption=caption, footnote=footnote)

## -----------------------------------------------------------------------------
labels <- list(
    variables=list(sex="Sex",
                   age="Age (years)",
                   ulcer="Ulceration",
                   thickness="Thicknessᵃ (mm)"),
    groups=list("", "", "Death"))

# Remove the word "death" from the labels, since it now appears above
levels(melanoma2$status) <- c("Alive", "Melanoma", "Non-melanoma")

## -----------------------------------------------------------------------------
strata <- c(list(Total=melanoma2), split(melanoma2, melanoma2$status))

## -----------------------------------------------------------------------------
my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), c("",
        "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
    c("", sapply(stats.default(x), function(y) with(y,
        sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

## -----------------------------------------------------------------------------
table1(strata, labels, groupspan=c(1, 1, 2), caption=caption, footnote=footnote,
       render.continuous=my.render.cont, render.categorical=my.render.cat)

## -----------------------------------------------------------------------------
f <- function(x, n, ...) factor(sample(x, n, replace=T, ...), levels=x)
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
table1(~ age + sex + wt | treat, data=dat)

## -----------------------------------------------------------------------------
table1(~ age + sex + wt | treat, data=dat, overall=F)

## -----------------------------------------------------------------------------
table1(~ age + wt | treat*sex, data=dat)

## -----------------------------------------------------------------------------
table1(~ age + wt | sex*treat, data=dat)

## -----------------------------------------------------------------------------
table1(~ treat + age + sex + wt, data=dat)

## -----------------------------------------------------------------------------
dat$dose <- (dat$treat != "Placebo")*sample(1:2, n, replace=T)
dat$dose <- factor(dat$dose, labels=c("Placebo", "5 mg", "10 mg"))

strata <- c(split(dat, dat$dose), list("All treated"=subset(dat, treat=="Treated")), list(Overall=dat))

labels <- list(
    variables=list(age=render.varlabel(dat$age),
                   sex=render.varlabel(dat$sex),
                   wt=render.varlabel(dat$wt)),
    groups=list("", "Treated", ""))

table1(strata, labels, groupspan=c(1, 3, 1))

## -----------------------------------------------------------------------------
table1(strata, labels, groupspan=c(1, 3, 1),
       render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]",
                           "Geo. mean (Geo. CV%)"="GMEAN (GCV%)"))

## -----------------------------------------------------------------------------
rndr <- function(x, name, ...) {
    if (!is.numeric(x)) return(render.categorical.default(x))
    what <- switch(name,
        age = "Median [Min, Max]",
        wt  = "Mean (SD)")
    parse.abbrev.render.code(c("", what))(x)
}

table1(~ age + sex + wt | treat, data=dat,
       render=rndr)

## -----------------------------------------------------------------------------
table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-zebra")

## -----------------------------------------------------------------------------
table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid")

## -----------------------------------------------------------------------------
table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid Rtable1-shade Rtable1-times")

## ---- echo=F------------------------------------------------------------------
table1(~ age + sex + wt | treat, data=dat, topclass="custom")

## -----------------------------------------------------------------------------
library(MatchIt) 
data(lalonde)

lalonde$treat    <- factor(lalonde$treat, levels=c(0, 1), labels=c("Control", "Treatment"))
lalonde$married  <- as.logical(lalonde$married == 1)
lalonde$nodegree <- as.logical(lalonde$nodegree == 1)
lalonde$race     <- factor(lalonde$race, levels=c("white", "black", "hispan"),
                                         labels=c("White", "Black", "Hispanic"))

label(lalonde$race)     <- "Race"
label(lalonde$married)  <- "Married"
label(lalonde$nodegree) <- "No high school diploma"
label(lalonde$age)      <- "Age"
label(lalonde$re74)     <- "1974 Income"
label(lalonde$re75)     <- "1975 Income"
label(lalonde$re78)     <- "1978 Income"
units(lalonde$age)      <- "years"

## -----------------------------------------------------------------------------
pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
    } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


## -----------------------------------------------------------------------------
table1(~ age + race + married + nodegree + re74 + re75 + re78 | treat,
    data=lalonde, overall=F, extra.col=list(`P-value`=pvalue))

## -----------------------------------------------------------------------------
dat <- expand.grid(i=1:50, group=LETTERS[1:3])
dat <- cbind(dat, matrix(round(exp(rnorm(6*nrow(dat))), 1), nrow=nrow(dat)))
names(dat)[3:8] <- paste0("V", 1:6)

## -----------------------------------------------------------------------------
table1(~ V1 + V2 + V3 + V4 + V5 + V6 | group, data=dat,
       topclass="Rtable1-grid Rtable1-center",
       render="Mean (CV%)<br/>Median [Min, Max]<br/>GMean (GCV%)")

## -----------------------------------------------------------------------------
table1(~ V1 + V2 + V3 + V4 + V5 + V6 | group, data=dat,
       topclass="Rtable1-grid Rtable1-center",
       render="Mean (CV%)<br/>Median [Min, Max]<br/>GMean (GCV%)",
       transpose=TRUE)

