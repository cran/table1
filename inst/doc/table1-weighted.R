## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------
library(table1, quietly=TRUE)
try(detach('package:printr', unload = TRUE), silent=TRUE) # Make sure printr is not loaded

## -----------------------------------------------------------------------------
library(survey, quietly=TRUE)
data(myco)

myco$Leprosy <- factor(myco$leprosy, levels=1:0, labels=c("Leprosy Cases", "Controls"))

myco$AgeCat <- factor(myco$Age,
    levels=c(7.5,      12.5,       17.5,       22.5,       27.5,       32.5      ),
    labels=c("5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34")
)

myco$ScarL <- as.logical(myco$Scar)

label(myco$Age) <- "Age"
units(myco$Age) <- "years"
label(myco$AgeCat) <- "Age Group"
label(myco$ScarL) <- "BCG vaccination scar"

table1(~ ScarL + Age + AgeCat | Leprosy, data=weighted(myco, wt), big.mark=",")

## -----------------------------------------------------------------------------
table1(~ Age + ScarL | Leprosy, data=weighted(myco, wt), transpose=T, big.mark=",")

## -----------------------------------------------------------------------------
table1(~ weighted(ScarL, wt) + Age + AgeCat | Leprosy, data=myco, big.mark=",")

## -----------------------------------------------------------------------------
data(api)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

svyby(~api99+api00, ~stype, dclus1, svymean)

svytable(~sch.wide+stype, dclus1)

## -----------------------------------------------------------------------------
myrender <- function(x, name, ...) {
    if (is.numeric(x)) {
        r <- svymean(as.formula(paste0("~", name)), subset(dclus1, (1:nrow(dclus1)) %in% indices(x)))
        r <- c(Mean=as.numeric(r), SE=sqrt(attr(r, "var", exact=T)))
        r <- unlist(stats.apply.rounding(as.list(r), big.mark=","))
    } else {
        r <- svytable(as.formula(paste0("~", name)), subset(dclus1, (1:nrow(dclus1)) %in% indices(x)))
        r <- unlist(stats.apply.rounding(as.list(r), big.mark=",", digits=1, rounding.fn=round_pad))
    }
    c("", r)
}

apiclus1$stype2 <- factor(apiclus1$stype, levels=c("E", "M", "H"),
    labels=c("Elementary", "Middle School", "High School"))

label(apiclus1$api99)    <- "API in 1999"
label(apiclus1$api00)    <- "API in 2000"
label(apiclus1$sch.wide) <- "Met school-wide growth target?"

table1(~ api99 + api00 + sch.wide | stype2, indexed(apiclus1), render=myrender,
    render.strat=names)

