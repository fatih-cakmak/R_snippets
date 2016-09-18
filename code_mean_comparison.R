treatmentVar <-c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1) # treatment is 1 or 0
question1 <-c(1,2,2,3,1,1,2,2,3,1,1,2,2,3,1,3) #choices available are 1, 2, or 3
Questiontab <- table(question1, treatmentVar)
Questiontab

chisq.test(Questiontab)
prop.test(Questiontab)


install.packages('descr')
library(descr)
library(xtable)
?descr
?xtable

## Demonstrate aov
## Taken from help(aov) in R 1.1.1
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
           62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
op <- options(contrasts = c("contr.helmert", "contr.treatment"))
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
options(op)

summary(npk.aov)
print(xtable(npk.aov))
print(xtable(anova(npk.aov)))
print(xtable(summary(npk.aov)))

summary(npk.aovE)
print(xtable(npk.aovE), type = "html")
print(xtable(summary(npk.aovE)), type = "html")


print(xtable(npk.aovE))









heads <- rbinom(1, size = 100, prob = .5)
prop.test(heads, 100)          # continuity correction TRUE by default
prop.test(heads, 100, correct = FALSE)

## Data from Fleiss (1981), p. 139.
## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers.
## A:  The alternative is that this proportion is different in at
##     least one of the populations.

smokers  <- c( 83, 90, 129, 70, 65)
patients <- c( 86, 93, 136, 82, 70)
prop.test(smokers, patients, conf.level = 0.95, correct = TRUE)
?prop.test



prop.test(x=c(128,170), n=c(428,682), alternative="two.sided", conf.level=.99)









a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)

var.test(a,b)

t.test(a,b, var.equal=TRUE, paired=FALSE)
