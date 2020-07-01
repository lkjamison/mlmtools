
library(lme4)
library(lmerTest)

N <- 20 # Number of People
S <- 5 # Number of Scenarios
n <- 3 # Number of Dependent Variables

Persons <- rep(seq(1, N, 1), each = S*n)
Anxiety <- rep(rbinom(N, 1, .5), each = S*n)
Scenarios <- rep(rep(seq(1, S, 1), each = n), times = N)
Order <- c()
for(i in 1:N){
  Order <- c(Order, rep(sample(S, S, replace = FALSE), each = 3))
}
Valence <- rep(seq(1, n, 1), times = N*S)
Minimum <- ceiling(runif(N*S*n, min = -1000, max = 1000))

data <- data.frame("Persons" = Persons,  "Anxiety" = Anxiety, 
                   "Scenarios" = Scenarios, "Order" = Order, 
                   "Valence" = Valence, "Minimum" = Minimum)
str(data)

model <- lmer(Minimum ~ Valence*Anxiety*Order + (1 | Persons) + (1 | Scenarios), data = data)
summary(model)
