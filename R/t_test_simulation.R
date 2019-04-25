i = 2

for (i in 1:20) {
  
  while(i < 10) 
  {
    i = i + 1; 
    print(i);
  }
}











x = rnorm(30, 45827, 55)
y = rnorm(30, 39892, 64)
n1 = rnorm(30, 3489860, 260)
n2 = rnorm(30, 3576294, 515)


x
y
n1
n2

summary(df1)
df1 = data.frame(1:30)
df1$control = x
df1$treatment = y
df1$control_n = n1
df1$treatment_n = n2
df1 = df1[,-1]


ttest = t.test(x/n1, y/n2, alternative = c("two.sided"),
               paired = FALSE, var.equal = FALSE, conf.level = 0.95)






##########



possible.ns <- seq(from=100, to=2000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=60, sd=20)              # control potential outcome
    tau <- 5                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))






z = -0.512433332
p = 2*pnorm(-abs(z))
p




##########



set.seed(3); 
S <- 1000; 
n <- 15; 
mu0 <- 1; 
mu <- 1.75

?rnorm()

out <- rnorm(S,n,mu)
