



gamma_sat <- 100 # saturation, seed/predator
E <- 1/50  # conversion efficiency, predator/seed
S <- 0.75 # survival rate
 
grid <- seq(1,100,1)

pred0 <- sample(10:20, 1) # initial number of predators (t = 0)
state0 <- sample(1:2, 1)

# seed production in two states
seedprod <- c(100, 1000)

# transition matrix
p_12 <- 0.15
p_22 <- 0
P <- matrix(c(1 - p_12, p_12, 
              1 - p_22, p_22),
            nrow = 2, byrow = TRUE)

# state sequences
states <- c(state0)
for (t in grid) {
  states <- c(states, sample(1:2, 1, prob = P[states[t], ])) 
}
states <- states[-1]


vpreds <- c(pred0)
vfl_sds <- c()
vprd_sds <- c()
for(t in grid){
  
  fl_sd <- rpois(1, seedprod[states[t]])
  
  sat <- gamma_sat * vpreds[t] # number of seeds eaten
  prd_sd <- min(sat, fl_sd) # predated seeds
  nonprd_sd <- fl_sd-prd_sd # non-predated seeds
  
  # store stuff
  vfl_sds <- c(vfl_sds, fl_sd)
  vprd_sds <- c(vprd_sds, prd_sd)
  
  # t+1
  newpred <- floor(prd_sd * E) # next year predators depend on predated seed? but maybe also on current pred pop.?
  pred <- floor(S*vpreds[t]) + newpred
  vpreds <- c(vpreds,pred)
}

par(mfrow = c(2,1), mar = c(2,4,1,1))
plot(x = grid, y = vpreds[grid], type = 'l',
     bty = 'n', ylim = c(0,50), ylab = '# predators')
abline(v = which(states == 2), col = 'darkred', lty = 2 )

plot(x = grid, y = vprd_sds/vfl_sds*100, type = 'l',
     bty = 'n', ylim = c(0,100), ylab = '% seeds that not escaped')
abline(v = which(states == 2), col = 'darkred', lty = 2 )