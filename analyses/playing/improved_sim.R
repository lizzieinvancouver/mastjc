

grid <- 1:30

# predator parameters
gamma_sat <- 3000 # max beech seeds one mouse eats per year (~1 kg food / 0.2 g per nut)
sat_half <- 500 # seeds needed (per predator) to reach half_satiation 
R <- 1/600 # conversion efficiency (offspring that survive/seed)
S <- 0.25 # maximum survival (when satiated)
starv_half <- 800 # seeds needed (per predator) to reach half-survival

# masting parameters
n_trees <- 100 # 100 mature trees per hectare?
# quadrat count under the canopy: 12–886 seeds/m2, 100 trees in a closed canopy forest of 1ha mean 100m2 canopy per tree
# let's say count is 10 seeds per m2 in non-masting and 500/m2 in masting, both *100
seeds_per_tree  <- c(1000, 50000) # low and high state seed prod. 
theta <- 0.35 # zero-inflation parameter for non-masting year: some trees just produce 0 seeds

# synchronous stand-level masting means between 70 an 90% of trees are masting
min_sync <- 0.7 
max_sync <- 0.9

prop_empty <- c(0.5, 0.1) # prop. of empty seeds

# state dynamics
state0 <- 1 # initial state
p_12 <- 0.25
p_22 <- 0.01
P <- matrix(c(1 - p_12, p_12,
              1 - p_22, p_22), nrow = 2, byrow = TRUE)


states <- c(state0)
for (t in grid){
  states <- c(states, sample(1:2, 1, prob = P[states[t],]))
}
states <- states[-1] # remvoe initial state




## ---- predator dynamics ----

vpred0 <- sample(10:50, 1) # initial predator population

vpreds <- c(vpred0) 
vfl_sds <- c()
vetb_sds <- c()
vprd_sds <- c()

for (t in grid) {
  
  sync <- runif(1, min_sync, max_sync) # synchronicity
  n_masting <- ifelse(states[t] == 2, rbinom(1, n_trees, sync), 0) # number of masting trees
  fl_sd_masting <- sum(rpois(n_masting, seeds_per_tree[2])) # fallen seeds by masting trees
  
  n_nonmasting <- n_trees - n_masting
  fl_sd_nonmasting <- sum(sapply(1:n_nonmasting, function(i){ifelse(rbinom(1, 1, 1 - theta), MASS::rnegbin(1,seeds_per_tree[1],100), 0)}))
  
  fl_sd  <- fl_sd_masting + fl_sd_nonmasting # total fallen seeds
  
  # some of these seeds are empty? And thus don't count in the 'eatable seeds'
  etb_sd <- fl_sd * (1 - prop_empty[states[t]])
  
  if (vpreds[t] <= 0) {
    
    prd_sd <- 0
    
  }else{
    
    flsd_perpred <- etb_sd/vpreds[t] # fallen seeds per predator   
    # fd_sd <- vpreds[t] * gamma_sat * flsd_perpred / (flsd_perpred + sat_half) # 'found' seeds: the satiation functional form! type II
    fd_sd <- vpreds[t] * gamma_sat * flsd_perpred^2/(flsd_perpred^2 + sat_half^2) # type III
    
    
    # fd_sd  <- fl_sd * (1 - exp(-a * vpreds[t])) # 
    prd_sd <- min(etb_sd, fd_sd) # predated seeds (max: fallen eatable seeds)
    
  }
  # if(t == 10){stop()}
  
  prdsd_perpred <- ifelse((vpreds[t] > 0), prd_sd / vpreds[t], 0) # predated seeds per predator
  
  S_eff  <- S * prdsd_perpred / (prdsd_perpred + starv_half) # starvation: low food leads to low survival
  newpred <- prd_sd * R  # recruitment, depends on seeds eaten (but should depend on seed eaten after some amount of maintenance)
  
  # store
  vfl_sds  <- c(vfl_sds, fl_sd)
  vetb_sds  <- c(vetb_sds, etb_sd)
  vprd_sds <- c(vprd_sds, prd_sd)
  
  # next year! 
  # note: some of the new pred. shoud leave the patch?
  vpreds <- c(vpreds, floor(S_eff*vpreds[t] + newpred)) 

}
vpreds <- head(vpreds, -1) # remove last one
vesc_sds <- (1 - vprd_sds / vetb_sds) * 100 # % escaped seeds

plot(grid, vetb_sds/vpreds, type = 'l', bty = 'n', ylim = c(0, max(vetb_sds/vpreds)),
     ylab = '# seeds (per pred)', xlab = '')



par(mfrow = c(3, 1), mar = c(2, 4, 1, 1))
plot(grid, vfl_sds, type = 'l', bty = 'n', ylim = c(0, max(vfl_sds)),
     ylab = '# seeds (per hectare)', xlab = '')
abline(v = which(states == 2), col = 'darkred', lty = 2)
plot(grid, vpreds, type = 'l', bty = 'n', ylim = c(0, max(vpreds)),
     ylab = '# predators (per hectare)', xlab = '')
abline(v = which(states == 2), col = 'darkred', lty = 2)
plot(grid, vesc_sds, type = 'l', bty = 'n', ylim = c(0, 100),
     ylab = '% seeds that escaped', xlab = '')
abline(v = which(states == 2), col = 'darkred', lty = 2)

# par(mfrow = c(1, 1), mar = c(2, 4, 1, 1))
# plot(grid, vpreds/max(vpreds), type = 'l', bty = 'n', ylim = c(0, 1),
#      ylab = '', xlab = '')
# lines(grid, vfl_sds/max(vfl_sds))


# Some notes, on wood mouse, bank vole...
# body mass: around 20g... A beech seed is around 0.2g 
# satiation: how much a 20g mouse can eat? Let's say 3000 seeds within the season?
# if the season is 150 days, it means 3000*0.2/150=4g per day?
# survival: less than 2 years! So few survive more than one year
# reproduction: up to 6 litters per female! And 4-8 offsprings per litter... so big maximum: 48 offsprings
# but many of the offsprings seem to die (during winter etc.), so:
# 48 * sexratio (0.5) * 20% survival ~ max. 5 per predator

#
# seq <- seq(1,700)
# y <- seq / (seq + 1500)
# plot(x = seq, y = y)



vetb_sds[9:11]
vpreds[9:11]
vprd_sds[9:11]


x <- seq(0, 5000)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
y <-gamma_sat * x^2/(x^2 + sat_half^2)
plot(x, y, type = 'l')


y <- x / (x + 100)
plot(x, y, type = 'l')


