# setwd('...')		# specify folder containing function file if necessary
source('WinningStreakFunctions.R')

true.talent.AVG <- .302
G <- 162
streak.length <- 56
k <- streak.length
n.sims <- 100000

# Randomly Sample Games from Trout's AB Distribution
ABs <- sample(1:8,G*n.sims,replace=T,prob=c(13,87,318,474,141,16,1,2))
# arbitrary AB distribution to set ABs per game to ~4.00
# ABs <- sample(2:8,G*n.sims,replace=T,prob=c(60,198,474,241,36,1,2))

# Probability of Hitting Safely in a Game
p <- 1 - (1-true.talent.AVG)^ABs
p.hit <- mean(p)

#SIM
# sim 1M seasons by repeating 100k sim ten times to limit size of tables to store in memory at one time
p.streak <- 0
tot.streaks <- 0
for(i in 1:10) {
	sim <- matrix(runif(G * n.sims) < p,nrow=n.sims)
	successful.streaks <- apply(sim,1,find.streaks,k)
	p.streak <- p.streak + sum(successful.streaks>0)
	tot.streaks <- tot.streaks + sum(successful.streaks)

}
tot.streaks.sim <- tot.streaks/(i*n.sims)
p.streak.sim <- p.streak/(i*n.sims)

# Formulas
tot.streaks.formula <- p.hit^k * ((G-k) * (1-p.hit) + 1)
p.streak.formula <- 1 - (1 - p.hit^k) ^ ((G-k) * (1-p.hit) + 1)
p.streak.constantp.exact <- prob.streak(G,k,p.hit)

p.hit

p.streak.sim
p.streak.formula

tot.streaks.sim
tot.streaks.formula