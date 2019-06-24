# setwd('...')		# specify folder containing function file if necessary
source('WinningStreakFunctions.R')

# parameters
true.talent <- .500
G <- 162
streak.length <- 15
k <- streak.length
n.sims <- 100000

# Starting Rotation
SP <- c(.06,.03,0,-.03,-.06)
p.base <- true.talent - (SP[1]+SP[2])/162
schedule.SP.vector <- rep((p.base+SP),ceiling(G/length(p.base+SP)))[1:G]
schedule.SP <- matrix(rep(schedule.SP.vector,n.sims),nrow=n.sims,byrow=TRUE)

# Homefield Advantage
home <- .04
away <- -.04
schedule.unsorted <- sample(rep(c(home,away),3))
schedule.vector <- rep(replicate(n.sims*9,sample(schedule.unsorted)),each=3)
schedule.HA <- matrix(schedule.vector,nrow=n.sims,byrow=TRUE)

# Schedule Opponents
opp <- rep((-8:9-.5)/(100),each=3)
schedule.opp.vector <- rep(replicate(n.sims,sample(opp)),each=3)
schedule.opp <- matrix(schedule.opp.vector,nrow=n.sims,byrow=TRUE)

# probability of winning each game
schedule.WP <- schedule.SP + schedule.HA + schedule.opp
p <- schedule.WP


# SIM
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

# formulas
tot.streaks.formula <- true.talent^k * ((G-k) * (1-true.talent) + 1)
p.streak.formula <- 1 - (1 - true.talent^k) ^ ((G-k) * (1-true.talent) + 1)
p.streak.constantp.exact <- prob.streak(G,k,true.talent)

p.streak.sim
p.streak.formula

tot.streaks.sim
tot.streaks.formula