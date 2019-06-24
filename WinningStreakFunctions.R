
find.streaks <- function(sim,streak.length) {
	streaks <- rle(sim)
	win.streaks <- streaks$lengths[streaks$values==1]
	successful.streaks <- sum(win.streaks >= streak.length)
	return(successful.streaks)		
}
	
	

# exact probability of a streak of k successes appearing in n trials with a constant probability p
# https://www.askamathematician.com/2010/07/q-whats-the-chance-of-getting-a-run-of-k-successes-in-n-bernoulli-trials-why-use-approximations-when-the-exact-answer-is-known/

prob.streak <- function(n,k,p){
	
	q <- 1-p
	
	total.A <- 0
	for (i in 0:n) {	
		N <- n-(i+1)*k
		if(N<i){
			A <- 0
		} else {
			#A <- factorial(N) / (factorial(N-T) * factorial(T)) * (-1*q*p^k)^T
			A <- choose(N,i) * (-1*q*p^k)^i
#			print(A)
		}
		total.A <- total.A + A
	}	
		
	total.B <- 0
	for (i in 1:n) {
		N <- n-i*k
		if(N<i){
			B <- 0
		} else {
			#B <- factorial(N) / (factorial(N-T) * factorial(T)) * (-1*q*p^k)^T
			B <- choose(N,i) * (-1*q*p^k)^i	
#			print(B)
		}	
		total.B <- total.B + B
	}	
	
	p.streak <- p^k * total.A - total.B
	p.streak <- min(p.streak,1)
	if(k==0){p.streak <- 1}
	return(p.streak)
}


