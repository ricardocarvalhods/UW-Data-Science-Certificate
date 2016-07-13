############################################################################################################
## Monty Hall Simulation
############################################################################################################

# Won the game or not
sim.won <- function(prize.door, first.door, switching){
  as.numeric(ifelse(first.door == prize.door, !switching, switching))
}

# Simulates randomly n door choices (1/3 probability for each door initially)
sim.choose.door <- function(n){
  door.prob <- runif(n)
  ifelse(door.prob <= 1/3, 1, ifelse(door.prob <= 2/3, 2, 3))
}

# Simulates randomly n choices of switching (TRUE) or not (FALSE)
sim.choose.switching <- function(n){
  switching.prob <- runif(n)
  ifelse(switching.prob <= 1/2, TRUE, FALSE)
}

# Simulates n games and calculates the probability of winning by switching or not switching
sim.game <- function(n=1000){
  prize.door <- sim.choose.door(n) # Define prize door (n times)
  first.door <- sim.choose.door(n) # Chose first door (n times)
  switching <- sim.choose.switching(n) # Define if will switch first door (n times)
  won <- sim.won(prize.door, first.door, switching) # Calculates the outcome of each run
  
  df <- data.frame(won = won, switching = switching) # Put runs in data frame
  
  # Calculate probabilities from the outcomes
  prob.win.switching <- table(df$won, df$switching)[2,2]/sum(table(df$won, df$switching)[,2])
  prob.win.not.switching <- table(df$won, df$switching)[1,2]/sum(table(df$won, df$switching)[,1])
  
  return(c(prob.win.switching, prob.win.not.switching))
}

# Plots distributions of probabilities using data frame with columns prob.win.switching and prob.win.not.switching
plot.probs <- function(df, bins = 50){
  require(ggplot2)
  require(gridExtra)
  
  # Plot for Switching
  bw <- (max(df$prob.win.switching) - min(df$prob.win.switching))/(bins - 1)
  h1 <- ggplot(df, aes(prob.win.switching)) + geom_histogram(binwidth = bw) + 
    ggtitle('Switching') + xlab('Probability of Winning') + ylab('Frequency') + 
    geom_vline(xintercept = mean(df$prob.win.switching), colour='red') + 
    scale_x_continuous(breaks = c(0.20,0.30,0.40,0.50,0.60,0.80, round(mean(df$prob.win.switching),3)), limits=c(0.20, 0.80))
  
  # Plot for NOT Switching
  bw <- (max(df$prob.win.not.switching) - min(df$prob.win.not.switching))/(bins - 1)
  h2 <- ggplot(df, aes(prob.win.not.switching)) + geom_histogram(binwidth = bw) + 
    ggtitle('NOT Switching') + xlab('Probability of Winning') + ylab('Frequency') + 
    geom_vline(xintercept = mean(df$prob.win.not.switching), colour='red') +
    scale_x_continuous(breaks = c(0.20,0.40,0.50,0.60,0.70,0.80, round(mean(df$prob.win.not.switching),3)), limits=c(0.20, 0.80))
  
  grid.arrange(h1, h2, nrow = 2, top="Distributions of Probability of Winning")
}

# Executes, repeated times, n runs of Monty Hall problem, also showing plot and descriptive results
dist.game <- function(reps=300, n=100){
  # Minimum n is 100
  if(n < 100){
    n <- 100
  }
  # Minimum reps is 300
  if(reps < 300){
    reps <- 300
  }
  
  dist <- data.frame(prob.win.switching = rep(0, times = reps),
                     prob.win.not.switching = rep(0, times = reps))
  
  # Repeating, "reps" times, "n" runs of Monty Hall problem 
  for(i in 1:reps){
    dist[i, ] <- sim.game(n)
  }
  
  # Plots distributions
  plot.probs(dist)
  
  # Returns descriptive results
  return(cat("<br/><br/>
             <li>Mean of Probability of Winnning by Switching: <span style='color:red'>", 
             round(mean(dist$prob.win.switching), 3), "</span></li>",
             "<li>Std of Probability of Winnning by Switching: <span style='color:red'>", 
             round(sqrt(var(dist$prob.win.switching)), 3), "</span></li>",
             "<li>Var. of Probability of Winnning by Switching: <span style='color:red'>", 
             round(var(dist$prob.win.switching), 5), "</span></li><br/>",
             
             "<li>Mean of Probability of Winnning by NOT Switching: <span style='color:red'>", 
             round(mean(dist$prob.win.not.switching), 3), "</span></li>",
             "<li>Std of Probability of Winnning by NOT Switching: <span style='color:red'>", 
             round(sqrt(var(dist$prob.win.not.switching)), 3), "</span></li>",
             "<li>Var. of Probability of Winnning by NOT Switching: <span style='color:red'>", 
             round(var(dist$prob.win.not.switching), 5), "</span><br/>", sep=""))
}

############################################################################################################

# Executes 1000 repetitions with 1000 runs of the Monty Hall problem each
# SHOWS PLOTS and OUTPUTS DESCRIPTIVE RESULTS
dist.game(1000, 1000)

# Executes the simulation 20 times with default values of repetitions and runs, to observe the running time
library(microbenchmark)
benchmark <- microbenchmark(
  dist.game(),
  times = 20
)

print(benchmark)

