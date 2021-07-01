library(ggplot2)
library(tidyverse)

update_RW <- function(value, gamma = .3, lambda=1) {
  value_compound <- sum(value)                    # value of the compound 
  prediction_error <- lambda - value_compound     # prediction error
  value_change <- gamma * prediction_error # change in strength
  value <- value + value_change                   # update value
  return(value)
}

# BLOCKING
sim_blocking = function(g1 = .09, g2 = .09, n = 100, nB = 30){
  
  # total number of trials across 
  # both phases of the task
  n_trials <- n
  n_B_introduce <- nB
  
  # vectors of zeros
  strength_A <- rep(0,n_trials)
  strength_B <- rep(0,n_trials)
  
  gamma <- c(g1, 0)
  
  for(trial in 2:n_trials) {
    
    # after trial 15, both stimuli are present
    if(trial > n_B_introduce) gamma <- c(g1, g2)
    
    # vector of current associative strengths
    v_old <- c(strength_A[trial-1], strength_B[trial-1])
    
    # vector of new associative strengths
    v_new <- update_RW(value = v_old, gamma = gamma)
    
    # record the new strengths
    strength_A[trial] <- v_new[1]
    strength_B[trial] <- v_new[2]
  }
  
  res <- list(sA = strength_A, sB = strength_B)
  return(res)
}

#####################################################################

plot_rwsim <- function(res){

  nsim <- length(res$sA)
  df = data.frame(t = rep(seq(1,nsim),2), 
                  stimulus = c(rep("A", nsim), rep("B", nsim)), 
                  strength = c(res$sA, res$sB))

  
  ggplot(df, aes(x=t, y=strength, group=stimulus)) + 
    geom_line(aes(linetype=stimulus, colour=stimulus)) + 
    geom_point(aes(colour=stimulus)) + 
    theme_classic() + theme(#axis.ticks.x=element_blank(),
                            #axis.ticks.y=element_blank(),
                            axis.text.x=element_text(size=14),
                            axis.text.y=element_text(size=14),
                            axis.title.y=element_text(size=20),
                            axis.line.y=element_line(size=0.2),
                            axis.title.x=element_text(size=20),
                            legend.position="top",
                            legend.text = element_text(size=10, 
                                                       face="bold")) + 
    xlab("Trial") + ylab("Strength")
}  