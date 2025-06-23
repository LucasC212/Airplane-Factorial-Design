power_factorial_23 <- function(beta_mean, beta_se, replicates, 
                               iter=1000, alpha=0.05){
  
  # Make sure inputs are proper
  if(length(beta_mean) != 8){
    stop("beta_mean must be a vector with 8 values")
  } 
  if(length(beta_se) != 8) {
    stop("beta_se must be a vector with 8 values")
  } 
  
  # Generate data frame with NA outcome values
  sim_data <- data.frame(
    outcome = rep(NA, (8*replicates)),
    x1 = rep(c(0,0,0,0,1,1,1,1), replicates),
    x2 = rep(c(0,0,1,1,0,0,1,1), replicates),
    x3 = rep(c(0,1,0,1,0,1,0,1), replicates)
  )
  
  pvals <- NA
  
  for(j in 1:iter){
    for(i in 1:dim(sim_data)[1]){
      
      # Generate random values of beta based on mean and se
      beta <- rnorm(n=8, mean=beta_mean, sd=beta_se)
      
      # Generate simulated outcome variable values
      sim_data$outcome[i] <- beta[1] +
        beta[2]*sim_data$x1[i] + 
        beta[3]*sim_data$x2[i] +
        beta[4]*sim_data$x3[i] +
        beta[5]*sim_data$x1[i]*sim_data$x2[i] +
        beta[6]*sim_data$x1[i]*sim_data$x3[i] +
        beta[7]*sim_data$x2[i]*sim_data$x3[i] +
        beta[8]*sim_data$x1[i]*sim_data$x2[i]*sim_data$x3[i]
    }
    
    # Run linear model for 2^3 data with all interaction terms
    model1 <- lm(outcome ~ x1+x2+x3+x1*x2+x1*x3+x2*x3+x1*x2*x3, data=sim_data)
    
    # Extract p-value and store it
    f <- summary(model1)$fstatistic
    pvals[j] <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  }
  
  # Calculate estimate of power
  power <- sum(pvals<0.05)/iter
  return(power)
}

