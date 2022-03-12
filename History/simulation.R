

sim_fun_A = function() {
  n <- 1000
  U <- rnorm(n, 1) # unmeasured distress 
  A <- rnorm(n, 1) # religious service attendance,
  M <- rnorm(n , A + U)# religious identification
  Y <- rnorm(n , - A *.2 + U + M *.1) #  distress reduced by attendance.
  
  # Religious id, caused by attendance + distress
  
  # simulate data
  simdat_A <- data.frame(
    M = M, # rel identification
    A = A, # rel service
    U = U, # distressing events (unmeasured)
    Y = Y )# outcome
  
  sim_A <- lm(Y ~ A + M, data = simdat_A)
  sim_A
}

# Replicate including the mediator
r_lm_A <- NA
r_lm_A = replicate(100, sim_fun_A(), simplify = FALSE )


# mediator attenuates true effect ~ .1 instead of ~.1
parameters::pool_parameters(r_lm_A)

## Repeat but with no mediator in model
set.seed(123)
sim_fun_B = function() {
  n <- 1000
  U <- rnorm(n, 1) # unmeasured distress 
  A <- rnorm(n, 1) # religious service attendance,
  M <- rnorm(n , A + U)# religious identification
  Y <- rnorm(n , - A *.2 + U + M *.1) #  distress reduced by attendance.
  
  # Simulate data
  simdat_B <- data.frame(
    M = L, # rel identification
    A = A, # rel service attendance
    U = U, # distressing events (unmeasured)
    Y = Y ) # outcome
  
  # Only model exposure
  sim_B <- lm(Y ~ A, data = simdat_A)
  sim_B
}

# Replication 100xs
r_lm_B <- NA
r_lm_B = replicate(100, sim_fun_B(), simplify = FALSE )

# recover true effect ~ .2
parameters::pool_parameters(r_lm_B)