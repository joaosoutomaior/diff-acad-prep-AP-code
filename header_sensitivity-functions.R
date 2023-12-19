#==============================================================
# File description
#==============================================================
# contents: 
#  functions for sensitivity analysis
# code author: Joao Souto-Maior and Ravi Shroff
# last updated: Dec 5, 2023  


#==============================================================
# Modified Rosenbaum and Rubin approach
#==============================================================
# original code from "library.R" (derived from J. Jung's rnr package)

# define helper functions
inv_logit <- stats::binomial()$linkinv
logit <- stats::binomial()$linkfun

# part of sensitivity analysis; this implements the closed-form solution for gamma and delta
# from the original rosenbaum/rubin paper
solve_closed <- function(prob, delta, lhs) {
  # Closed-form solution for theta, given
  # 1 - lhs = prob * inv_logit(theta) + (1 - prob) * inv_logit(theta + delta)
  # as presented in Rosenbaum & Rubin
  a <- lhs * exp(delta)
  b <- (lhs - prob) * exp(delta) + lhs - 1 + prob
  c <- lhs - 1
  w <- (-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
  theta <- log(w)
  return(theta)
}
solve <- Vectorize(solve_closed)

# Main sensitivity function; takes as input 
# obj (testing data)
# q = p(u = 1 | x) (prevalence of confounder)
# da = change in log-odds of treat = 1 if u = 1 (effect on enrollment)
# dr = change in log-odds of response = 1 if treat = 1 and u = 1 (effect on exam passage)
# Function assumes that obj has:
# a column called risk (i.e., Pr(r(1,1) = 1 | c, x))
# a column called prop_score (i.e., Pr(a = 1 | c, x))
# the two columns above are obtained by applying a model estimated on training data
# to the observations in obj
sensitize <- function(obj, q, da, dr) {
  risk <- obj$risk
  
  gamma = solve(prob = 1 - q,
                delta = da,
                lhs = 1 - obj$prop_score)
  
  # compute p(a=1|c,x,u)
  p_a_u1 <- inv_logit(gamma + da)
  p_a_u0 <- inv_logit(gamma)
  
  # compute p(u=1|a=1,c,x)
  pu1_a1 <- p_a_u1*q/((p_a_u0*(1-q)) + (p_a_u1*q))
  
  beta = solve(prob = 1 - pu1_a1,
               delta = dr,
               lhs = 1 - risk)
  
  # compute p(r(1,1) = 1|c, x, u)
  p_r_u1 <- inv_logit(beta + dr)
  p_r_u0 <- inv_logit(beta)
  
  obj$adjusted_risk_u1 <- p_r_u1
  obj$adjusted_risk_u0 <- p_r_u0
  obj$adjusted_prop_score_u1 <- p_a_u1
  obj$adjusted_prop_score_u0 <- p_a_u0
  obj$gamma <- gamma
  obj$beta <- beta
  
  return(obj)
}

#==============================================================
# Apply sens functions
#==============================================================
apply_sens_fr <- function(sensitivity_data,
                       q_group_reference,
                       q_group_interest,
                       race_group_reference,
                       race_group_interest,
                       da,
                       dr,
                       N,
                       control = "none",
                       mlm = "none"){
  
  # adjust code to make sure that Pr(u = 1 | c,x) differs across races.
  sensitivity_data_group_reference <- sensitivity_data %>%
    filter(race == race_group_reference)
  sensitivity_data_group_interest <- sensitivity_data %>%
    filter(race == race_group_interest)
  
  # Apply the entire process twice
  # once for group of interest and another for reference group.
  custom_sens_group_reference <- sensitize(sensitivity_data_group_reference,
                                           q = q_group_reference,
                                           da = da,
                                           dr = dr)
  
  custom_sens_group_interest <- sensitize(sensitivity_data_group_interest,
                                          q = q_group_interest,
                                          da = da,
                                          dr = dr)
  
  #----------------------- reference group (e.g., white students) -------------
  # Create two copies of the data
  synth_data_group_reference_d0 <- as_tibble(custom_sens_group_reference) %>%
    mutate(data_type = "d0")
  synth_data_group_reference_d1 <- as_tibble(custom_sens_group_reference) %>%
    mutate(data_type = "d1")
  
  # create the new values of enrollment and risk
  synth_data_group_reference_d0 <- synth_data_group_reference_d0 %>% 
    mutate(new_enroll = adjusted_prop_score_u0,
           new_risk = adjusted_risk_u0,
           weight = 1 - q_group_reference)
  
  synth_data_group_reference_d1 <- synth_data_group_reference_d1 %>% 
    mutate(new_enroll = adjusted_prop_score_u1,
           new_risk = adjusted_risk_u1,
           weight = q_group_reference)
  
  #---------------------- group of interest (e.g., Black students) ------------
  # Create two copies of the data
  synth_data_group_interest_d0 <- as_tibble(custom_sens_group_interest) %>%
    mutate(data_type = "d0")
  synth_data_group_interest_d1 <- as_tibble(custom_sens_group_interest) %>%
    mutate(data_type = "d1")
  
  # create the new values of enrollment and risk
  synth_data_group_interest_d0 <- synth_data_group_interest_d0 %>% 
    mutate(new_enroll = adjusted_prop_score_u0,
           new_risk = adjusted_risk_u0,
           weight = 1 - q_group_interest)
  
  synth_data_group_interest_d1 <- synth_data_group_interest_d1 %>% 
    mutate(new_enroll = adjusted_prop_score_u1,
           new_risk = adjusted_risk_u1,
           weight = q_group_interest)
  #----------------------------------------------------------------------------
  
  # create single dataset for all students
  new_data <- rbind(synth_data_group_reference_d0, 
                    synth_data_group_reference_d1, 
                    synth_data_group_interest_d0, 
                    synth_data_group_interest_d1)
  
  f <- paste0("new_enroll ~ race + logit(new_risk) +", 
                      control)
  # issue passing the formula into the glm function
  m <- stats::glm(formula = new_enroll ~ race + logit(new_risk) + ID_school, 
                  data = new_data,
                  family = "quasibinomial",
                  weights = new_data$weight)
  
  x = exp(m$coefficients[[paste0("race",race_group_interest)]])
  x
}

#==============================================================
# Grid search
#==============================================================
grid_search_sensitivity_fr <- function(
                                    data,
                                    q_group_reference_range,
                                    q_group_interest_range,
                                    race_group_reference,
                                    race_group_interest,
                                    da_range,
                                    dr_range,
                                    N,
                                    control = "none",
                                    mlm = "none") {
  
  # define a named list of parameter values to search over
  parameters_to_search <- list(q_group_reference = q_group_reference_range,
                               q_group_interest = q_group_interest_range,
                               da = da_range,
                               dr = dr_range) %>% 
    cross_df() # Convert to data frame grid
  
  # define data to store results  
  grid_search_data <- as.data.frame(matrix(nrow = nrow(parameters_to_search), 
                                           ncol = 6))
  names(grid_search_data)[1] <- "combination_number"
  names(grid_search_data)[2] <- "q_group_reference"
  names(grid_search_data)[3] <- "q_group_interest"
  names(grid_search_data)[4] <- "da"
  names(grid_search_data)[5] <- "dr"
  names(grid_search_data)[6] <- "outcome"
  
  # apply the sens function over all combinations of parameter values
  for(i in 1:nrow(parameters_to_search)){
    sens_results <- apply_sens_fr(
                               data,
                               q_group_reference = parameters_to_search$q_group_reference[[i]],
                               q_group_interest = parameters_to_search$q_group_interest[[i]],
                               race_group_reference = race_group_reference,
                               race_group_interest = race_group_interest,
                               da = parameters_to_search$da[[i]],
                               dr = parameters_to_search$dr[[i]],
                               N = N,
                               control = control,
                               mlm = mlm)
    grid_search_data[i, 1] <- i
    grid_search_data[i, 2] <- parameters_to_search$q_group_reference[[i]]
    grid_search_data[i, 3] <- parameters_to_search$q_group_interest[[i]]
    grid_search_data[i, 4] <- parameters_to_search$da[[i]]
    grid_search_data[i, 5] <- parameters_to_search$dr[[i]]
    grid_search_data[i, 6] <- sens_results 
  }
  
  # organize results
  grid_search_data <- as_tibble(grid_search_data)
  min <- grid_search_data %>%
    arrange(outcome)
  min <- min[1,]
  max <- grid_search_data %>%
    arrange(desc(outcome))
  max <- max[1,]
  
  # report as list
  grid_search_results <- list(grid_search_data, min, max)
  return(grid_search_results)
}
