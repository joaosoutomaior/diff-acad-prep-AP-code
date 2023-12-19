#==============================================================
#                  File description
#==============================================================
# contents: 
#  parameters used in model construction
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

# Fraction of data to use as testing sample
train_fraction = 0.10
cutoff_value = 1000 * train_fraction

# seed number to use across the code
seed = 123

# N samples with replacement for the bootstrapping of std. errors
N_bootstrap = 100

# parameters to search over in the sensitivity analysis
N_sensitivity = 1
q_group_reference_range = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
q_group_interest_range = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
da_range =  c(-log(3), -log(2), 0, log(2), log(3))
dr_range =  c(-log(3), -log(2), 0, log(2), log(3))

# confidence level for plots
c_level_plots = 0.95



