# gurobi_test 
# 10/29/2019

##########################################
#
# if gurobi is not installed
# for you on Vinnie's server (dor-rcomp02)
# follow the instructions in
# gurobi-installation-as-of-2019-Oct.R
#
#########################################


#################################
# designmatch package help 
#################################

# https://cran.rstudio.com/web/packages/designmatch/designmatch.pdf

#################################
# Install Packages
#################################

# install.packages("designmatch")
# install.packages("slam")
# install.packages("Rglpk")
# install.packages("gurobi")

#################################
# Set your working directory
#################################

setwd("~/") # change this to the directory you want to use

################################# 
# Load the libraries.
################################# 

library("designmatch")
library("gurobi")

data(lalonde)

attach(lalonde)

################################# 
# Briefly describe the data before matching.
################################# 

table(treatment)

vars = cbind(age, education, black, hispanic, married, nodegree, re74, re75, re78)
meantab(vars, treatment, which(treatment==1), which(treatment==0))

################################# 
# Using all the treated observations, find an optimal pair matching that minimizes the total sum of covariate distances between matched pairs using a rank-based Mahalanobis distance matrix.  Check covariate balance.
################################# 

# Treatment indicator
t_ind = treatment

# Matrix of covariates
X_mat = cbind(age, education, married, nodegree, re74, re75)

# Distance matrix
dist_mat = distmat(t_ind, X_mat)

# Subset matching weight
subset_weight = NULL

# Total pairs to be matched
total_groups = sum(t_ind)

# Solver options
t_max = 60*1 # time in seconds
solver = "gurobi"
approximate = 0 # 0/1 - typically use exact match - use 0/use 1 with glpk
solver = list(name = solver, t_max = t_max, approximate = approximate, round_cplex = 0, trace_cplex = 0)

## -- [Choose the covariate balancing desired for each variable, can be a combination of mean balancing, fine balancing, exact balancing] -- ##

# Moment balance: least restrictive-constrain differences in means to be at most .1 standard deviations (or other amount you specify) apart
mom_covs = cbind(age, education,  nodegree, re74, re75)
mom_tols = round(absstddif(mom_covs, t_ind, .1), 2) # You define the tolerance (the 0.1 is tolerance here)
mom = list(covs = mom_covs, tols = mom_tols)


# Fine balance: more restrictive-contstrain proportions to be the same accross groups
# (Use with discrete variables)
fine_covs = cbind(married,hispanic)
fine = list(covs = fine_covs)


# Exact balance: most restrictive
exact_covs = cbind(married)
exact = list(covs = exact_covs)

## Match (define all desired specifications)                  
out = bmatch(t_ind = t_ind, dist_mat = dist_mat, total_groups = total_groups, mom = mom, fine = fine, exact = exact, solver = solver)              


# Indices of the treated units and matched controls
t_id = out$t_id  
c_id = out$c_id  

# Total number of matched pairs
length(t_id)

# Total of distances between matched pairs
out$obj_total

# Assess mean balance
meantab(X_mat, t_ind, t_id, c_id)

# Assess fine balance (note here we are getting an approximate solution)
for (i in 1:ncol(fine_covs)) {
  print(colnames(fine_covs)[i])
  print(finetab(fine_covs[, i], out$t_id, out$c_id))
}

# Pairsplot
pairsplot(re74, re75, t_id, c_id, "re74", "re75", "pairs in re74 and re75")


