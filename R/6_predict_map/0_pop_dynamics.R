
rm(list = ls())

# Piper GLM method --------------------------------------------------------

# Proportion of breeding birds in the population (Piper 1993). 
prop_brd <- 0.74 # We will compare this with the population matrix estimate

# Note that this proportion is likely to depend on distance to and size of the colonies.
# Since we are interested in the proportion at the colonies, we could consider a larger prop.


# At large breeding colonies the proportion of BREEDING birds to non-breeding birds
# could be around 0.9 (see GLM expression below).
# This expression was taken from a GLM presented in the thesis by Steven Piper 1993.
# Although it admittedly does not provide a very good fit
alpha <- 0.8879
beta <- 0.0013
nbirds <- c(10, 100, 1000)

prop <- exp(alpha + beta*nbirds) / (1 + exp(alpha + beta*nbirds))


# Use a population matrix -------------------------------------------------

# - We will define the age at first breeding to be 5 years old (Piper 1993)

# - Proportion of adults (at breeding colonies Piper 1993)
prop_ad <- 0.9

# - Proportion of adults that breed (Piper 1993)
prop_ad_brd <- 0.8

# - Prop of breeders (in the first case we should add some birds that breed when subadults)
# (Piper 1993, Borello 2002, Schabo 2017)
prop_brd <- c(prop_ad * prop_ad_brd, 0.5, 0.8)    
          
# - Proportion of eggs that make it to large nestling (Piper 1993, Borello 2002, Borello 2002, Schabo 2017)
mean_ntlg <- c(0.70, 0.57, 0.4, 0.64)

# - From this, we calculate the max fecundity to be prop_bred*nestsucces/2
fec <- 0.7 * 0.6 * 0.5 # fecundity per pair


# survival rates for the different ages
# phi <- c(0.45, 0.6, 0.65, 0.65, 0.75, 0.90)

# Two options
phi_binom <- c(0.46, 0.75)

age <- 1:7
phi_func <- exp(-0.904 + 1.060*age - 0.134*age^2) / (1 + exp(-0.904 + 1.060*age - 0.134*age^2))

A <- rbind(c(0, 0, 0, 0, 0, phi_binom[2]*fec),
           c(phi_binom[1], 0, 0, 0, 0, 0),
           c(0, phi_binom[2], 0, 0, 0, 0),
           c(0, 0, phi_binom[2], 0, 0, 0),
           c(0, 0, 0, phi_binom[2], 0, 0),
           c(0, 0, 0, 0, phi_binom[2], phi_binom[2]))


# Piper in his thesis acknowledges that adult survival should be aroudn 90%
phi <- c(0.46, 0.75, 0.90)
A <- rbind(c(0, 0, 0, 0, 0, phi[3]*fec),
           c(phi[1], 0, 0, 0, 0, 0),
           c(0, phi[2], 0, 0, 0, 0),
           c(0, 0, phi[2], 0, 0, 0),
           c(0, 0, 0, phi[2], 0, 0),
           c(0, 0, 0, 0, phi[3], phi[3]))

# Eigenvalues and eigenvectors of transition matrix provide population rate of change
# and steady-state age proportions, respectively
eigA <- eigen(A)

# Normalize steady-state age proportions (sum to 1)
stt_prop <- round(Re(eigA$vectors[,1] / sum(eigA$vectors[,1])), 2)

A
eigA
stt_prop
sum(stt_prop[1:5])
