
# IMV in CFA

## Functions for calculating IMV in CFA with binary outcomes
### main function
- imvfun(model1, model2, vary, data, NY, varx=NA, seed=1234, nfold=5)
- imv_prob_fun(): imv for comparing one model with pre-obtained probabilities (e.g., obtained based on outcome prevalence)

### supporting functions:
- foldfun: imv in one iteration in k-fold cv
- predict_fun() or predict_insample_fun(): estimate in the training set and predict p_ij in the test set
- imv.binary: imv calculation based y and p_i
  - source('https://raw.githubusercontent.com/ben-domingue/imv/main/R/imv_binary.R')


## Empirical Example

