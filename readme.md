
# IMV in CFA

## 1. Functions for calculating IMV in CFA with binary outcomes
### Main function
- imvfun(model1, model2, vary, data, varx=NA, seed=1234, nfold=5)
  - model1: Baseline model
  - model2: Enhanced model
  - vary: Name of variables
  - data: Data
  - varx: Currently set to NA; reserved for future SEM advancements
  - seed: Seed for random data splitting during cross-validation
  - nfold: Number of data segments in cross-validation. If set to 1, it means prediction uses the entire sample
- imv_base_fun(): IMV for comparing one model with the baseline model that ignores the correlation between items

### Supporting functions:
- foldfun(): IMV in one iteration in k-fold cv
- predict_fun() or predict_insample_fun(): Predict on the test set after estimating on the training set
- imv.binary(): IMV calculation based $y_i$ and $p_i$
  - source('https://raw.githubusercontent.com/ben-domingue/imv/main/R/imv_binary.R')


## 2. Empirical Example in the Paper

Data source: Kim, M., Winkler, C., & Talley, S. (2021). Binary item cfa of behavior problem index (BPI) using mplus: A step-by-step tutorial. *The Quantitative Methods for Psychology, 17*, 141–153

```{r}
library(lavaan)
library(tidyverse)

source('https://raw.githubusercontent.com/ben-domingue/imv/main/R/imv_binary.R')
source('https://raw.githubusercontent.com/zhanglj37/imv_sem/main/imv.r')
source('https://raw.githubusercontent.com/zhanglj37/imv_sem/main/fold.r')
source('https://raw.githubusercontent.com/zhanglj37/imv_sem/main/prediction.r')
```

### Six-factor Model vs Baseline Model

```{r}
data = read.table('BPI2019.dat')
vary = colnames(data)


model1 <- 'AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
         Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
         Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
         Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
         PeerProb =~ NA*pp1 + pp2 + pp3
         Depend =~ NA*de1 + de2 + de3 + de4
         AnxDep ~~ 1*AnxDep
         Headstr ~~ 1*Headstr
         Antisoc ~~ 1*Antisoc
         Hyperac ~~ 1*Hyperac
         PeerProb ~~ 1*PeerProb
         Depend ~~ 1*Depend
'

set.seed(1)
base = imv_base_fun(model1, vary, data)
round(apply(base, 2, mean),3)
```


### Six-factor Model vs Five-factor Model

```{r}
model_6f<<-model1


model_5f<<-'AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
         Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
         Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
         Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
         PeerProb =~ NA*pp1 + pp2 + pp3 + de1 + de2 + de3 + de4
         AnxDep ~~ 1*AnxDep
         Headstr ~~ 1*Headstr
         Antisoc ~~ 1*Antisoc
         Hyperac ~~ 1*Hyperac
         PeerProb ~~ 1*PeerProb
'

# Traditional fitting indicies
fit5f <- cfa(model_5f, data=data, ordered=colnames(data), parameterization="delta")
summary(fit5f, fit.measures=TRUE)
fit6f <- cfa(model1, data=data, ordered=colnames(data), parameterization="delta")
summary(fit6f, fit.measures=TRUE)
anova(fit5f, fit6f)

# Out-of-sample prediction
set.seed(1)
imv56f <- imvfun(model_5f, model_6f, vary, data, nfold=5)
round(apply(imv56f, 2, mean), 3)

# In-sample prediction
set.seed(1)
imv_56f_in <- imvfun(model_5f, model_6f, vary, data, nfold=1)
round(imv_56f_in,3)
```
