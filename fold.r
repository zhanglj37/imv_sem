
foldfun <- function(train, test, model1, model2, vary, varx=NA) {

    imv = NULL
    yprob1 <- predict_fun(train, test, model1, vary)
    yprob2 <- predict_fun(train, test, model2, vary)
    if(sum(is.na(yprob1))==0 & sum(is.na(yprob2))==0){ 
    # exclude error condition
        for (varyi in vary){
            imv.temp <- imv.binary(as.vector(test[,varyi]), 
                yprob1[,varyi], 
                yprob2[,varyi]) 
            imv <- c(imv, imv.temp)
        }
        imv.allpar <- mean(imv)
            #imv.binary(c(as.matrix(test)), 
            #c(as.matrix(yprob1)),
            #c(as.matrix(yprob2))) 
        imv <- c(imv, imv.allpar)
    }else{
        imv = rep(NA, length(vary)+1)
    }

    names(imv) = c(vary, 'all')

    return(imv)
}
