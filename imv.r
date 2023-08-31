
imvfun <- function(model1, model2, vary, data, 
    varx=NA, seed=1234, nfold=5){

    NY = length(vary)
    
    if(nfold > 1){ # cross-validation
        set.seed(seed)
        data$group <- sample(1:nfold, nrow(data), replace=TRUE)
        data_lst <- split(data, data$group)

        imv = NULL
        for (foldi in 1:nfold){
            test<-data_lst[[foldi]][,1:NY]
            train<-data.frame(do.call("rbind",data_lst[-foldi]))[,1:NY]
            imv_foldi <- foldfun(train, test, model1, model2, vary)
            imv = rbind(imv, c(imv_foldi, foldi))
        }
    }else{ # insample 
        imv = NULL
        yprob1 <- predict_insample_fun(data, model1, vary)
        yprob2 <- predict_insample_fun(data, model2, vary)
        for (varyi in vary){
            imv.temp <- imv.binary(as.vector(data[,varyi]), yprob1[,varyi], yprob2[,varyi]) 
            imv <- c(imv, imv.temp)
        }
        imv.allpar <- mean(imv)
            #imv.binary(c(as.matrix(data)), 
            #c(as.matrix(yprob1)),
            #c(as.matrix(yprob2))) 
        imv <- c(imv, imv.allpar)
        names(imv) = c(vary, 'all')
        return(imv)
    }


    return(imv)
}


imv_base_fun <- function(model, vary, data, 
    varx=NA, seed=1234, nfold=5){
    
    NY = length(vary)

    # data preparation
    set.seed(seed)
    data$group <- sample(1:nfold, nrow(data), replace=TRUE)
    data_lst <- split(data, data$group)
    imv_rep = NULL
    for (foldi in 1:nfold){
        test<-data_lst[[foldi]][,1:NY]
        train<-data.frame(do.call("rbind",data_lst[-foldi]))[,1:NY]
        imv_foldi = NULL
        yprob <- predict_fun(train, test, model, vary)
        if(sum(is.na(yprob))==0){ 
        # exclude error condition
            for (varyi in vary){
                imv.temp <- imv.binary(as.vector(test[,varyi]), 
                    rep(mean(test[,varyi]), length(test[,varyi])), 
                    yprob[,varyi]) 
                imv_foldi <- c(imv_foldi, imv.temp)
            }
            imv.allpar <- mean(imv_foldi)
            imv_foldi <- c(imv_foldi, imv.allpar)
        }else{
            imv_foldi = rep(NA, length(vary)+1)
        }

        names(imv_foldi) = c(vary, 'all')

        imv_rep = rbind(imv_rep, c(imv_foldi, foldi))
    }

    return(imv_rep)
}


