
predict_fun <- function(train, test, model, vary){

    fit_train <- cfa(model, data=train, ordered=colnames(train), parameterization="delta")
    ov_pred = lavPredict(fit_train, newdata = test, type = 'ov') 

    y_threshold = inspect(fit_train,what="est")$tau
    y_residual = inspect(fit_train,what="est")$theta


    yprob = NULL
    for (varyi in vary){
        i = which(colnames(train)==varyi)
        ov_pred[,i] = ov_pred[,i] - y_threshold[i]
        temp = pnorm(ov_pred[,i]/sqrt(y_residual[i,i]) )
        yprob = cbind(yprob, temp)
    }
    colnames(yprob) = vary
    return(yprob)

}

predict_insample_fun <- function(data, model, vary){

    fit_train <- cfa(model, data=data, ordered=colnames(data), parameterization="delta")
    ov_pred = lavPredict(fit_train, type = 'ov') 

    y_threshold = inspect(fit_train,what="est")$tau
    y_residual = inspect(fit_train,what="est")$theta


    yprob = NULL
    for (varyi in vary){
        i = which(colnames(data)==varyi)
        ov_pred[,i] = ov_pred[,i] - y_threshold[i]
        temp = pnorm(ov_pred[,i]/sqrt(y_residual[i,i]) )
        yprob = cbind(yprob, temp)
    }
    colnames(yprob) = vary
    return(yprob)

}

