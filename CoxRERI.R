

RERI.svycox <- function (model, param = c("product", "dummy"), coef=NULL,
                         conf.level = 0.95, model.df=100000, lower.tail=F)
{
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  if (class(model)[1] != "coxph" & class(model)[1] != "svycoxph" ) 
    stop("Error: model must be a coxph or svycoxph object!")
  
  model.sum = summary(model)
  model.coef <- model.sum$coefficients
  if (class(model)[1] == "svycoxph" | class(model)[1] == "coxph") {
    # auto get locations and names of exposure A, B and A:B ; def to coef;
    if (param == "product") {
      inc.name = rownames(model.coef)[grepl('.+[:].+', rownames(model.coef))]
      nameAB = strsplit(inc.name, ':')[[1]]
      nameABC = c(nameAB, inc.name )
      coef = sapply(nameABC, function(row){return(which(rownames(model.coef)==row))})
      print(coef)
      
      colname.coef <- 'coef'
      colname.se <- 'se(coef)'
    }
    
    if (param == "dummy") {
      inc.name <- rownames(model.coef)[grepl('[dD]ummy|DUMMY', rownames(model.coef))]
      if (length(inc.name)==3) {
        nameABC <- inc.name
        coef = sapply(nameABC, function(row){return(which(rownames(model.coef)==row))})
        print(coef)
      }else{
        
        if (is.null(coef))
          stop("Error: dummy method must def three coef indices of dummy variable!")      
        names(coef) <- rownames(model.coef)[coef]
        print(coef)
        
      }

      
      
      
      colname.coef <- 'coef'
      colname.se <- 'se(coef)'
    }
    
    theta1 <- model.coef[ ,colname.coef][coef[1]]
    theta2 <- model.coef[ ,colname.coef][coef[2]]
    theta3 <- model.coef[ ,colname.coef][coef[3]]
    theta1.se <- model.coef[ ,colname.se][coef[1]]
    theta2.se <- model.coef[ ,colname.se][coef[2]]
    theta3.se <- model.coef[ ,colname.se][coef[3]]
  }
  
  if (theta1 < 0 | theta2 < 0) 
    warning("At least one of the two regression coefficients is less than zero (i.e., OR < 1). Estimates of RERI and AP will be invalid. Estimate of SI valid.")
  if (param == "product") {
    cov.mat <- vcov(model)
    h1 <- exp(theta1 + theta2 + theta3) - exp(theta1)
    h2 <- exp(theta1 + theta2 + theta3) - exp(theta2)
    h3 <- exp(theta1 + theta2 + theta3)
    reri.var <- (h1^2 * theta1.se^2) + (h2^2 * theta2.se^2) + 
      (h3^2 * theta3.se^2) + (2 * h1 * h2 * cov.mat[coef[1], 
                                                    coef[2]]) + (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) + 
      (2 * h2 * h3 * cov.mat[coef[2], coef[3]])
    reri.se <- sqrt(reri.var)
    reri.p <- exp(theta1 + theta2 + theta3) - exp(theta1) - 
      exp(theta2) + 1
    reri.l <- reri.p - (z * reri.se)
    reri.u <- reri.p + (z * reri.se)
    
    reri.pvalue <- 2*pt(abs(reri.p/reri.se), df=model.df, lower.tail = lower.tail)
    reri <- data.frame(est = reri.p, lower = reri.l, upper = reri.u, pvalue=reri.pvalue)
    rownames(reri) <- 'RERI'
    mult.p <- as.numeric(exp(theta3))
    mult.ci <- suppressMessages(confint(object = model, 
                                        parm = coef[3]))
    mult.l <- as.numeric(exp(mult.ci[1]))
    mult.u <- as.numeric(exp(mult.ci[2]))
    
    colname.pvalue <- colnames(model.coef)[grepl('Pr.+z.*', colnames(model.coef))]
    mult.pvalue <- model.coef[, colname.pvalue][coef[3]]
    
    
    multiplicative <- data.frame(est = mult.p, lower = mult.l, 
                                 upper = mult.u, pvalue = mult.pvalue)
    rownames(multiplicative) <- 'Multiplicative'
    cov.mat <- vcov(model)
    h1 <- ((exp(theta1 + theta2 + theta3) - exp(theta1))/(exp(theta1 + 
                                                                theta2 + theta3))) - ((exp(theta1 + theta2 + theta3) - 
                                                                                         exp(theta1) - exp(theta2) + 1)/(exp(theta1 + theta2 + 
                                                                                                                               theta3)))
    h2 <- ((exp(theta1 + theta2 + theta3) - exp(theta2))/(exp(theta1 + 
                                                                theta2 + theta3))) - ((exp(theta1 + theta2 + theta3) - 
                                                                                         exp(theta1) - exp(theta2) + 1)/(exp(theta1 + theta2 + 
                                                                                                                               theta3)))
    h3 <- 1 - ((exp(theta1 + theta2 + theta3) - exp(theta1) - 
                  exp(theta2) + 1)/exp(theta1 + theta2 + theta3))
    apab.var <- (h1^2 * theta1.se^2) + (h2^2 * theta2.se^2) + 
      (h3^2 * theta3.se^2) + (2 * h1 * h2 * cov.mat[coef[1], 
                                                    coef[2]]) + (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) + 
      (2 * h2 * h3 * cov.mat[coef[2], coef[3]])
    apab.se <- sqrt(apab.var)
    apab.p <- (exp(theta1 + theta2 + theta3) - exp(theta1) - 
                 exp(theta2) + 1)/exp(theta1 + theta2 + theta3)
    apab.l <- apab.p - (z * apab.se)
    apab.u <- apab.p + (z * apab.se)
    
    apab.pvalue <- 2*pt(abs(apab.p/apab.se), df=model.df, lower.tail = lower.tail)
    apab <- data.frame(est = apab.p, lower = apab.l, upper = apab.u, pvalue = apab.pvalue)    
    rownames(apab) <- 'APAB'

    s.p <- (exp(theta1 + theta2 + theta3) - 1)/(exp(theta1) + 
                                                  exp(theta2) - 2)
    cov.mat <- vcov(model)

    if (class(model)[1] == "svycoxph" & 
        class(model)[2] == "coxph" & 
        s.p < 0) {
      warning(paste("Point estimate of synergy index (S) is less than zero (", 
                    round(s.p, digits = 2), ").\n  Confidence intervals cannot be calculated using the delta method. Consider re-parameterising as linear odds model.", 
                    sep = ""))
    }
    
    h1 <- ((exp(theta1 + theta2 + theta3))/(exp(theta1 + 
                                                  theta2 + theta3) - 1)) - (exp(theta1)/(exp(theta1) + 
                                                                                           exp(theta2) - 2))
    h2 <- ((exp(theta1 + theta2 + theta3))/(exp(theta1 + 
                                                  theta2 + theta3) - 1)) - (exp(theta2)/(exp(theta1) + 
                                                                                           exp(theta2) - 2))
    h3 <- exp(theta1 + theta2 + theta3)/(exp(theta1 + theta2 + 
                                               theta3) - 1)
    lns.var <- h1^2 * theta1.se^2 + h2^2 * theta2.se^2 + 
      h3^2 * theta3.se^2 + (2 * h1 * h2 * cov.mat[coef[2], 
                                                  coef[1]]) + (2 * h1 * h3 * cov.mat[coef[3], coef[1]]) + 
      (2 * h2 * h3 * cov.mat[coef[3], coef[2]])
    lns.se <- sqrt(lns.var)
    lns.p <- log(s.p)
    lns.l <- lns.p - (z * lns.se)
    lns.u <- lns.p + (z * lns.se)
    s.l <- exp(lns.l)
    s.u <- exp(lns.u)
    
    s.pvalue <- 2*pt(abs(log(lns.p)/lns.se), df=model.df, lower.tail = lower.tail)
    s <- data.frame(est = s.p, lower = s.l, upper = s.u, pvalue = s.pvalue)
    rownames(s) <- 'S'
    rval = rbind(multiplicative, reri, apab, s)
    #rval <- list(multiplicative = multiplicative, reri = reri, apab = apab, s = s)
  }
  if (param == "dummy") {
    cov.mat <- vcov(model)
    h1 <- -exp(theta1)
    h2 <- -exp(theta2)
    h3 <- exp(theta3)
    reri.var <- (h1^2 * (cov.mat[coef[1], coef[1]])) + (h2^2 * 
                                                          (cov.mat[coef[2], coef[2]])) + (h3^2 * (cov.mat[coef[3], 
                                                                                                          coef[3]])) + (2 * h1 * h2 * cov.mat[coef[1], coef[2]]) + 
      (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) + (2 * 
                                                     h2 * h3 * cov.mat[coef[2], coef[3]])
    reri.se <- sqrt(reri.var)
    reri.p <- exp(theta3) - exp(theta1) - exp(theta2) + 1
    reri.l <- reri.p - (z * reri.se)
    reri.u <- reri.p + (z * reri.se)
    
    reri.pvalue <- 2*pt(abs(reri.p/reri.se), df=model.df, lower.tail = lower.tail)
    reri <- data.frame(est = reri.p, lower = reri.l, upper = reri.u, pvalue=reri.pvalue)
    rownames(reri) <- 'RERI'
    #reri <- data.frame(est = reri.p, lower = reri.l, upper = reri.u)
    mult.p <- as.numeric(exp(theta3))
    mult.ci <- suppressMessages(confint(object = model, 
                                        parm = coef[3]))
    mult.l <- as.numeric(exp(mult.ci[1]))
    mult.u <- as.numeric(exp(mult.ci[2]))

    colname.pvalue <- colnames(model.coef)[grepl('Pr.+z.*', colnames(model.coef))]
    mult.pvalue <- model.coef[, colname.pvalue][coef[3]]
    
    
    multiplicative <- data.frame(est = mult.p, lower = mult.l, 
                                 upper = mult.u, pvalue = mult.pvalue)
    rownames(multiplicative) <- 'Multiplicative'

    cov.mat <- vcov(model)
    h1 <- -exp(theta1 - theta3)
    h2 <- -exp(theta2 - theta3)
    h3 <- (exp(theta1) + exp(theta2) - 1)/exp(theta3)
    apab.var <- (h1^2 * (cov.mat[coef[1], coef[1]])) + 
      (h2^2 *(cov.mat[coef[2], coef[2]])) + 
      (h3^2 * (cov.mat[coef[3], coef[3]])) + 
      (2 * h1 * h2 * cov.mat[coef[1], coef[2]]) + 
      (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) + 
      (2 * h2 * h3 * cov.mat[coef[2], coef[3]])
    apab.se <- sqrt(apab.var)
    apab.p <- (exp(theta3) - exp(theta1) - exp(theta2) + 1)/exp(theta3)
    apab.l <- apab.p - (z * apab.se)
    apab.u <- apab.p + (z * apab.se)
    
    apab.pvalue <- 2*pt(abs(apab.p/apab.se), df=model.df, lower.tail = lower.tail)
    apab <- data.frame(est = apab.p, lower = apab.l, upper = apab.u, pvalue = apab.pvalue)    
    rownames(apab) <- 'APAB'

    s.p <- (exp(theta3) - 1)/(exp(theta1) + exp(theta2) - 
                                2)
    cov.mat <- vcov(model)

    if (class(model)[1] == "svycoxph" & class(model)[2] == 
        "coxph" & s.p < 0) {
      warning(paste("Point estimate of synergy index (S) is less than zero (", 
                    round(s.p, digits = 2), ").\n  Confidence intervals cannot be calculated using the delta method. Consider re-parameterising as linear odds model.", 
                    sep = ""))
    }

    h1 <- -exp(theta1)/(exp(theta1) + exp(theta2) - 2)
    h2 <- -exp(theta2)/(exp(theta1) + exp(theta2) - 2)
    h3 <- exp(theta3)/(exp(theta3) - 1)
    lns.var <- h1^2 * theta1.se^2 + 
      h2^2 * theta2.se^2 + 
      h3^2 * theta3.se^2 + 
      (2 * h1 * h2 * cov.mat[coef[2], coef[1]]) + 
      (2 * h1 * h3 * cov.mat[coef[3], coef[1]]) + 
      (2 * h2 * h3 * cov.mat[coef[3], coef[2]])
    lns.se <- sqrt(lns.var)
    lns.p <- log(s.p)
    lns.l <- lns.p - (z * lns.se)
    lns.u <- lns.p + (z * lns.se)
    s.l <- exp(lns.l)
    s.u <- exp(lns.u)
    # this Pvalue cal using log(p), log(se); CI all cal in log model and finally
    # trans CI to exp(log(lowlimit)), exp(log(lowlimit));
    # 经检验， 下面的公式是 delt 方法的 RERI 计算方法，P值是对应的；
    # 自己编写的方法需要手动指定 Df 自由度，默认100000; 其结果和 delt excel P值结果基本一直；
    # df 影响的是 P值得大小， 对SE CI COEF 没有影响。
    s.pvalue <- 2*pt(abs(lns.p/lns.se), df=model.df, lower.tail = lower.tail)
    s <- data.frame(est = s.p, lower = s.l, upper = s.u, pvalue = s.pvalue)
    rownames(s) <- 'S'
    rval = rbind(multiplicative, reri, apab, s)
    #rval <- list(reri = reri, apab = apab, s = s, multiplicative = multiplicative)
  }
  return(rval)
}
