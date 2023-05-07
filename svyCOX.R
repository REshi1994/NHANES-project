bind_list <- function(list,
                      fun,
                      seq=c(1:length(list)),
                      ...)
  {
  if(!is.list(list))
    stop(cat("object must be a list!"))

  for (i in seq) {
    if(which(seq==i)==1){
      combine <- list[[i]]
      # cat(paste(deparse(substitute(fun)),",  start from??",i,"\n",sep = ""))
    }else{
      combine <- fun(combine, list[[i]],...)
      # cat(paste(paste(deparse(substitute(fun)),",  ",i,"  ",deparse(substitute(...)),"\n",sep = "")))
    }
    
  }
  return(combine)
  
}


des_quantile <- function(qname, 
                         varname,
                         Y,
                         svyquantile.quantiles,
                         design,
                         digit_n=2)
{
  
  # cal total quantile
  exe_text_ <- paste("svyquantile(~",
                     varname, ',',
                     'design, ',
                     "quantile= c(", 
                     paste(svyquantile.quantiles, 
                           collapse = ",", sep = ''),"),",
                     'na.rm = T)',
                     sep = '')
  
  X.total.table <- eval(parse(text = exe_text_))[[varname]]
  X.total.table1 <- as.data.frame(X.total.table)[,c('quantile','se')]
  
  X.total.table1 <- X.total.table1[qlevels, "quantile"]
  
  
  
  # cal per X by Y.levels
  exe_text <- paste("svyby(~",
                    varname, ', ~',
                    Y, ',',
                    'design, ', 
                    'svyquantile, ',
                    "quantile= c(", 
                    paste(svyquantile.quantiles, 
                          collapse = ",", sep = ''),"),",
                    'na.rm = T)',
                    sep = '')
  X.table <- eval(parse(text = exe_text))
  X.table1 <- as.data.frame.matrix(X.table)
  X.table1 <- as.data.frame(X.table1) 
  
  qlevel.cols <- !grepl('^se.+', colnames(X.table1))
  X.table1 <- X.table1[,qlevel.cols]
  dgn <- paste("%.", digit_n, "f", sep = '')
  # 
  qlevel.cols2 <- grepl(paste(qlevels, collapse = '|'),
                        colnames(X.table1))
  
  # add total des to X.table1, and then gen new des info together
  X.table1["Sum", qlevel.cols2] <- X.total.table1
  X.table1[, qlevel.cols2] <- sapply(X.table1[, qlevel.cols2],
                                     function(col.q)
                                     {
                                       sprintf(dgn, col.q)
                                     })
  X.table1[, "quantile_des"] <- apply(X.table1[, qlevel.cols2],
                                      MARGIN = 1,
                                      function(row.q)
                                      {
                                        paste(row.q[1], " (",
                                              row.q[2], ", ",
                                              row.q[3], ")",
                                              sep = '')
                                      })
  
  X.table1 <- data.frame(t(X.table1))
  X.table1 <- X.table1["quantile_des",]
  X.table1[,'Varnames'] <- varname
  rownames(X.table1) <- c(1:nrow(X.table1))
  
  
  return(X.table1)
  
}
# des_quantile(svyquantile.names, 
#              varname,
#              Y,
#              svyquantile.quantiles,
#              design)




des_table1 <- function(formular, 
                       design, 
                       ref_total='total',
                       varnames_pre= '   ',
                       svy_numeric_show = 'se',
                       svychisq_statistic= 'Chisq',
                       svyranktest_test = 'KruskalWallis',
                       svyANOVA_method = 'Wald',
                       pvalue.limit = 0.001,
                       digit_n = 2,
                       svyquantile.names = NULL,
                       svyquantile.quantiles= c(.50,.25,.75),
                       conf.level = 0.95,
                       output=F,
                       output.name=NULL,
                       view=T)
{
  #svychisq_statistic ref:
  # svychisq_statistic <- c("F","Chisq","Wald","adjWald","lincom","saddlepoint")
  data <- design$variables
  dgn <- paste("%.", digit_n, 'f', sep = '')
  formular2 <- gsub(' ', '', formular)
  Y = formular2[2]
  X = formular2[3]
  X.names <- strsplit(X, '[+]')[[1]]
  
  X.levels <- sapply(X.names, function(X.name)
  {
    if (class(design$variables[,X.name]) == 'factor')
    {
      return(levels(design$variables[,X.name]))
    }
  })
  Y.levels <- levels(design$variables[,Y])
  
  exe_text <- paste("svytable(~",
                    Y, ',',
                    'design',
                    ')',
                    sep = '')
  Y.total <- eval(parse(text = exe_text))
  Y.total <- t(as.data.frame(addmargins(Y.total,1)))
  colnames(Y.total) <- Y.total[Y,]
  Y.total <- Y.total[, c('Sum', Y.levels)]
  
  Xdf.list <- list()
  tpb <- progress::progress_bar$new(total = length(X.levels),clear = F,width = 40,
                                    format = "[:bar](:spin) :current vars :percent :elapsed" )
  for (num in c(1:length(X.levels)))
  {
    tpb$tick()
    varname <- names(X.levels)[num]
    levels <- X.levels[[num]]
    
    if (!is.null(levels))
    {
      #------------------
      # cal total for per X
      exe_text4 <- paste("xtabs(~",
                         varname, ',',
                         'data',
                         ')',
                         sep = '')
      X.total.table1 <- eval(parse(text = exe_text4))
      X.total.table1 <- as.data.frame(X.total.table1)
      
      exe_text <- paste("svytable(~",
                        varname, ',',
                        'design)',
                        sep = '')
      X.total.table2 <- eval(parse(text = exe_text))
      X.total.table2 <- as.data.frame(X.total.table2)
      
      
      
      
      #-------------------------
      # no weight table cal real population number
      exe_text4 <- paste("xtabs(~",
                         varname, '+',
                         Y, ',',
                         'data',
                         ')',
                         sep = '')
      Xnowight.table <- eval(parse(text = exe_text4))
      Xnowight.table <- as.data.frame.matrix(Xnowight.table)
      Xnowight.table <- as.data.frame(Xnowight.table)
      
      
      rnm <- paste(varnames_pre,
                   rownames(Xnowight.table),
                   sep = '')
      
      Xnowight.table[,'Varnames'] <- rnm
      Xnowight.table[,'Sum'] <- X.total.table1[, "Freq"]
      Xnowight.table <- Xnowight.table[, c('Varnames','Sum', Y.levels)]
      addr <- Xnowight.table[1,]
      addr[1,] <- NA
      addr[,'Varnames'] <- varname
      
      Xnowight.table <- rbind(addr, Xnowight.table)
      rownames(Xnowight.table) <- c(1:nrow(Xnowight.table))
      
      # weight table cal prop
      exe_text <- paste("svytable(~",
                        Y, '+',
                        varname, ',',
                        'design)',
                        sep = '')
      X.table <- eval(parse(text = exe_text))
      X.table1 <- as.data.frame.matrix(X.table)
      X.table1 <- as.data.frame(t(X.table1))
      
      rnm <- paste(varnames_pre,
                   rownames(X.table1),
                   sep = '')
      
      X.table1[,'Varnames'] <- rnm
      X.table1[,'Sum'] <- X.total.table2[,"Freq"]
      X.table1 <- X.table1[, c('Varnames','Sum', Y.levels)]
      addr <- X.table1[1,]
      addr[1,] <- NA
      addr[,'Varnames'] <- varname
      
      X.table1 <- rbind(addr, X.table1)
      rownames(X.table1) <- c(1:nrow(X.table1))
      
      # cal svychisq pvalue
      exe_text2 <- paste("svychisq(~",
                         Y, '+',
                         varname, ',',
                         'design', ',',
                         'statistic=', 
                         "'",svychisq_statistic,"'",
                         ')',
                         sep = '')
      pvalue1 <- eval(parse(text = exe_text2))$p.value
      pvalue1 <- deal_pvalue(pvalue1, 
                             pvalue.limit = pvalue.limit)
      pvalue1[,'Statistic'] <- paste('svychisq(',
                                     svychisq_statistic,')',
                                     sep = '')
      pvalue1[2:nrow(X.table1) ,] <- NA
      
      # cal svyranktest pvalue
      exe_text3 <- paste("svyranktest(",
                         varname, '~',
                         Y, ',',
                         'design', ',',
                         'test=', 
                         "'",svyranktest_test,"'",
                         ')',
                         sep = '')
      pvalue2 <- eval(parse(text = exe_text3))$p.value
      pvalue2 <- deal_pvalue(pvalue2, 
                             pvalue.limit = pvalue.limit)
      pvalue2[,'Statistic'] <- paste('svyranktest(',
                                     svyranktest_test,')',
                                     sep = '')
      pvalue2[2:nrow(X.table1) ,] <- NA
      
      X.table2 <- cbind(X.table1, pvalue1, pvalue2)
      
      
      if (ref_total=='total')
      {
        for (ylevel in c('Sum',Y.levels))
        {
          
          # print(X.table2[, ylevel]/as.numeric(Y.total["Freq",ylevel][1]))
          X.table2[, ylevel] <- X.table2[, ylevel]/(as.numeric(Y.total["Freq",ylevel][1]))*100
          X.table2[, ylevel] <- sprintf(dgn,
                                        X.table2[, ylevel])
          # paste X.table2 and Noweight.table
          X.table2[, ylevel] <- paste(Xnowight.table[,ylevel], 
                                      ' (',
                                      X.table2[,ylevel],
                                      ')', sep = '')
          X.table2[grepl('NA.*)', X.table2[,ylevel]), 
                   ylevel] <- NA
          # X.table2[, ylevel] <- gsub('[\(\)]', '',X.table2[, ylevel])
        }
        
      }
      
      
      # numeric car cal 
    }else
    {
      
      # cal per X total
      exe_text <- paste("svymean(~",
                        varname, ',',
                        'design, ', 
                        'na.rm = T)',
                        sep = '')
      
      
      X.total.table <- eval(parse(text = exe_text))
      X.total.table1 <- as.data.frame(X.total.table)
      
      
      
      
      # cal per X by Y.levels
      exe_text <- paste("svyby(~",
                        varname, ', ~',
                        Y, ',',
                        'design, ', 
                        'svymean, ', 
                        'na.rm = T)',
                        sep = '')
      X.table <- eval(parse(text = exe_text))
      X.table1 <- as.data.frame.matrix(X.table)
      X.table1 <- as.data.frame(X.table1)
      
      if (svy_numeric_show=='se')
      {
        
        X.table1[, varname] <- paste(sprintf(dgn,
                                             X.table1[, varname]),
                                     ' (',
                                     sprintf(dgn,
                                             X.table1[, "se"]),
                                     ')',
                                     sep = '')
        # this difference from multi-factor loc
        X.total.table1[, varname] <- paste(sprintf(dgn,
                                                   X.total.table1[, 'mean']),
                                           ' (',
                                           sprintf(dgn,
                                                   X.total.table1[, varname]),
                                           ')',
                                           sep = '')
      }else if (svy_numeric_show=='sd')
      {
        X.table1[, varname] <- paste(sprintf(dgn,
                                             X.table1[, varname]),
                                     ' (',
                                     sprintf(dgn,
                                             (X.table1[, "se"])**2),
                                     ')',
                                     sep = '')
        # this difference from multi-factor loc
        X.total.table1[, varname] <- paste(sprintf(dgn,
                                                   X.total.table1[, 'mean']),
                                           ' (',
                                           sprintf(dgn,
                                                   (X.total.table1[, varname])**2),
                                           ')',
                                           sep = '')
      }else if (svy_numeric_show=='ci')
      {
        CI <- confint(X.table, level = conf.level)
        CI <- as.data.frame(CI)
        CI[,varname] <- paste('(',
                              sprintf(dgn,
                                      CI[, 1]),
                              '~',
                              sprintf(dgn,
                                      (CI[, 2])),
                              ')',
                              sep = '')
        X.table1[, varname] <- paste(sprintf(dgn,
                                             X.table1[, varname]),
                                     CI[, varname],
                                     sep = ' ')
        
        CI.total <- confint(X.total.table, level = conf.level)
        CI.total <- as.data.frame(CI.total)
        CI.total[,varname] <- paste('(',
                                    sprintf(dgn,
                                            CI.total[, 1]),
                                    '~',
                                    sprintf(dgn,
                                            (CI.total[, 2])),
                                    ')',
                                    sep = '')
        X.total.table1[, varname] <- paste(sprintf(dgn,
                                                   X.total.table1[, 'mean']),
                                           CI.total[, varname],
                                           sep = ' ')
      }
      
      X.table1 <- X.table1[, c(Y,varname)]
      X.table1 <- as.data.frame(t(X.table1))
      X.table1 <- X.table1[nrow(X.table1),]
      
      X.table1[,'Varnames'] <- rownames(X.table1)
      X.table1[,'Sum'] <- X.total.table1[, varname]
      
      if (!is.null(svyquantile.names))
      {
        qlevels <- as.character(svyquantile.quantiles)
        
        for (qname in svyquantile.names)
        {
          if (qname == varname)
          {
            X.table1 <- with(designs, des_quantile(qname, 
                                                   varname,
                                                   Y,
                                                   svyquantile.quantiles,
                                                   
                                                   digit_n = digit_n))
          }
        }
        
        
      }
      
      
      X.table1 <- X.table1[, c('Varnames','Sum', Y.levels)]
      rownames(X.table1) <- c(1:nrow(X.table1))
      
      if (length(Y.levels)==2)
      {
        exe_text1 <- paste("svyttest(",
                           varname, '~',
                           Y, ',',
                           'design',
                           ')',
                           sep = '')
        pvalue1 <- eval(parse(text = exe_text1))$p.value
        pvalue1 <- deal_pvalue(pvalue1, 
                               pvalue.limit = pvalue.limit)
        pvalue1[,'Statistic'] <- paste('svyttest(',
                                       'mean',')',
                                       sep = '')
        
      }else if (length(Y.levels)>2)
      {
        exe_text <- paste("svyglm(",
                          varname, '~',
                          Y, ',',
                          'design',
                          ')',
                          sep = '')
        svymodel <- eval(parse(text = exe_text))
        
        pvalue1 <- regTermTest(svymodel, Y, method = svyANOVA_method)$p
        pvalue1 <- deal_pvalue(pvalue1, 
                               pvalue.limit = pvalue.limit)
        pvalue1[,'Statistic'] <- paste('svyANOVA(',
                                       svyANOVA_method,')',
                                       sep = '')
        
        
      }
      
      # cal svyranktest pvalue
      exe_text3 <- paste("svyranktest(",
                         varname, '~',
                         Y, ',',
                         'design', ',',
                         'test=', 
                         "'",svyranktest_test,"'",
                         ')',
                         sep = '')
      pvalue2 <- eval(parse(text = exe_text3))$p.value
      pvalue2 <- deal_pvalue(pvalue2, 
                             pvalue.limit = pvalue.limit)
      pvalue2[,'Statistic'] <- paste('svyranktest(',
                                     svyranktest_test,')',
                                     sep = '')
      
      # bind X.table and pvalue
      pvalue <- cbind(pvalue1, pvalue2)
      X.table2 <- cbind(X.table1, pvalue)
      
      
    }
    Xdf.list[[num]] <- X.table2
    
  }
  table1 <- bind_list(Xdf.list, rbind)
  colnames(table1)[grepl('Sum',colnames(table1))] <- 'Total'
  
  title <- table1[1:3,]
  title[1:3,] <- NA
  title[1,1] <- paste("Baseline characteristics of participants from US National Health and Nutrition Examination Survey (US NHANES) by ",
                      Y)
  title[2, Y.levels[1]] <- Y
  title[3,] <- colnames(table1)
  title[3,1] <- 'Characteristics'
  # cal Y total number
  exe_text <- paste("xtabs(~",
                    Y, ',',
                    'data',
                    ')',
                    sep = '')
  Y.total.table <- eval(parse(text = exe_text))
  Y.total.table1 <- as.data.frame(addmargins(Y.total.table,1))
  rownames(Y.total.table1) <- Y.total.table1[, Y]
  
  
  title[3, c('Total', Y.levels)] <- paste(title[3, c('Total', Y.levels)],
                                          '\n',
                                          "(n=",
                                          Y.total.table1[c('Sum',Y.levels),"Freq"],
                                          ')',
                                          sep = '')
  
  
  for (col in c(1:ncol(table1)))
  {
    table1[, col] <- gsub('NA', '',sprintf("%s",table1[, col]))
    
  }
  if (view)
  {
    View(table1)
  }
  # 
  if (output)
  {
    tail.des <- paste('Note: ',
                      "Data are presented as weighted mean(SE), N(%) for continuous and categorical variables.",
                      " Weighted to be nationally representative. Weighted percentage may not sum to 100% because of missing data.",
                      '\n',
                      "Abbreviations: ",
                      " LTPA, leisure time physical activity; ",
                      "BMI, body mass index (calculated as weight in kilograms divided by height in meters squared); ",
                      "NHANES, the National Health and Nutrition Examination Survey; ",
                      "GPA, general physical activity; ",
                      "ADL, activities of daily living; ",
                      "NSAIDs, nonsteroidal anti-inflammatory drugs; ",
                      "COX-2, cyclooxygenase 2.",
                      sep = '')
    if (is.null(output.name))
    {
      output.name <- paste('table1', '.csv', sep = '')
    }else
    {
      output.name <- paste(output.name, '.csv', sep = '')
    }
    
    table2 <- rbind(title, table1)
    
    table2[(nrow(table2)+1), 1] <- NA
    table2[nrow(table2), 1] <- tail.des[1]
    colnames(table2) <- ''
    
    for (col in c(1:ncol(table2)))
    {
      table2[, col] <- gsub('NA', '',sprintf("%s",table2[, col]))
      
    }
    if (view){
      View(table2)
    }
    
    write.csv(table2, output.name, row.names = F)
  }
  
}



deal_pvalue <- function(model.pvalue, 
                        pvalue.limit = 0.001)
  {
  n = grepRaw('[1-9]', sprintf('%f',pvalue.limit)) - 2
  dg_n <- paste("%.",n,"f",sep = "")
  model.pvalue2 <- lapply(model.pvalue, function(pvalue){
    return(list(pvalue, pvalue.limit, dg_n))})
  # lapply(model.pvalue2, function(x){print(x[1])})
  
  pvalue.vector2 <- sapply(model.pvalue2, function(argus){
    pvalue <- argus[[1]]
    pvalue.limit <- argus[[2]]
    dg_n <- argus[[3]]
    if (pvalue<pvalue.limit){
      pvalue <- paste('<',sprintf(dg_n,pvalue.limit), sep = '')
    }else{
      pvalue <- sprintf(dg_n, pvalue)
    }
    return(pvalue)
    
  })
  
  pvalue.label <- sapply(model.pvalue, function(pvalue){
    pvalue <- ifelse(pvalue<0.001, '***',
                     ifelse(pvalue<0.01, '**',
                            ifelse(pvalue<0.05, '*',
                                   ifelse(pvalue<0.1, '.', ''))))
    
    
  })
  
  res.p <- data.frame(Pvalue = pvalue.vector2,
                      Plabel = pvalue.label)
  names(res.p) <- c('P-value', 'P-label')
  return(res.p)
  
}


deal_coef <- function(result.coef, 
                      coef.digt)
  {
  dg_n <- paste("%.",coef.digt,"f",sep = "")
  for (col in names(result.coef)) {
    result.coef[, col] <- sapply(result.coef[,col], function(value){
      sprintf(dg_n, value)
    })
  }
  
  new_result <- apply(result.coef, MARGIN = 1, function(row){
    paste(row[1], ' (', row[2], '~', row[3], ')', 
          sep = '')}, simplify = T)
  return(new_result)
}

deal_result <- function(result, 
                        model,
                        ref.group = F,
                        exp.coef = T,
                        X.NAME.repfix='',
                        X.LEVELS.prefix='   ')
{
  X.levels <- model$xlevels
  X.assign <- model$assign
  
  result2 <- sapply(X.assign, 
                    function(row){result[row,]},
                    simplify = F)
  result3 <- sapply(names(result2), 
                    function(varname)
                      {
                      deal.ys <- which(grepl(varname, names(X.levels)))
                      df <- result2[[varname]]
                      
                      if (length(deal.ys)>0)
                      {
                        df2 <- df[1,]
                        df2[1,] <- ''
                        varname <- paste(X.NAME.repfix,
                                         varname, sep = '')
                        df2[1,1] <- varname 
                        if (ref.group)
                        {
                          df3 <- df2
                          ref.level <- X.levels[[varname]][1]
                          df3[1,1] <- paste(X.LEVELS.prefix,
                                            ref.level, sep = '')
                          df3[1,2] <- paste('Ref (',
                                            ifelse(exp.coef,1,0),
                                            ')', sep = '')
                          df2 <- rbind(df2, df3)
                        }
                        
                        
                        df[,"Variables"] <- sapply(df[,"Variables"] ,
                                                   function(name)
                                                   {gsub(varname, X.LEVELS.prefix, name)} , 
                                                   simplify = T, 
                                                   USE.NAMES = F)                      
                        
                        df <- rbind(df2, df)
                      }

                      return(df)
                    },
                    simplify = F)
  
  
  result3 <- bind_list(result3, fun = rbind)
  rownames(result3) <- c(1:nrow(result3))
  return(result3)
}

res_svycoxph = function(model, 
                        coef.digt = 2,
                        pvalue.limit = 0.001,
                        coef.round = 2,
                        conf.level=0.95,
                        COEF.NAME = 'coef',
                        exp.coef = T,
                        SE.NAME = 'robust se',
                        PVALUE.NAME = "Pr(>|z|)",
                        view=T,
                        simple=T,
                        X.NAME.repfix='',
                        X.LEVELS.prefix='   ',
                        ref.group = F,
                        return.result.final=T)
{
  
  model.sum <- summary(model)
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  
  coef <- model.sum$coefficients
  
  if (exp.coef==T){
    model.ll <- exp(coef[, COEF.NAME]-z*coef[ ,SE.NAME])
    model.up <- exp(coef[, COEF.NAME]+z*coef[ ,SE.NAME])
  }else{
    model.ll <- coef[, COEF.NAME]-z*coef[ ,SE.NAME]
    model.up <- coef[, COEF.NAME]+z*coef[ ,SE.NAME]
  }

  model.pvalue <- coef[, PVALUE.NAME]
  result.p <- deal_pvalue(model.pvalue, pvalue.limit)
  
  # if cal OR,HR using exp(coef) else cal β using exp.
  result.coef <- data.frame(expcoef = if(exp.coef==T)
    {exp(coef[, COEF.NAME])}else{coef[, COEF.NAME]}, 
                            lower = model.ll, 
                            upper = model.up)
  result.coef <- deal_coef(result.coef, coef.digt)
  
  
  result <- cbind(Variables = names(result.coef),
                  result.coef, 
                  result.p, 
                  coef)
  colnames(result)[2]<- paste(COEF.NAME,' (',
                              100*conf.level ,'%CI)',
                              sep = '')
  rownames(result) <- c(1:nrow(result))
  if (!return.result.final)
  {
    if (view)
      View(result)
    return(result)
  }

  
  result <- deal_result(result,
                        model,
                        ref.group,
                        exp.coef,
                        X.NAME.repfix,
                        X.LEVELS.prefix)
  rownames(result) <- c(1:nrow(result))
  if (simple)
    {result <- result[,c(1,2,3,4)]}
    
  if (view)
    {View(result)}
    
  return(result)
}



deal_table2 <- function(results.Comb, view=T)
{
  res.coef <- sapply(results.Comb[,2], function(value)
  {
    if (grepl('Ref', value))
    {
      res <- c(1,1,1)
    }else if (grepl('~', value))
    {
      values <- strsplit(value,' ')
      est <- values[[1]][1]
      ci <- gsub('[()]', '',values[[1]][2])
      ci <- strsplit(ci,'~')[[1]]
      res <- c(est, ci)
      res <- sapply(res, as.numeric, simplify = T,USE.NAMES = F)
    }else{
      res <- c(NA,NA,NA)
    }
    
    
    return(res)
  },
  simplify = F
  )
  res.coef <- bind_list(res.coef, rbind)
  rownames(res.coef) <- c(1:nrow(results.Comb))
  colnames(res.coef) <- c('coef', 'lower', 'upper')
  res.final <- cbind(results.Comb, res.coef)
  res.final['Variables'] <- sapply(res.final['Variables'],
                                   function(value)
                                   {
                                     gsub(' ', '',value)
                                   },
                                   simplify = T)
  
  if (view)
  {
    View(res.final)
  }
  return(res.final)
}
# res_svycoxph instructions:

# 1. simplily get result dealed by input only one argus 'model'.
# default value==> show as uppper function





get_num_and_Weight.percent <- function(formulaZX,
                                       design,
                                       x,
                                       digit_n = 2,
                                       ref_total = 'total')
{
  # def x as risk factor, def Z as stratify var also called covariable
  
  dgn <- paste("%.", digit_n, 'f', sep = '')
  data <- design$variables
  X.levels <- levels(data[, x])
  #cal weighted number
  exe_text <- paste("svytable(~",
                    x, ',',
                    'design',
                    ')',
                    sep = '')
  x.total <- eval(parse(text = exe_text))
  x.total <- as.data.frame(addmargins(x.total,1))
  rownames(x.total) <- x.total[,x]
  
  # no weight table cal real population number
  exe_text4 <- paste("xtabs(~",
                     x, ',',
                     'data',
                     ')',
                     sep = '')
  Xnowight.table <- eval(parse(text = exe_text4))
  Xnowight.table <- as.data.frame(addmargins(Xnowight.table,1))
  rownames(Xnowight.table) <- Xnowight.table[,x]
  
  # gen table2 XY to cal weighted prop
  table2 <- svytable(formulaZX, design)
  table2 <- as.data.frame.matrix(table2)
  # gen table22 XY to cal no-weight number
  table22 <- xtabs(formulaZX, data)
  
  # cycle to bind all value
  if (ref_total == 'total')
  {
    table22 <- as.data.frame.matrix(table22)
    for (col in X.levels)
    {
      table2[, col] <- table2[, col]*100/(x.total[col, "Freq"])
      table2[, col] <- sprintf(dgn, table2[, col])
      table2[, col] <- paste(table22[, col],
                             ' (',
                             table2[, col],
                             ')',
                             sep = '')
    }
  }
  
  return(table2)
  
  
  
  
  
  
}


formular <- Surv(time = time, status) ~ LTPA3 +
  age2 + gender +race+education20+family_income+
  smoking_status + excessive_drinker+ BMI3+GPA+ADL+
  HTN_que + DM_que + RD + CVD +stroke+ cancer+
  NSAIDs_gen12 + NSAIDs_gen22 + weakOpioid2

res.svyCox.stratify <- function(formular, 
                                design, 
                                x='LTPA_time_estimate',
                                level.space='   ',
                                digit_n = 2,
                                ref_total = 'total',
                                plot_forest=T,
                                output=F,
                                output.name=NULL,
                                view=T,
                                split.coef=T)
{
  # generally we put x at first place in formula
  model <- svycoxph(formular, design)
  X.levels <- model$xlevels
  X.assign <- model$assign
  xlevels <- X.levels[[x]]
  results <- list()
  data <- design$variables
  # cal no-weight number for each group in x
  exe_text4 <- paste("xtabs(~",
                     x, ',',
                     'data',
                     ')',
                     sep = '')
  Xnowight.table <- eval(parse(text = exe_text4))
  Xnowight.table <- as.data.frame(addmargins(Xnowight.table,1))
  rownames(Xnowight.table) <- Xnowight.table[,x]
  
  # cal total coef
  result.total <- res_svycoxph(model, view = F)
  selrow <- length(X.levels[[x]])
  result.total <- result.total[1:selrow,1:3]
  
  if (plot_forest)
  {
    
    forest.rows <- sapply(xlevels[2:length(xlevels)], 
                          function(level)
                          {
                            result.total[grepl(level,result.total$Variables),]
                          },
                          simplify = F)
    result.total <- bind_list(forest.rows, cbind)
    
    covvars <- as.data.frame(matrix(Xnowight.table[xlevels,"Freq"], 
                                    nrow = 1))
    colnames(covvars) <- xlevels
    
    cnm1 <- xlevels
    cnm2 <- colnames(result.total)
    cnms <- c("Covariables", cnm1, cnm2)
    tolname <- paste(level.space, 'Adujusted', sep = '')
    result.total <- data.frame(Total = tolname,
                               covvars,
                               result.total)
    colnames(result.total) <- cnms
    result.total <- rbind(result.total, result.total)
    result.total[1,] <- ''
    result.total[1,1] <- "Overall"
  }
  
  # add progress bar
  tpb <- progress::progress_bar$new(total = length(X.levels),clear = F,width = 40,
                                    format = "[:bar](:spin) :current vars :percent :elapsed" )
  for (nnn in c(1:length(X.levels)))
  {
    tpb$tick()
    name <- names(X.levels)[nnn]
    if (name != x)
    {
      # get X levels
      Xlevs <- X.levels[[name]]
      # gen new formula exclude var which need to stratify
      formular2 <- gsub(name, '', formular)
      formular2[3] <- gsub('[+] *[+]', '+', formular2[3])
      formular2[3] <- gsub('[+] *$', '', formular2[3])
      formular2[3] <- gsub('^ *[+]', '', formular2[3])
      
      formular3 <- paste(formular2[2], 
                         formular2[1],
                         formular2[3],
                         sep = '')
      formular3 <- formula(formular3)
      
      formulaZX <- formula(paste('~', name, '+',x))
      svyprop <- get_num_and_Weight.percent(formulaZX,
                                            design,
                                            x,
                                            digit_n,
                                            ref_total)
      resX <- list()
      # n.events <- vector()
      # lets estimate model for each levels in stratified var
      for (num in c(1:length(Xlevs)))
      {
        lev <- Xlevs[num]
        # subset 
        design2 <- subset(design, 
                          design[["variables"]][,name]==lev & 
                            !is.na(design[["variables"]][,name]))
        
        
        fit <- svycoxph(formular3, design2)
        result <- res_svycoxph(fit, view = F)
        selrow <- length(X.levels[[x]])
        result <- result[1:selrow,1:3] # 行数？
        result[,1] <- paste(level.space,
                            result[,1], sep = '')
        result[1,1] <- lev
        if (plot_forest)
        {
          xlevels <- X.levels[[x]]
          forest.rows <- sapply(xlevels[2:length(xlevels)], 
                                function(level)
                                {
                                  result[grepl(level,result$Variables),]
                                },
                                simplify = F)
          result <- bind_list(forest.rows, cbind)
          rownames(result) <- lev
        }
        
        
        resX[[num]] <- result
        
        # list.enents[[name]] <- n.events
      }
      
      
      resX <- bind_list(resX, rbind)
      
      if (plot_forest)
      {
        covvars <- paste(level.space, rownames(resX),
                         sep = '')
        cnm1 <- colnames(svyprop)
        cnm2 <- colnames(resX)
        cnms <- c("Covariables", cnm1, cnm2)
        resX <- data.frame(covvars,
                           svyprop,
                           resX)
        colnames(resX) <- cnms
        title <- resX[1,]
        title[1,] <- ''
        title[1,1] <- name
        resX <- rbind(title, resX)
        rownames(resX) <- c(1:nrow(resX))
      }
      
      results[[nnn]] <- resX
      
    }
    
    
  }
  
  results2 <- bind_list(results, rbind)
  
  
  # bind total coef and stratify analysis
  results2 <- rbind(result.total, results2)
  rownames(results2) <- c(1:nrow(results2))
  colnames(results2)[grepl('Covariables',colnames(results2))] <- 'Subgroups'
  
  
  if (split.coef)
  {
    results2 <- deal_stratify.table(results2)
  }
  
  
  if (view)
  {
    View(results2)
  }
  
  if (output)
  {
    if (is.null(output.name))
    {
      output.name <- paste('table_STRATIFY', '.csv', sep = '')
    }else
    {
      output.name <- paste(output.name, '.csv', sep = '')
    }
    
    write.csv(results2, output.name, row.names = F)
  }
  
  
  
  return(results2)
  
  
}


deal_stratify.table <- function(strat_table)
{
  rep.cols <- grepl('coef|estimate|results *[(].+[)]', colnames(strat_table))
  
  rep.df <- strat_table[, rep.cols]
  
  dfn.list <- list()
  for (col in colnames(rep.df))
  {
    dfn <- sapply(rep.df[, col], function(value)
    {
      if (value=='' | is.na(value))
      {
        values = rep('', 3)
      }else
      {
        values <- strsplit(value,'[(~)]')[[1]]
      }
      
      
    }, simplify = F)
    dfn2 <- data.frame(bind_list(dfn, rbind))
    colnames(dfn2) <- c('coef', "lower", "upper")
    rownames(dfn2) <- c(1:nrow(dfn2))
    dfn.list[[col]] <- dfn2
  }
  dfn.list <- bind_list(dfn.list, cbind)
  
  strat_table <- cbind(strat_table, dfn.list)
  #add 2 rows for name
  title.name <- strat_table[1:2,]
  title.name[1:2,] <- ''
  title.name[1:2, 1] <- c('show.name1', 'show.name2')
  
  strat_table <- rbind(title.name, strat_table)
  rownames(strat_table) <- c(1:nrow(strat_table))
  return(strat_table)
}





des_weight_mortality <- function(X='LTPA6',
                                 mort='status',
                                 design,
                                 digit_n=2)
{
  dgn <- paste("%.", digit_n, "f", sep = '')
  xlevels <- levels(design$variables[, X])
  # cal total by X and mort
  exe.text2 <- paste('svytable(~',
                    X, '+',
                    mort, ', ',
                    'design)',
                    sep = '')
  total.weight2 <- eval(parse(text = exe.text2))
  total.weight2 <- addmargins(total.weight2,1)
  
  total.weight2.prop <- prop.table(total.weight2,1)*100
  total.weight2.prop <- as.data.frame.matrix(total.weight2.prop)
  
  
  total.weight2 <- as.data.frame.matrix(total.weight2)
  # bind 1 and 2
  total.weight2[, 'mortality'] <- paste(
    sprintf('%.0f' , total.weight2[, "TRUE"]),
    " (",
    sprintf(dgn , total.weight2.prop[, "TRUE"]),
    ')',
    sep = '')
  
  # cal no weight prop and number
  data <- design$variables
  exe.text3 <- paste('xtabs(~',
                     X, '+',
                     mort, ', ',
                     'data)',
                     sep = '')
  total.weight3 <- eval(parse(text = exe.text3))
  total.weight3 <- addmargins(total.weight3,1)
  total.weight3 <- addmargins(total.weight3,2)
  
  
  total.weight3 <- as.data.frame.matrix(total.weight3)
  # bind 1 and 2
  total.weight3[, 'mortality1'] <- paste(
    sprintf('%.0f' , total.weight3[, "TRUE"]),
    "/",
    sprintf('%.0f' , total.weight3[, "Sum"]),
    sep = '')
  
  total.weight <- cbind(total.weight2,
                        total.weight3)
  
  total.weight <- total.weight[,c("mortality1", "mortality")]
  
  total.weight <- total.weight[c('Sum', xlevels), ]
  colnames(total.weight) <- c('Death/No. of participants',
                              'Weighted death (%)')
  return(total.weight)
  
  }








