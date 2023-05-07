
# des imp table
des_table1.imp <- function(formular, 
                           designs, 
                           ref_total='total',
                           varnames_pre= '   ',
                           svy_numeric_show = 'se',
                           digit_n = 2,
                           svyquantile.names = NULL,
                           svyquantile.quantiles= c(.50,.25,.75),
                           conf.level = 0.95,
                           output=F,
                           output.name=NULL,
                           view=T,
                           sprintf0.num=F)
{
  design <- designs$designs[[1]]
  data <- design$variables
  data.list <- list()
  for (num in c(1:length(designs$designs)))
  {
    data.list[[num]] <- designs$designs[[num]]$variables
  }
  data.list <- imputationList(data.list)
  
  
  dgn <- paste("%.", digit_n, 'f', sep = '')
  formular2 <- gsub(' ', '', formular)
  X = formular2[2]
  Z = formular2[3]
  Z.names <- strsplit(Z, '[+]')[[1]]
  
  
  
  Z.levels <- sapply(Z.names, function(Z.name)
  {
    if (class(design$variables[,Z.name]) == 'factor')
    {
      return(levels(design$variables[,Z.name]))
    }
  })
  xlevels <- levels(data[, X])
  exe_text <- paste("svytable(~",
                    X, ',',
                    'design',
                    ')',
                    sep = '')
  X.total <- eval(parse(text = exe_text))
  X.total <- t(as.data.frame(addmargins(X.total,1)))
  X.total <- as.data.frame(X.total)
  colnames(X.total) <- X.total[X,]
  X.total <- X.total[, c('Sum', xlevels)]
  
  Zdf.list <- list()
  tpb <- progress::progress_bar$new(total = length(Z.levels),clear = F,width = 40,
                                    format = "[:bar](:spin) :current vars :percent :elapsed" )
  
  # cycle 
  for (num in c(1:length(Z.levels)))
  {
    tpb$tick()
    varname <- names(Z.levels)[num]
    zlevels <- Z.levels[[num]]
    
    if (varname != X) 
    {
      if (!is.null(zlevels))
      {
        # cal weight total for per X
        exe_text4 <- paste("with( designs,",
                           "svyby(~",
                           varname, ',',
                           "~", X, ',',
                           'FUN = svytotal',
                           '))',
                           sep = '')
        Z.total.table <- eval(parse(text = exe_text4))
        Z.total.table1 <- MIcombine(Z.total.table)
        Z.total.table1 <- summary(Z.total.table1)
        # rownames(Z.total.table1) <- gsub(varname, '',
        #                                  rownames(Z.total.table1))
        
        mt <- matrix(rep(NA, length(zlevels)*length(xlevels)),
                     nrow = length(zlevels),
                     ncol = length(xlevels),
                     dimnames = list(zlevels, xlevels))
        for (rw in zlevels)
        {
          for (col in xlevels)
          {
            zbind <- paste(varname, rw, sep = '')
            find.str <- paste(col, zbind, sep = ':')
            
            lg.str <- which(rownames(Z.total.table1)==find.str)
            mt[rw, col] <- Z.total.table1[lg.str, "results"]
            
          }
        }
        
        #  weight total number
        exe_text4 <- paste("with( designs,",
                           "svytotal(~",
                           varname,
                           '))',
                           sep = '')
        Z.total.table2 <- eval(parse(text = exe_text4))
        Z.total.table2 <- MIcombine(Z.total.table2)
        Z.total.table2 <- summary(Z.total.table2)
        rownames(Z.total.table2) <- gsub(varname, '',
                                         rownames(Z.total.table2))
        
        mt2 <- cbind(Z.total.table2, mt)
        mt2 <- mt2[,c('results', xlevels) ]
        colnames(mt2)[1] <- 'Sum'
        
        # no weight table cal real population number per X.levels
        exe_text4 <- paste("with( data.list,",
                           "xtabs(~",
                           varname, '+',
                           X,
                           '))',
                           sep = '')
        Znowight.table <- eval(parse(text = exe_text4))
        Znowight.table1 <- bind_list(Znowight.table, rbind)
        mt3 <- matrix(rep(NA, length(zlevels)*length(xlevels)),
                      nrow = length(zlevels),
                      ncol = length(xlevels),
                      dimnames = list(zlevels, xlevels))
        
        for (rw in zlevels)
        {
          lg.str <- which(rownames(Znowight.table1)==rw)
          for (col in xlevels)
          {
            
            if (sprintf0.num)
            {
              mt3[rw, col] <- as.numeric(
                sprintf('%.0f',mean(Znowight.table1[lg.str, col])))
            }else
            {
              mt3[rw, col] <- mean(Znowight.table1[lg.str, col])
            }
            
            
          }
        }
        
        #cal no weight total number
        exe_text4 <- paste("with( data.list,",
                           "xtabs(~",
                           varname, 
                           '))',
                           sep = '')
        Znowight.table2 <- eval(parse(text = exe_text4))
        Znowight.table2 <- bind_list(Znowight.table2, rbind)
        mt4 <- matrix(rep(NA, length(zlevels)),
                      nrow = length(zlevels),
                      ncol = 1,
                      dimnames = list(zlevels, "Sum"))
        for (rw in zlevels)
        {
          if (sprintf0.num)
          {
            mt4[rw, "Sum"] <- as.numeric(
              sprintf('%.0f',mean(Znowight.table2[,rw])) )
          }else
          {
            mt4[rw, "Sum"] <- mean(Znowight.table2[,rw])
          }
          
        }
        mt4 <- cbind(mt4, mt3)
        
        
        # Z.table2 <- mt2
        # Znowight.table <- mt4
        # cal weight prop
        if (ref_total=='total')
        {
          for (xlevel in c('Sum',xlevels))
          {
            
            # trans to prop
            mt2[, xlevel] <- mt2[, xlevel]/(as.numeric(X.total["Freq",xlevel][1]))*100
            # and set digit
            mt2[, xlevel] <- sprintf(dgn,
                                     mt2[, xlevel])
            # paste X.table2 and Noweight.table
            mt2[, xlevel] <- paste(mt4[,xlevel], 
                                   ' (',
                                   mt2[,xlevel],
                                   ')', sep = '')
            mt2[grepl('NA.*)', mt2[,xlevel]), 
                xlevel] <- NA
            # X.table2[, ylevel] <- gsub('[\(\)]', '',X.table2[, ylevel])
          }
          
        }
        
        rnm <- paste(varnames_pre,
                     rownames(mt2),
                     sep = '')
        rownames(mt2) <- rnm
        
        title1 <- mt2[1, ]
        title1[1,] <- ''
        rownames(title1) <- varname
        
        mt2 <- rbind(title1, mt2)
        mt2[, 'Variables'] <- rownames(mt2)
        mt2 <- mt2[, c('Variables', 'Sum', xlevels)]
        rownames(mt2) <- c(1:nrow(mt2))
        
        Z.table2 <- mt2
      }else
      {
        
        # cal per X total
        exe_text <- paste("with( designs,",
                          "svymean(~",
                          varname, ',', 
                          'na.rm = T))',
                          sep = '')
        
        
        Z.total.table <- eval(parse(text = exe_text))
        Z.total.table1 <- MIcombine(Z.total.table)
        Z.total.table1 <- summary(Z.total.table1)
        rownames(Z.total.table1) <- 'Sum'
        # cal per X by xlevels
        exe_text <- paste("with( designs,",
                          "svyby(~",
                          varname, ', ~',
                          X, ',',
                          'FUN = svymean, ', 
                          'na.rm = T))',
                          sep = '')
        Z.table <- eval(parse(text = exe_text))
        Z.table1 <- MIcombine(Z.table)
        Z.table1 <- summary(Z.table1)
        
        Z.table1 <- rbind(Z.total.table1, Z.table1)
        
        if (svy_numeric_show=='se')
        {
          Z.table1[, "results"] <- paste(sprintf(dgn,
                                                 Z.table1[, "results"]),
                                         ' (',
                                         sprintf(dgn,
                                                 Z.table1[, "se"]),
                                         ')',
                                         sep = '')
        }else if (svy_numeric_show=='sd')
        {
          Z.table1[, "results"] <- paste(sprintf(dgn,
                                                 Z.table1[, "results"]),
                                         ' (',
                                         sprintf(dgn,
                                                 (Z.table1[, "se"])**2),
                                         ')',
                                         sep = '')
        }else if (svy_numeric_show=='ci')
        {
          Z.table1[, "results"] <- paste(sprintf(dgn,
                                                 Z.table1[, "results"]),
                                         ' (',
                                         sprintf(dgn,
                                                 Z.table1[, "(lower"]),
                                         '~',
                                         sprintf(dgn,
                                                 Z.table1[, "upper)"]),
                                         ")",
                                         sep = '')
        }
        
        Z.table1 <- data.frame(t(Z.table1))
        Z.table1 <- Z.table1['results',]
        rownames(Z.table1) <- varname
        colnames(Z.table1) <- c('Sum', xlevels)
        
        if (!is.null(svyquantile.names))
        {
          qlevels <- as.character(svyquantile.quantiles)
          if (qname == varname)
          {
            exe_text <- paste("with(designs,",
                              "svyquantile( ~",
                              varname, ',',
                              "quantiles = svyquantile.quantiles))")
            
            mt5 <- eval(parse(text = exe_text))
            mt5 <- MIcombine(mt5)$coefficients
            mt5 <- sprintf(dgn, mt5)
            mt5 <- paste(mt5[1], 
                         " (",
                         mt5[2], ', ',
                         mt5[3],
                         ")",
                         sep = '')
            mt6 <- matrix(rep(NA, 1),
                          nrow = 1,
                          ncol = 1,
                          dimnames = list(varname, 'Sum'))
            mt6[1,1] <- mt5
            mt6 <- data.frame(mt6)
            
            exe_text2 <- paste("with(designs,",
                               "svyby( ~",
                               varname, ',',
                               '~',X, ',',
                               "FUN = svyquantile, ",
                               "quantiles = svyquantile.quantiles))")
            mt7 <- eval(parse(text = exe_text2))
            mt7 <- MIcombine(mt7)$coefficients
            names(mt7) <- gsub(varname, '', names(mt7))
            
            mt8 <- matrix(rep(NA, length(qlevels)*length(xlevels)),
                          nrow = length(xlevels),
                          ncol = length(qlevels),
                          dimnames = list(xlevels, qlevels))
            for (rw in xlevels)
            {
              for (col in qlevels)
              {
                print(paste(col, rw, sep = ':.'))
                lg.str <- grepl(paste(rw, col, sep = ':.'),
                                names(mt7))
                
                mt8[rw, col] <- mt7[lg.str]
                
              }
            }
            
            mt8[, 1] <- apply(mt8, MARGIN = 1, 
                              FUN = function(rw)
                              {
                                paste(sprintf(dgn, rw[1]),
                                      ' (',
                                      sprintf(dgn, rw[2]),
                                      ', ',
                                      sprintf(dgn, rw[3]),
                                      ')',
                                      sep = '')
                                
                              })
            
            colnames(mt8)[1] <- varname
            
            mt8 <- data.frame(t(mt8))[varname,]
            mt8 <- cbind(mt6, mt8)
            colnames(mt8) <- c('Sum', xlevels)
            
            Z.table1 <- mt8
            
            
          }
        }
        
        Z.table2 <- Z.table1
        
        Z.table2[, 'Variables'] <- varname
        X.table2 <- Z.table2[, c('Variables', 'Sum', xlevels)]
        rownames(Z.table2) <- c(1:nrow(Z.table2))
      }
    }
    
    
    Zdf.list[[num]] <- Z.table2
    
    
  }
  
  
  table1 <- bind_list(Zdf.list, rbind)
  colnames(table1)[grepl('Sum',colnames(table1))] <- 'Total'
  table1 <- table1[, c('Variables', 'Total', xlevels)]
  
  title <- table1[1:3,]
  title[1:3,] <- NA
  title[1,1] <- paste("Baseline characteristics of participants from US National Health and Nutrition Examination Survey (US NHANES) by ",
                      X)
  title[2, xlevels[1]] <- X
  title[3,] <- colnames(table1)
  title[3,1] <- 'Characteristics'
  # cal Y total number
  exe_text <- paste("xtabs(~",
                    X, ',',
                    'data',
                    ')',
                    sep = '')
  X.total.table <- eval(parse(text = exe_text))
  X.total.table1 <- as.data.frame(addmargins(X.total.table,1))
  rownames(X.total.table1) <- X.total.table1[, X]
  
  
  title[3, c('Total', xlevels)] <- paste(title[3, c('Total', xlevels)],
                                         '\n',
                                         "(n=",
                                         X.total.table1[c('Sum',xlevels),"Freq"],
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
                      "GPA, gerenal physical activity; ",
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



# cox analysis for imp
get_num_and_Weight.percent.imp <- function(formulaZX,
                                           designs,
                                           digit_n = 2,
                                           ref_total = 'total',
                                           sprintf0.num=F)
{
  # get Z, X name and levels
  formular2 <- as.character(formulaZX)
  formular3 <- gsub('[ ]', '', formular2[2])
  formular3 <- strsplit(formular3, '[+]')[[1]]
  
  
  Z = formular3[1]
  X = formular3[2]
  
  
  # def x as risk factor, def Z as stratify var also called covariable
  
  dgn <- paste("%.", digit_n, 'f', sep = '')
  design <- designs$designs[[1]]
  data <- design$variables
  xlevels <- levels(data[, X])
  zlevels <- levels(data[, Z])
  
  data.list <- list()
  for (num in c(1:length(designs$designs)))
  {
    data.list[[num]] <- designs$designs[[num]]$variables
  }
  data.list <- imputationList(data.list)
  
  
  
  
  
  #cal weighted number
  exe_text <- paste("svytable(~",
                    X, ',',
                    'design',
                    ')',
                    sep = '')
  x.total <- eval(parse(text = exe_text))
  x.total <- as.data.frame(addmargins(x.total,1))
  rownames(x.total) <- x.total[,X]
  
  # no weight table cal real population number
  exe_text4 <- paste("xtabs(~",
                     X, ',',
                     'data',
                     ')',
                     sep = '')
  Xnowight.table <- eval(parse(text = exe_text4))
  Xnowight.table <- as.data.frame(addmargins(Xnowight.table,1))
  rownames(Xnowight.table) <- Xnowight.table[,X]
  
  # gen table2 ZX to cal weighted prop
  # cal weight total for by
  exe_text4 <- paste("with( designs,",
                     "svyby(~",
                     Z, ',',
                     "~", X, ',',
                     'FUN = svytotal',
                     '))',
                     sep = '')
  Z.total.table <- eval(parse(text = exe_text4))
  Z.total.table1 <- MIcombine(Z.total.table)
  Z.total.table1 <- summary(Z.total.table1)
  rownames(Z.total.table1) <- gsub(Z, '',
                                   rownames(Z.total.table1))
  
  mt <- matrix(rep(NA, length(zlevels)*length(xlevels)),
               nrow = length(zlevels),
               ncol = length(xlevels),
               dimnames = list(zlevels, xlevels))
  for (rw in zlevels)
  {
    for (col in xlevels)
    {
      
      find.str <- paste(col, rw, sep = ':')
      # find.str <- gsub(spe.str,'.',find.str)
      lg.str <- which(rownames(Z.total.table1)==find.str)
      
      mt[rw, col] <- Z.total.table1[lg.str, "results"]
      
    }
  }
  
  #  weight total number
  exe_text4 <- paste("with( designs,",
                     "svytotal(~",
                     Z,
                     '))',
                     sep = '')
  Z.total.table2 <- eval(parse(text = exe_text4))
  Z.total.table2 <- MIcombine(Z.total.table2)
  Z.total.table2 <- summary(Z.total.table2)
  rownames(Z.total.table2) <- gsub(Z, '',
                                   rownames(Z.total.table2))
  
  mt2 <- cbind(Z.total.table2, mt)
  mt2 <- mt2[,c('results', xlevels) ]
  colnames(mt2)[1] <- 'Sum'
  
  
  # gen table22 XY to cal no-weight number
  
  exe_text4 <- paste("with( data.list,",
                     "xtabs(~",
                     Z, '+',
                     X,
                     '))',
                     sep = '')
  Znowight.table <- eval(parse(text = exe_text4))
  Znowight.table1 <- bind_list(Znowight.table, rbind)
  mt3 <- matrix(rep(NA, length(zlevels)*length(xlevels)),
                nrow = length(zlevels),
                ncol = length(xlevels),
                dimnames = list(zlevels, xlevels))
  
  for (rw in zlevels)
  {
    for (col in xlevels)
    {
      
      lg.str <- which(rownames(Znowight.table1)==rw)
      # 
      if (sprintf0.num)
      {
        mt3[rw, col] <- as.numeric(
          sprintf('%.0f',mean(Znowight.table1[lg.str, col])))
      }else
      {
        mt3[rw, col] <- mean(Znowight.table1[lg.str, col])
      }
      
      
    }
  }
  
  #cal no weight total number
  exe_text4 <- paste("with( data.list,",
                     "xtabs(~",
                     Z, 
                     '))',
                     sep = '')
  Znowight.table2 <- eval(parse(text = exe_text4))
  Znowight.table2 <- bind_list(Znowight.table2, rbind)
  mt4 <- matrix(rep(NA, length(zlevels)),
                nrow = length(zlevels),
                ncol = 1,
                dimnames = list(zlevels, "Sum"))
  for (rw in zlevels)
  {
    if (sprintf0.num)
    {
      mt4[rw, "Sum"] <- as.numeric(
        sprintf('%.0f',mean(Znowight.table2[,rw])) )
    }else
    {
      mt4[rw, "Sum"] <- mean(Znowight.table2[,rw])
    }
    
  }
  mt4 <- cbind(mt4, mt3)
  
  table2 <- mt2
  table22 <- mt4
  
  # cycle to bind all value
  if (ref_total == 'total')
  {
    
    for (col in c('Sum',xlevels))
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

res_svycoxph.imp = function(models, 
                        coef.digt = 2,
                        pvalue.limit = 0.001,
                        coef.round = 2,
                        conf.level=0.95,
                        COEF.NAME = 'results',
                        exp.coef = T,
                        SE.NAME = 'se',
                        model.df=100000, 
                        lower.tail=F,
                        view=T,
                        simple=T,
                        X.NAME.repfix='',
                        X.LEVELS.prefix='   ',
                        ref.group = F,
                        return.result.final=T)
{

  model <- MIcombine(models)

  model.sum <- summary(model)
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  
  coef <- model.sum
  
  if (exp.coef==T){
    model.ll <- exp(coef[, COEF.NAME]-z*coef[ ,SE.NAME])
    model.up <- exp(coef[, COEF.NAME]+z*coef[ ,SE.NAME])
  }else{
    model.ll <- coef[, COEF.NAME]-z*coef[ ,SE.NAME]
    model.up <- coef[, COEF.NAME]+z*coef[ ,SE.NAME]
  }
  
  coef.p <- coef[, "results"]
  coef.se <- coef[, 'se']
  model.pvalue <- apply(coef, MARGIN = 1,
                        function(rw)
                          {
                          
                          2*pt(abs(as.numeric(rw[COEF.NAME])/as.numeric(rw[SE.NAME])), 
                               df=model.df, 
                               lower.tail = lower.tail)
                        })
  result.p <- deal_pvalue(model.pvalue, pvalue.limit)
  
  # if cal OR,HR using exp(coef) else cal β using exp.
  result.coef <- data.frame(expcoef = if(exp.coef==T)
  {exp(coef[, COEF.NAME])}else{coef[, COEF.NAME]}, 
  lower = model.ll, 
  upper = model.up)
  result.coef <- deal_coef(result.coef, coef.digt)
  
  
  result <- cbind(Variables = rownames(model.sum),
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
                        models[[1]],
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



formular <- Surv(time = time, status) ~ LTPA_time_estimate +
  age2 + gender +race + education20 + family_income+
  smoking_status + excessive_drinker+ BMI3+GPA+ADL+
  HTN_que + DM_que + RD + CVD + stroke+ cancer +
  NSAIDs_gen12 + NSAIDs_gen22 + weakOpioid2
# stratify analysis for imp
res.svyCox.stratify.imp <- function(formular, 
                                    designs, 
                                    x='LTPA_time_estimate',
                                    level.space='   ',
                                    digit_n = 2,
                                    ref_total = 'total',
                                    plot_forest=T,
                                    output=F,
                                    output.name=NULL,
                                    view=T,
                                    split.coef=T,
                                    ref.group = F,
                                    pvalue.limit=0.001,
                                    sprintf0.num = F,
                                    model.df = Inf)
{
  # generally we put x at first place in formula
  model <- svycoxph(formular, designs$designs[[1]])
  X.levels <- model$xlevels
  X.assign <- model$assign
  xlevels <- X.levels[[x]]
  
  data <- designs$designs[[1]]$variables
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
  result.total <- res_svycoxph(model, 
                               view = F,
                               ref.group = ref.group,
                               coef.digt = digit_n,
                               pvalue.limit = pvalue.limit)
  selrow <- length(X.levels[[x]])
  result.total <- result.total[1:selrow,1:3]
  
  # cbind total number for all 
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
  
  results <- list()
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
      svyprop <- get_num_and_Weight.percent.imp(formulaZX,
                                                designs,
                                                digit_n,
                                                ref_total,
                                                sprintf0.num)
      resX <- list()
      # n.events <- vector()
      # lets estimate model for each levels in stratified var
      for (num in c(1:length(Xlevs)))
      {
        lev <- Xlevs[num]
        # subset 
        designs2 <- designs
        # subset designs
        for (numm in c(1:length(designs2$designs)))
        {
          design2 <- designs2$designs[[numm]]
          designs2$designs[[numm]] <- subset(design2, 
                                             design2[["variables"]][,name]==lev & 
                                               !is.na(design2[["variables"]][,name]))
        }
        
        fits <- with(designs2, svycoxph(formular3))
        
        
        result <- res_svycoxph.imp(fits, 
                                   view = F,
                                   coef.digt = digit_n,
                                   pvalue.limit = pvalue.limit,
                                   ref.group = ref.group,
                                   model.df = model.df)
        
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
  
  sel.col <- which(colnames(results2)!='Sum')
  cnm <- colnames(results2)[sel.col]
  results2 <- results2[,sel.col]
  colnames(results2) <- cnm
  # bind total coef and stratify analysis
  colnames(result.total) <- cnm
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
















bind_list <- function(list,
                      fun,
                      seq=c(1:length(list)),
                      ...)
{
  if(!is.list(list))
    stop(cat("?ϲ????ļ???????list??"))
  
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







