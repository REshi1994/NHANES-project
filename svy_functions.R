bind_list <- function(list,fun,seq=c(1:length(list)),...){
  if(!is.list(list))
    stop(cat("?ϲ????ļ???????list??"))
  
  
  
  for (i in seq) {
    if(which(seq==i)==1){
      combine <- list[[i]]
      cat(paste(deparse(substitute(fun)),",  start from??",i,"\n",sep = ""))
    }else{
      combine <- fun(combine, list[[i]],...)
      cat(paste(paste(deparse(substitute(fun)),",  ",i,"  ",deparse(substitute(...)),"\n",sep = "")))
    }
    
  }
  return(combine)
  
}
#####################
#NHANES LMF  ??NHIS LMF???????أ??Զ?��??????
lmf_download <- function(web="https://www.cdc.gov/nchs/data-linkage/mortality-public.htm",
                         source="nhanes",guidline_download=TRUE,method_download="curl"){
  
  #url is that public-motality website. Not FTP site including ftp files
  
  
  library(rvest)
  if(length(grep("ftp",web))==0){
    html <- read_html(web)
    ele <- html%>%html_elements("ul li a")%>%
      html_attr(.,"href")
    indexftp <- grep("ftp",ele)
    indexftp2 <- ele[indexftp]%>%grep("\\.R|\\.sas|\\.do",.)
    webftp <- ele[indexftp][-indexftp2]
  }else if(length(grep("ftp",web))==1){
    webftp <- web
  }
  
  if(isTRUE(guidline_download)){
    dir.create("guideline")
    index <- grep("\\.pdf|\\.R|\\.do|\\.sas",ele)
    
    for (i in index) {
      if(length(grep("\\.pdf",ele[i]))>0){
        ele[i] <- paste("https://www.cdc.gov",ele[i],sep = "")}
      name <- strsplit(ele[i],"/")[[1]]%>%.[length(.)]
      download.file(ele[i],paste("guideline/",name,sep = ""),
                    method = method_download)
    }
  }
  
  print(noquote(paste("ftp??ַ:",webftp)))
  ele2 <- read_html(webftp)%>%html_elements("pre a")%>%
    html_text()
  
  
  if(source=="nhanes"){
    index.nhanes <- grep("NHANES",ele2)
    for (i in index.nhanes) {
      url2 <- paste(webftp,ele2[i],sep = "")
      download.file(url2,ele2[i],method = method_download)
      
      dsn <- read_fwf(file=ele2[i],
                      col_types = "iiiiiiii",
                      fwf_cols(seqn = c(1,6),
                               eligstat = c(15,15),
                               mortstat = c(16,16),
                               ucod_leading = c(17,19),
                               diabetes = c(20,20),
                               hyperten = c(21,21),
                               permth_int = c(43,45),
                               permth_exm = c(46,48)),
                      na = c("", "."))
      str(dsn)
      table(dsn$eligstat)
      table(dsn$mortstat, useNA="ifany")
      table(dsn$ucod_leading, useNA="ifany")
      table(dsn$diabetes, useNA="ifany")
      table(dsn$hyperten, useNA="ifany")
      
      write.csv(dsn,paste(gsub("dat","",ele2[i])
                          ,"csv",sep = ""))
      
    }
    dir.create("NHANES")
    a <- list.files(getwd())
    from <- grep("NHANES_",a)
    to <- grep("NHANES",a[-from])
    for (i in from) {
      file.copy(a[i],a[to])
      file.remove(a[i])
    }
    
    
  }
  
  
  if(source=="nhis"){
    
    index.nhis <- grep("NHIS",ele2)
    for (i in index.nhis) {
      url3 <- paste(webftp,ele2[i],sep = "")
      download.file(url3,ele2[i],method = method_download)
      
      dsn <- read_fwf(file=ele2[i],
                      col_types = "ciiiiiiidd",
                      fwf_cols(publicid = c(1,14),
                               eligstat = c(15,15),
                               mortstat = c(16,16),
                               ucod_leading = c(17,19),
                               diabetes = c(20,20),
                               hyperten = c(21,21),
                               dodqtr = c(22,22),
                               dodyear = c(23,26),
                               wgt_new = c(27,34),
                               sa_wgt_new = c(35,42)),
                      na = c("", "."))
      str(dsn)
      table(dsn$eligstat)
      table(dsn$mortstat, useNA="ifany")
      table(dsn$ucod_leading, useNA="ifany")
      table(dsn$diabetes, useNA="ifany")
      table(dsn$hyperten, useNA="ifany")
      table(dsn$dodqtr, useNA="ifany")
      table(dsn$dodyear, useNA="ifany")
      write.csv(dsn,paste(gsub("dat","",ele2[i])
                          ,"csv",sep = ""))
      
      
      
    }
    dir.create("NHIS")
    a <- list.files(getwd())
    from <- grep("NHIS_",a)
    to <- grep("NHIS",a[-from])
    for (i in from) {
      file.copy(a[i],a[to])
      file.remove(a[i])
    }
    
  }
  
  
}


#????ת?????ߣ???Ҫ?Լ??ֶ??????ļ?
lmf_Trans <- function(years){
  
  
  
  if(is.integer(years)|(is.list(years))){
    
    name1 <- vector()
    name2 <- vector()
    sta =1 
    for (i in years) {
      if(i<1999 | i>2018){next}
      
      if(i%%2==1){
        name1[sta] = paste(i,"_",(i+1),sep = "")
      }else if(i%%2==0){
        name1[sta] = paste(i-1,"_",(i),sep = "")}
      sta= sta+1
    }
    
    name1 <- unique(name1)} 
  
  for (i in name1) {
    srvyin <- paste("NHANES_",i,"_MORT_2019_PUBLIC.dat",sep = "")
    srvyout <- paste("NHANES_",i,sep = "")
    dsn <- read_fwf(file=srvyin,
                    col_types = "iiiiiiii",
                    fwf_cols(seqn = c(1,6),
                             eligstat = c(15,15),
                             mortstat = c(16,16),
                             ucod_leading = c(17,19),
                             diabetes = c(20,20),
                             hyperten = c(21,21),
                             permth_int = c(43,45),
                             permth_exm = c(46,48)
                    ),
                    na = c("", ".")
    )
    
    # NOTE:   SEQN is the unique ID for NHANES.
    
    # Structure and contents of data
    str(dsn)
    
    
    # Variable frequencies
    
    #ELIGSTAT: Eligibility Status for Mortality Follow-up
    table(dsn$eligstat)
    #1 = "Eligible"
    #2 = "Under age 18, not available for public release"
    #3 = "Ineligible"
    
    #MORTSTAT: Final Mortality Status
    table(dsn$mortstat, useNA="ifany")
    # 0 = Assumed alive
    # 1 = Assumed deceased
    # <NA> = Ineligible or under age 18
    
    #UCOD_LEADING: Underlying Cause of Death: Recode
    table(dsn$ucod_leading, useNA="ifany")
    # 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
    # 2 = Malignant neoplasms (C00-C97)
    # 3 = Chronic lower respiratory diseases (J40-J47)
    # 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
    # 5 = Cerebrovascular diseases (I60-I69)
    # 6 = Alzheimer's disease (G30)
    # 7 = Diabetes mellitus (E10-E14)
    # 8 = Influenza and pneumonia (J09-J18)
    # 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
    # 10 = All other causes (residual)
    # <NA> = Ineligible, under age 18, assumed alive, or no cause of death data available
    
    #DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
    table(dsn$diabetes, useNA="ifany")
    # 0 = No - Condition not listed as a multiple cause of death
    # 1 = Yes - Condition listed as a multiple cause of death
    # <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available
    
    #HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
    table(dsn$hyperten, useNA="ifany")
    # 0 = No - Condition not listed as a multiple cause of death
    # 1 = Yes - Condition listed as a multiple cause of death
    # <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available
    
    # Re-name the dataset, DSN, to the short survey name then remove other R objects
    
    write.csv(dsn,paste(srvyout,"csv",sep = "."))
    
    rm(dsn, srvyin, srvyout)
    
    
  }
  rm(name1, sta)
}



############################
char_attr <- function(charr) {
  char.c <- function(classc){
    case_when(length(grep("[[:upper:]]",classc))==1 ~ "upper",
              length(grep("[[:lower:]]",classc))==1 ~ "lower",
              length(grep("[[:digit:]]",classc))==1 ~ "digit",
              length(grep("[[:punct:]]",classc))==1 ~ "punct",
              length(grep("[[:blank:]]",classc))==1 ~ "blank",
              TRUE ~ "chinese?")}
  
  charr.attr <- matrix(rep(""),nrow = nchar(charr),ncol = 3)
  colnames(charr.attr)<- c("attr","replace","new")
  rownames(charr.attr) <- c(1:nchar(charr))
  for (i in c(1:nchar(charr))) {
    c1 <- substr(charr,i,i)
    charr.attr[i,1] <- char.c(c1)
    rownames(charr.attr)[i] <- c1 
    
    c2 <- gsub("[[:punct:]]|[[:blank:]]",".",c1)
    charr.attr[i,2] <- c2}
  if(nchar(charr)>1){
    if(charr.attr[1,1]=="upper" & charr.attr[2,1]=="upper"){
      charr.attr[,3] <- toupper(charr.attr[,2])
    }else{
      charr.attr[,3] <- c(toupper(charr.attr[1,2]),tolower(charr.attr[-1,2]))}
  }else if(nchar(charr)==1){
    charr.attr[,3] <- toupper(charr.attr[,2])}
  return(charr.attr)
}
char.replace <- function(data.char, key.char="",charr.attr=F) {
  if(is.data.frame(data.char)){
    stop("Not support data.frame temporarily, suggest char vector instead!!",call. = F)
  }
  
  name.new <- sapply(data.char,FUN = char_attr)
  nn <- names(name.new)
  
  w.matrix <- list()
  total.w <- vector()
  total.name <- vector()
  total.replace <- vector()
  total.new <- vector()
  all <- list()
  for (i in nn) {
    
    w.list <- grep(key.char,i)
    attrr <- name.new[[i]][,1]
    w.matrix[[i]] <-grep(key.char,attrr)
    if(length(w.list)==0|length(w.matrix[[i]])==0)
      stop("There are no matching char terms",call. = F)
    if(length(w.list)!=0|length(w.matrix[[i]])!=0){
      if(length(w.matrix[[i]])==0){ w.matrix[[i]] <- "" }
      if(length(w.list)==0){w.list <- which(i==nn)}
      
      title <- matrix(rep(""),ncol = ncol(name.new[[i]]),
                      nrow = 3,
                      dimnames = list(c(paste("[",
                                              which(i==nn),"]",key.char),
                                        "Content:",
                                        "Strsplit"),
                                      colnames(name.new[[i]])))
      title[1,] <- c(" Location:",
                     paste(class(name.new)," ",which(i==nn),",",sep = ""),
                     paste("name: ",i,sep = ""))
      title[2,1] <- paste("(",class(name.new[[i]])[1],")",sep = "")
      title[2,2] <- paste("[","1,",
                          paste(w.matrix[[i]],collapse = ""),
                          "]",sep = "")
      title[3,] <- colnames(name.new[[i]])
      content <- name.new[[i]]
      all[[i]] <- rbind(title,content)
      colnames(all[[i]]) <- c("---------",
                              "--------",
                              paste(rep("-",(nchar(i)+6)),collapse = ""))
      if(isTRUE(charr.attr))
        print(noquote(all[[i]]))
      total.w[i] <-  which(i==nn)
      total.name[i] <- i 
      total.replace[i] <- paste(name.new[[i]][,"replace"],collapse = "")
      total.new[i] <- paste(name.new[[i]][,"new"],collapse = "")
    }
    
    
  }
  total <- data.frame(which=total.w,
                      origin=total.name,
                      replace=total.replace,
                      new=total.new,
                      row.names = c(1:length(total.w)))
  return(total)
}

###################

des_table1 <- function(fml,data_or_design,seq.by=NULL,na.rm=T,
                       output=F,output_style="se",output_name=NULL,
                       title.csv=c(NULL,NULL), confna.rm=T,conf_level.CI=0.95,
                       digits_n=2,digits_p=3,digits_p_round=F,
                       vars_prefix=NULL,vars_prefix_sep="",
                       method_numeric="Wald",
                       method_factor="Chisq",
                       method_ranktest="KruskalWallis",
                       view=T){
  # fml  =  x1+x2 ~ by 
  fmlc <- as.character(fml)
  fmlc.vars <- strsplit(fmlc[2], " [+] ")[[1]]
  spe.list <- list(fmlc.vars=grep("`",fmlc.vars),
                   by=grep("`",fmlc[3]))
  if(length(spe.list[[1]])>0){
    warning(paste("Var",
                  gsub("`","",
                       fmlc.vars[grep("`",fmlc.vars)]),"contains special characters!"),call. = F)
  }else if(length(spe.list[[2]])>0){
    warning(paste("GroupVar",
                  gsub("`","",fmlc[3]),"contains special characters!"),call. = F)}
  
  
  class.dod <- class(data_or_design)[length(class(data_or_design))]
  if(class.dod=="survey.design"){
    data.vars <- data_or_design[["variables"]]
  }else if(class.dod=="data.frame"){
    data.vars <- data_or_design}
  
  
  if(class(data.vars[[fmlc[3]]])!="factor")
    stop("Group var must be a factor!!! ")
  
  by.level <- list(nlevels= nlevels(data.vars[[fmlc[3]]]),
                   levels= levels(data.vars[[fmlc[3]]]))
  vars.level <- list()
  class.vars <- vector()
  for (i in fmlc.vars) {
    class.vars[i] <- class(data.vars[[i]])
    vars.level[[i]] <- if(class.vars[i]=="numeric"){i}
    else{levels(data.vars[[i]])}}
  # set digitals  
  dg_n <- paste("%.",digits_n,"f",sep = "")
  ttt <- paste("%.",digits_p,"f",sep = "")
  dg_p <- list(1*(10^-digits_p),99,ttt,sprintf(ttt,99))
  ppp <- function(x){
    pv <- x %>%ifelse(isTRUE(digits_p_round),round(.,digits = digits_n),.)%>%
      if_else(.<dg_p[[1]],dg_p[[2]],.) %>%sprintf(dg_p[[3]],.) %>%
      gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep=""))
    pl <- case_when(x<0.001 ~ "***",x<0.01 ~ "**",x<0.05 ~ "*",x<0.1 ~ ".",TRUE~"")
    return(c(pv,pl))}
  nnn <- function(x){n <-sprintf(dg_n,x) 
  return(n)}
  
  method.n <- c("mean","Wald","WorkingWald","LRT")
  names(method.n)<- method.n
  method.f <- c("F","Chisq","Wald","adjWald","lincom","saddlepoint")
  names(method.f)<- method.f  
  method.r <- c("wilcoxon", "vanderWaerden", "median","KruskalWallis")
  names(method.r)<- method.r
  sta <- if(class.dod=="survey.design"){
    c("svyttest","svyANOVA","svychisq","svyranktest")
  }else(c("ttest","ANOVA","chisq","ranktest"))
  output_style <- tolower(output_style)
  ME <- list()
  SE <- list()
  SE_all <- list()
  CP <- list()
  CP.title <- list()
  CP.new <- list()
  N. <- 1 - ((1 - conf_level.CI)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  
  pvalue <- list()
  pvalue.cln <- c("P-value","P-label","Statistic (method)",
                  "P-value","P-label","Statistic (method)")
  
  Tsubp <- list() 
  tpb <- progress::progress_bar$new(total = length(fmlc.vars),clear = F,width = 40,
                                    format = "[:bar](:spin) :current vars :percent :elapsed" )
  for (i in fmlc.vars){
    tpb$tick()
    if(class.vars[i]=="numeric"){
      
      pvalue[[i]] <- matrix(rep(""),ncol = 6,
                            nrow = 1,
                            dimnames = list(i,pvalue.cln))
      
      Tsubp[[i]] <- matrix(rep(""),nrow = 1,ncol = 2 + by.level[[1]]+ length(pvalue.cln),
                           dimnames = list(i,
                                           c("Variables","Total",by.level[[2]],
                                             colnames(pvalue[[i]])))) 
      if(class.dod=="survey.design"){
        fmlt.me <- formula(paste("~",i))
        fmlt.by <- formula(paste("~",fmlc[3]))
        m1 <- svymean(fmlt.me , data_or_design, na.rm = na.rm)
        m2 <- svyby(fmlt.me ,fmlt.by ,data_or_design,svymean, na.rm = na.rm)
        ME[[i]] <- c(m1[1],m2[,2])
        SE[[i]] <- c(SE(m1)[1],SE(m2))
      }else if(class.dod=="data.frame"){
        
        ME[[i]] <- c( mean(data.vars[[i]], na.rm = na.rm  ), 
                      by(data.vars[[i]], data.vars[[fmlc[3]]],FUN = mean , na.rm = na.rm))
        SE[[i]] <- sqrt(c(sd(data.vars[[i]], na.rm = na.rm ), 
                         by(data.vars[[i]], data.vars[[fmlc[3]]],FUN = sd, na.rm = na.rm)))}
      
      if(output_style=="se"){
        SE_all[[i]] <- paste(nnn(ME[[i]])," (",
                             nnn(SE[[i]]),")", sep="")
      }else if(output_style=="sd"){
        SE_all[[i]] <- paste(nnn(ME[[i]])," (",
                             nnn(SE[[i]]^2),")",sep = "")
      }else if(output_style=="ci"){
        SE_all[[i]] <- paste(nnn(ME[[i]])," (",
                             nnn(ME[[i]]- z*SE[[i]]),"~",
                             nnn(ME[[i]]+ z*SE[[i]]),")",
                             sep = "")}
      
      
      fmlt.p <- formula(paste(i,"~",fmlc[[3]]))
      if(class.dod=="survey.design"){
        if(by.level[[1]]==2 ){
          p <- svyttest(fmlt.p,
                        data_or_design)$p.value}
        if(by.level[[1]]>2 ){
          m.aov <- svyglm(fmlt.p,  data_or_design)
          p <- regTermTest(m.aov,fmlc[3],
                           method = (method.n[-1])[method_numeric])$p[1]}
        p1 <- svyranktest(fmlt.p,  data_or_design,
                          test = method.r[method_ranktest])$p.value[1]
        
        
      }else if(class.dod=="data.frame"){
        if(by.level[[1]]==2 ){
          p <- t.test(fmlt.p,data = data.vars)[["p.value"]]
          p1 <- wilcox.test(fmlt.p,data = data.vars)[["p.value"]]}
        if(by.level[[1]]>2 ){
          m1 <- aov(fmlt.p,data = data.vars)
          p <- regTermTest(m1,fmlc[3],
                           method = method.n[method_numeric])[["p"]][1]
          p1 <- kruskal.test(fmlt.p,data = data.vars)[["p.value"]]}}
      pvalue[[i]][1,3] <- paste(ifelse(by.level[[1]]==2,sta[1],sta[2])," (",
                                case_when(by.level[[1]]==2 ~ "mean",
                                          by.level[[1]]>2 & 
                                            class.dod=="data.frame" ~ "Wald",
                                          TRUE ~ (method.n[-1])[method_numeric]),")",sep = "")
      pvalue[[i]][1,6] <- paste(sta[4]," (",
                                case_when(class.dod=="survey.design" ~ method.r[method_ranktest],
                                          by.level[[1]]==2 ~ method.r[1],
                                          TRUE ~ method.r[4]),")",sep = "")
      pvalue[[i]][1,c(1,2,4,5)] <- c(ppp(p),ppp(p1))
      Tsubp[[i]][1,] <- c(i,SE_all[[i]],pvalue[[i]])
      
      
    }else if(class.vars[i]=="factor"){
      fml.chisq <- formula(paste("~",i,"+",fmlc[3],sep = ""))
      tab <- xtabs(fml.chisq , data = data.vars)
      if(class.dod=="survey.design"){
        prop <- nnn(prop.table(addmargins(
          svytable(fml.chisq,data_or_design),2),2)*100)
      }else{prop <- nnn(prop.table(addmargins(tab,2),2)*100) }
      
      CP[[i]] <- paste(addmargins(tab,2)," (",
                       prop,")")%>%
        matrix(.,ncol = by.level[[1]]+1,
               nrow = length(vars.level[[i]]),
               dimnames = list(vars.level[[i]],
                               c(by.level[[2]],"total")))%>%
        as.data.frame(.)%>%
        mutate(Variables=paste(ifelse(is.null(vars_prefix),
                                      paste("  ",i),vars_prefix),
                               rownames(.),
                               sep = vars_prefix_sep)
        )%>%.[,c("Variables","total",by.level[[2]])]
      CP.title[[i]] <- matrix(c(i,rep("",(ncol(CP[[i]])-1))),
                              nrow = 1,
                              ncol = by.level[[1]]+2,
                              dimnames = list(i,colnames(CP[[i]])))
      CP.new[[i]] <- rbind(CP.title[[i]], CP[[i]])
      pvalue[[i]] <- matrix(rep(""),ncol = 6,
                            nrow = nrow(CP.new[[i]]),
                            dimnames = list(rownames(CP.new[[i]]),pvalue.cln))
      
      
      if(class.dod=="survey.design"){
        fmlt.rank <- formula(paste(i,"~",fmlc[[3]]))
        p <- svychisq(fml.chisq,
                      data_or_design,
                      statistic=method.f[method_factor])$p.value
        p1 <- svyranktest(fmlt.rank,
                          data_or_design,
                          test = method.r[method_ranktest])$p.value[1]
      }else if(class.dod=="data.frame"){
        p <- chisq.test(tab)$p.value
        
        fmlt.rank <- paste("as.numeric(",i,")~",fmlc[3],sep = "")
        p1 <- if(by.level[[1]]==2 ){
          wilcox.test(eval(parse(text = fmlt.rank)),
                      data = data.vars)[["p.value"]]}
        else if(by.level[[1]]>2){
          kruskal.test(eval(parse(text = fmlt.rank)),
                       data = data.vars)[["p.value"]]}}
      pvalue[[i]][1,3] <- paste(sta[3]," (",
                                case_when(class.dod=="survey.design" ~ method.f[method_factor],
                                          TRUE ~ method.f[2]),")",sep = "")
      pvalue[[i]][1,6] <- paste(sta[4]," (",
                                case_when(class.dod=="survey.design" ~ method.r[method_ranktest],
                                          by.level[[1]]==2 ~ method.r[1],
                                          TRUE ~ method.r[4]),")",sep = "")
      
      pvalue[[i]][1,c(1,2,4,5)] <- c(ppp(p),ppp(p1))
      Tsubp[[i]] <- cbind( CP.new[[i]] , pvalue[[i]])%>%as.matrix(.)}}
  
  print.view <- list()
  for (i in c(1:length(Tsubp))) {
    if(i==1){
      print.view[[1]] <- Tsubp[[1]]}
    if(i>1){
      print.view[[i]] <- rbind(print.view[[i-1]],Tsubp[[i]])}}
  
  
  print.view <- print.view[[length(print.view)]]
  rownames(print.view)<- c(1:nrow(print.view))
  
  
  by.count <- addmargins(xtabs(formula(paste(fmlc[1],fmlc[3],sep = "")),
                               data = data.vars)
  )%>%.[c("Sum",by.level[[2]])]
  
  colnames(print.view)[c(1:(1+by.level[[1]]))+1] <- 
    paste(c("Total",by.level[[2]]),
          "\n","(n=",by.count,")",sep = "")
  
  
  if(isTRUE(output)){
    title <- matrix(rep(""),nrow = 4,ncol = ncol(print.view),
                    dimnames = list(c(1:4),colnames(print.view)))
    title[1,1] <- ifelse(is.null(title.csv[1]),
                         paste("Table 1 Baseline characteristics of participants by categories of ",
                               fmlc[3],sep = ""), title.csv[1]) 
    title[2,3] <- ifelse(is.null(title.csv[2]),fmlc[3],title.csv[2])
    title[3,] <- colnames(title)
    title[4,1] <- paste("Notes: Data are presented as mean (",
                        ifelse(output_style=="se","SE)","95% CI)"),
                        " or N (%).","\nAbbreviations: ",sep = "")
    oup <- rbind(title[c(1:3),], print.view, title[4,])
    rownames(oup) <- c(1:nrow(oup))
    colnames(oup) <- rep("",ncol(oup))
    
    output_name <- ifelse(is.null(output_name),
                          paste(toupper(output_style),
                                fmlc[3],"levels",by.level[[1]],
                                "csv",sep = "."),output_name)
    
    write.csv(oup,output_name)
    if(isTRUE(view))
      View(oup)
    return(head(noquote(oup)))
  }else{
    if(isTRUE(view))
      View(print.view)
    return(head(noquote(print.view)))}
  
}



###################
#des_SE_CI new
des_SE_CI <- function(vars,by,design,
                      output=T,style_output="se",output_name=NULL,
                      digits_n=2,digits_p=3,style_p_round=F,
                      vars_style=2,vars_prefix=NULL){
  n_vars <- as.character(vars)%>%strsplit(.,"[+]")%>%.[[2]]%>%
    gsub(.,pattern = " ",replacement = "")#vars???б?��????
  n_by <- as.character(by)%>%strsplit(.,"[+]")%>%.[[2]]%>%
    gsub(.,pattern = " ",replacement = "") #by ??��????
  subgps <-nlevels(design[["variables"]][[n_by]])
  subgps_names <- levels(design[["variables"]][[n_by]])
  svyttest_names <- paste0(n_vars,"~",n_by,sep ="")
  
  #a <- levels(design[["variables"]][[   n_vars[n]   ]])
  tab_names <- paste0("~",n_vars,"+",n_by,sep ="")
  unwtd_by <- addmargins(xtabs(by,data = design[["variables"]]))%>%.[c(subgps+1,c(1:subgps))]
  dg_n <- paste("%.",digits_n,"f",sep = "")
  #dg_p?????趨
  ttt <- paste("%.",digits_p,"f",sep = "")
  dg_p <- list(1*(10^-digits_p),99,ttt,sprintf(ttt,99))
  
  if(style_output=="se"){
    SE <- svymean(vars,design)%>%
      data.frame(.)%>%
      mutate(mean_SE=paste(sprintf(dg_n,.[,1])," (",
                           sprintf(dg_n,.[,2]),")",sep = ""),
             Variables=rownames(.))%>% .[,c(4,3)]
    SEby <- data.frame(t(svyby(vars,by,design,svymean))) %>% .[-1,] %>%
      data.frame(.,.[grep("se|both",rownames(.)),])%>% 
      .[-c(grep("se",rownames(.))),]%>%
      mutate_all(.,as.numeric)
    
    newcol_SEby <- c((2*subgps+1):(3*subgps))
    for (i in newcol_SEby) {
      SEby[,i] <- paste(sprintf(dg_n,SEby[,i-2*subgps])," (",
                        sprintf(dg_n,SEby[,i-subgps]),")",sep = "")
    }
    SE_all <- data.frame(SE,SEby[,-c(1:(2*subgps))])
  }else if(style_output=="ci|both"){
    CI <-  svymean(vars,design)%>%
      data.frame(.,confint(.))%>%
      mutate(mean_95CI=paste(sprintf(dg_n,.[,1])," (",
                             sprintf(dg_n,.[,3]),"~",
                             sprintf(dg_n,.[,4]),")",sep = ""),
             Variables=rownames(.))%>% .[,c(6,5)]
    CIby <- data.frame(confint(svyby(vars,by,design,svymean))) 
    vb <- c(1:subgps)
    rowby <- list()
    CIby1 <- list()
    for (i in vb) {
      rowby[[i]] <- seq(i,(sum(CIby[,1]>=0)),subgps)
      CIby1[[i]] <- CIby[rowby[[i]],]
    }
    CIby2  <- data.frame(CIby1[[1]])
    for (i in vb) {
      CIby2[,c((2*i-1):(2*i))] <- CIby1[[i]]
    }
    SE <- data.frame(t(svyby(vars,by,design,svymean)))
    CIby <-CIby2%>% data.frame(SE[-c(1,grep("se",row.names(SE))),])%>%
      mutate_all(.,as.numeric)
    vb1 <- seq(1,2*subgps,2)
    for (i in vb) {
      CIby[,(3*subgps+i)] <- paste(sprintf(dg_n,CIby[,2*subgps+i])," (",
                                   sprintf(dg_n,CIby[,vb1[vb[i]]]),"~",
                                   sprintf(dg_n,CIby[,vb1[vb[i]]+1]),")",sep = "")
    }
    CI_all <- data.frame(CI,CIby[,c((3*subgps+1):(4*subgps))])
    
  }
  
  
  vc <- c(1:length(n_vars))
  svyttest_formula <- list()
  for (i in vc) {
    svyttest_formula[[i]] <- formula(svyttest_names[i])
  }
  
  if(subgps==2){
    pvalue <- vector()
    pvalue1 <- vector()
    for (i in vc ){
      pvalue[i] <- svyttest(svyttest_formula[[i]],design)$p.value
      pvalue1[i] <- svyranktest(svyttest_formula[[i]],design)$p.value
    }
    
    for (i in vc ){
      pvalue1[i] <- svyranktest(svyttest_formula[[i]],design)$p.value
    }
    if(isFALSE(style_p_round)){
      pvalue <- matrix(pvalue,nrow =length(n_vars) ,ncol = 1,
                       dimnames = list(c(1:length(n_vars)),"P-value"))%>%
        if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
        sprintf(dg_p[[3]],.) %>%
        gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
    }else{
      pvalue <- matrix(pvalue,nrow =length(n_vars) ,ncol = 1,
                       dimnames = list(c(1:length(n_vars)),"P-value"))%>%
        round(.,digits = digits_p)%>%
        if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
        sprintf(dg_p[[3]],.) %>%
        gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
    }
    
  }else if(subgps>=3){
    pvalue1 <- vector()
    for (i in vc ) {
      pvalue1[i] <- svyranktest(svyttest_formula[[i]],design)$p.value
    }
  }
  #Pֵ???㣬ʵ????????С????λ??
  if(isFALSE(style_p_round)){
    pvalue1 <- matrix(pvalue1,nrow =length(n_vars) ,ncol = 1,
                      dimnames = list(c(1:length(n_vars)),"P-value"))%>%
      if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
      sprintf(dg_p[[3]],.) %>%
      gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
  }else{
    pvalue1 <- matrix(pvalue1,nrow =length(n_vars) ,ncol = 1,
                      dimnames = list(c(1:length(n_vars)),"P-value"))%>%
      round(.,digits = digits_p)%>%
      if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
      sprintf(dg_p[[3]],.) %>%
      gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
  }
  
  
  if(vars_style==1){#Ҫ????��label??names ?????ո?ʽ?????ã?????һ??????
    di_names2_both <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value","P-value")
    di_names2_both2 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value")
    
    di_names2_se_or_ci <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value","P-value")
    di_names2_se_or_ci2 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
    di_names2_both1 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value","P-value")
    di_names2_both21 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value")
    
    di_names2_se_or_ci1 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value","P-value")
    di_names2_se_or_ci21 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
  }else if(vars_style==2|vars_style==3){
    di_names2_both <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value","P-value")
    di_names2_both2 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value")
    
    di_names2_se_or_ci <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value","P-value")
    di_names2_se_or_ci2 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
    di_names2_both1 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value","P-value")
    di_names2_both21 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,.,"P-value")
    
    di_names2_se_or_ci1 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value","P-value")
    di_names2_se_or_ci21 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
  }
  
  
  
  if(isTRUE(output)) {
    if(is.null(output_name)) {
      if(subgps>=3){
        if(style_output=="both") {
          SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue1)
          colnames(SE_CI_full)<-di_names2_both2
          nullname <- paste("SE+CI_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_both21
          return(SE_CI_full)
        }else if(style_output=="se") {
          SE_CI_full <- data.frame(SE_all,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci2
          nullname <- paste("SE_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_se_or_ci21
          return(SE_CI_full)
        }else if(style_output=="ci") {
          SE_CI_full <- data.frame(CI_all,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci2
          nullname <- paste("CI_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_se_or_ci21
          return(SE_CI_full)
        }
      }else if(subgps==2){
        if(style_output=="both") {
          SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_both
          nullname <- paste("SE+CI_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_both1
          return(SE_CI_full)
        }else if(style_output=="se") {
          SE_CI_full <- data.frame(SE_all,pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci
          nullname <- paste("SE_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_se_or_ci1
          return(SE_CI_full)
        }else if(style_output=="ci") {
          SE_CI_full <- data.frame(CI_all,pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci
          nullname <- paste("CI_subgps",subgps,".csv",sep = "")
          write.csv(SE_CI_full,nullname)
          colnames(SE_CI_full)<-di_names2_se_or_ci1
          return(SE_CI_full)
        }
      }
      
    }else {
      output_name <- paste(output_name,"csv",sep = ".")
      if(subgps>=3){
        if(style_output=="both") {
          SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue1)
          colnames(SE_CI_full)<-di_names2_both2
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_both21
          return(SE_CI_full)
        }else if(style_output=="se") {
          SE_CI_full <- data.frame(SE_all,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci2
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_se_or_ci21
          return(SE_CI_full)
        }else if(style_output=="ci") {
          SE_CI_full <- data.frame(CI_all,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci2
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_se_or_ci21
          return(SE_CI_full)
        }
      }else if(subgps==2){
        if(style_output=="both") {
          SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_both
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_both1
          return(SE_CI_full)
        }else if(style_output=="se") {
          SE_CI_full <- data.frame(SE_all,pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_se_or_ci1
          return(SE_CI_full)
        }else if(style_output=="ci") {
          SE_CI_full <- data.frame(CI_all,pvalue,pvalue1)
          colnames(SE_CI_full)<-di_names2_se_or_ci
          write.csv(SE_CI_full,output_name)
          colnames(SE_CI_full)<-di_names2_se_or_ci1
          return(SE_CI_full)
        }
      }
      
      
    }
  }else {
    if(subgps>=3){
      if(style_output=="both") {
        SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue1)
        colnames(SE_CI_full)<-di_names2_both21
        return(SE_CI_full)
      }else if(style_output=="se") {
        SE_CI_full <- data.frame(SE_all,pvalue1)
        colnames(SE_CI_full)<-di_names2_se_or_ci21
        return(SE_CI_full)
      }else if(style_output=="ci") {
        SE_CI_full <- data.frame(CI_all,pvalue1)
        colnames(SE_CI_full)<-di_names2_se_or_ci21
        return(SE_CI_full)
      }
    }else if(subgps==2){
      if(style_output=="both") {
        SE_CI_full <- data.frame(SE_all,CI_all[,-1],pvalue,pvalue1)
        colnames(SE_CI_full)<-di_names2_both1
        return(SE_CI_full)
      }else if(style_output=="se") {
        SE_CI_full <- data.frame(SE_all,pvalue,pvalue1)
        colnames(SE_CI_full)<-di_names2_se_or_ci1
        return(SE_CI_full)
      }else if(style_output=="ci") {
        SE_CI_full <- data.frame(CI_all,pvalue,pvalue1)
        colnames(SE_CI_full)<-di_names2_se_or_ci1
        return(SE_CI_full)
      }
    } 
  }
  
  
}



#####################
des_counts <- function(vars,by,design,
                       style_p_round=F,digits_p=3,digits_n=1,
                       output=T,output_name=NULL,
                       vars_style=2,vars_prefix=NULL){
  n_by <- as.character(by)%>%strsplit(.,"[+]")%>%.[[2]] #by ??��????
  subgps <-nlevels(design[["variables"]][[n_by]])   #by ????ˮƽ
  subgps_names <- levels(design[["variables"]][[n_by]])  #by????????
  
  n_vars <- as.character(vars)%>%strsplit(.,"[+]")%>%.[[2]]%>%
    gsub(.,pattern = " ",replacement = "")#vars???б?��????
  #a <- levels(design[["variables"]][[   n_vars[n]   ]])
  tab_names <- paste0("~",n_vars,"+",n_by,sep ="")
  unwtd_by <- addmargins(xtabs(by,data = design[["variables"]]))%>%.[c(subgps+1,c(1:subgps))]
  
  
  
  #????Variables?? ???㣻û???漰 n_by
  vc <- c(1:length(n_vars))#??��????ת??Ϊ????˳????Ϊ????ѭ????׼??
  
  lev_names <- list()
  for (i in n_vars) {
    lev_names[[i]] <- levels(design[["variables"]][[i]])
  }
  
  
  lev <- vector()
  di_names1 <-list()
  tab_formula <- list()
  prop <- list()
  unwtd_all <- list()
  Tsub <- list()
  for (i in vc) {
    lev[i] <- length(lev_names[[i]])
    tab_formula[[i]] <- formula(tab_names[i])
    prop[[i]] <- rbind(prop.table(addmargins(svytable(tab_formula[[i]],design),2),2)*100)
    unwtd_all[[i]] <- rbind(addmargins(xtabs(tab_formula[[i]],data = design[["variables"]]),2))
    Tsub[[i]] <- data.frame(unwtd_all[[i]],prop[[i]])
  }
  
  
  #list ?? ??????
  newcol <- c((2*subgps+3):(3*subgps+3))
  if(digits_n==1){
    for (i in vc) {
      for (k in newcol) {
        Tsub[[i]][,k] <-paste(sprintf("%.0f",Tsub[[i]][,k-2*subgps-2])," (",
                              sprintf("%.1f",Tsub[[i]][,k-subgps-1]),")",sep = "")
      }
      Tsub[[i]] <- Tsub[[i]][,c((2*subgps+3):(3*subgps+3))]%>%.[,c(subgps+1,c(1:subgps))]
    }
  }else if(digits_n==2){
    for (i in vc) {
      for (k in newcol) {
        Tsub[[i]][,k] <-paste(sprintf("%.0f",Tsub[[i]][,k-2*subgps-2])," (",
                              sprintf("%.2f",Tsub[[i]][,k-subgps-1]),")",sep = "")
      }
      Tsub[[i]] <- Tsub[[i]][,c((2*subgps+3):(3*subgps+3))]%>%.[,c(subgps+1,c(1:subgps))]
    }
  }
  
  
  if(vars_style==1){#Ҫ????��label??names ?????ո?ʽ?????ã?????һ??????
    for (i in vc) {
      di_names1[[i]] <- paste(vars_prefix,lev_names[[i]],sep = "")
      dimnames(Tsub[[i]])[1] <- list(di_names1[[i]])
    }
    di_names2 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    di_names22 <- paste(subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
  }else if(vars_style==2|vars_style==3){
    for (i in vc) {
      di_names1[[i]] <- paste(vars_prefix,n_vars[i],lev_names[[i]],sep = "")
      dimnames(Tsub[[i]])[1] <- list(di_names1[[i]])
    }
    di_names2 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.,"\n(n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    di_names22 <- paste(n_by,subgps_names,sep = "")%>%c("Total",.)%>%
      paste(.," (n=",unwtd_by,")",sep = "")%>%c("Variables",.,"P-value")
    
  }
  
  
  
  
  
  
  
  if(vars_style==1|vars_style==2){
    Tsub1 <- list()
    for (i in vc) {
      newrow <- matrix(c(rep("",subgps+1)),ncol = (subgps+1),nrow = 1,
                       dimnames = list(n_vars[i],names(Tsub[[i]])))
      Tsub1[[i]] <- rbind(newrow,Tsub[[i]])
    }
    
    lev1 <- lev+1
    Tsub_cbind <- data.frame(Tsub1[[1]])
    Variables <- matrix(nrow =sum(lev1) ,ncol = 1,
                        dimnames = list(c(1:sum(lev1)),"Variables"))
    for (i in vc) {
      Tsub_cbind[(cumsum(lev1)[i]-lev1[i]+1):cumsum(lev1)[i],] <- Tsub1[[i]]
      Variables[(cumsum(lev1)[i]-lev1[i]+1):cumsum(lev1)[i]] <-rownames(Tsub1[[i]])
    }
    
    
    pvalue <- matrix(nrow =sum(lev1) ,ncol = 1,
                     dimnames = list(c(1:sum(lev1)),"P-value"))
    for (i in vc) {
      pvalue[cumsum(lev1)[i]-lev1[i]+1] <- summary(svytable(tab_formula[[i]],design))[["statistic"]][["p.value"]]
    }
    
    
  }else if(vars_style==3){
    Tsub_cbind <- data.frame(Tsub[[1]]) 
    Variables <- matrix(nrow =sum(lev) ,ncol = 1,
                        dimnames = list(c(1:sum(lev)),"Variables"))
    
    for (i in vc) {
      Tsub_cbind[(cumsum(lev)[i]-lev[i]+1):cumsum(lev)[i],] <- Tsub[[i]]
      Variables[(cumsum(lev)[i]-lev[i]+1):cumsum(lev)[i]] <- rownames(Tsub[[i]])
    }
    
    pvalue <- matrix(nrow =sum(lev) ,ncol = 1,
                     dimnames = list(c(1:sum(lev)),"P-value"))
    for (i in vc) {
      pvalue[cumsum(lev)[i]-lev[i]+1] <- summary(svytable(tab_formula[[i]],design))[["statistic"]][["p.value"]]
    }
  }
  
  
  if(isFALSE(style_p_round)){
    
    if(digits_p==4){
      pvalue <- if_else(pvalue<0.0001,99,pvalue) %>%
        sprintf("%.4f",.) %>%
        gsub(.,pattern = "99.0000",replacement = "<0.0001")
    }else if(digits_p==3){
      pvalue <- if_else(pvalue<0.001,99,pvalue) %>%
        sprintf("%.3f",.) %>%
        gsub(.,pattern = "99.000",replacement = "<0.001")
    }else if(digits_p==2){
      pvalue <- if_else(pvalue<0.01,99,pvalue) %>%
        sprintf("%.2f",.) %>%
        gsub(.,pattern = "99.00",replacement = "<0.01")
    }
  }else{
    if(digits_p==4){
      pvalue <- if_else(pvalue<0.0001,99,pvalue) %>%
        round(.,digits = 4)%>%
        sprintf("%.4f",.) %>%
        gsub(.,pattern = "99.0000",replacement = "<0.0001")
    }else if(digits_p==3){
      pvalue <- if_else(pvalue<0.001,99,pvalue) %>%
        round(.,digits = 3)%>%
        sprintf("%.3f",.) %>%
        gsub(.,pattern = "99.000",replacement = "<0.001")
    }else if(digits_p==2){
      pvalue <- if_else(pvalue<0.01,99,pvalue) %>%
        round(.,digits = 2)%>%
        sprintf("%.2f",.) %>%
        gsub(.,pattern = "99.00",replacement = "<0.01")
    }
  }
  pvalue <- gsub(pvalue,pattern = "NA",replacement = "")
  
  
  Tsubp <- data.frame(Variables,Tsub_cbind,pvalue)
  
  if(isTRUE(output)){
    names(Tsubp) <- di_names2
    if(is.null(output_name)){
      if(vars_style==1){
        nullname <- paste("Count_style1_sugps",subgps,".csv",sep = "")
        write.csv(Tsubp,nullname)
        names(Tsubp) <- di_names22
        return(Tsubp)
      }else if(vars_style==2){
        nullname <- paste("Count_style2_sugps",subgps,".csv",sep = "")
        write.csv(Tsubp,nullname)
        names(Tsubp) <- di_names22
        return(Tsubp) 
      }else if(vars_style==3){
        nullname <- paste("Count_style3_sugps",subgps,".csv",sep = "")
        write.csv(Tsubp,nullname)
        names(Tsubp) <- di_names22
        return(Tsubp) 
      }
    }else {
      output_name <- paste(output_name,"csv",sep = ".")
      write.csv(Tsubp,output_name)
      names(Tsubp) <- di_names22
      return(Tsubp)
      
    }
  }else {
    names(Tsubp) <- di_names22
    return(Tsubp)
    
  }
  
  
}







#################################################


result_view <- function(x,digits_m=3){   
    a <- summary(x)
    b <- a$coefficients
    c <- cbind(exp(coef(x)),
               if(is.null(x[["call"]])){exp(confint.default(x))}else{
                 exp(confint(x))},
               b[,4])%>%round(.,digits = digits_m)
    dimnames(c)[[2]] <- c("OR", "LL", "UL","P-value")

    return(c)
}#?鿴????


res_view_graphy <- function(fml,data_or_design,
                            ifs=NULL,family=NULL,subset=NULL,
                            digits_p=3,digits_n=2,
                            graphy=T,group.label=NULL,
                            view_row=NULL,view_col=NULL,view=c(1:length(result)),
                            na.rm=F,vars_style=1,vars_prefix=""){  
  if(is.logical(ifs))
    stop("????ifs Ӧ????Ϊ?ַ???ʽ???????????߼???ʽ")
  if(!is.null(view_row)& !is.list(view_row)){stop("view_row Ӧ????list ??ʽ")}
  if(!is.null(view_col)& !is.list(view_col)){stop("view_col Ӧ????list ??ʽ")}
  
  fmlc <- gsub(fml,pattern = "",replacement = "")
  X.fml <- strsplit(fmlc[3]," [+] ")[[1]][1]
  
  XZ.fml <- strsplit(fmlc[3]," [+] ")[[1]]
  XZ.level <- list()
  for (i in XZ.fml) {
    XZ.level[[i]] <- if(is.null(data_or_design[["call"]])){
      if(is.factor(data_or_design[[i]])){levels(data_or_design[[i]])[-1]}
      else if(is.numeric(data_or_design[[i]])){i}
      else{stop("X Z ??��?????? factor ???? numeric")}
    }else{
      XZ.level[[i]] <- if(is.factor(data_or_design[["variables"]][[i]])){levels(data_or_design[["variables"]][[i]])[-1]}
      else if(is.numeric(data_or_design[["variables"]][[i]])){i}
      else{stop("X Z ??��?????? factor ???? numeric")}}}
  vars <- list()
  vars[[1]] <- XZ.level[[1]]
  for (i in c(1:length(XZ.fml))) {
    if(i==1){next}
    vars[[i]] <- c(vars[[i-1]],XZ.level[[i]])}
  vars_simple <- c("Intercept",vars[[length(vars)]])
  
  if(is.null(family)){
    if(isTRUE(is.factor(  if(is.null(data_or_design[["call"]])){data_or_design[[fmlc[2]]]}
                          else{data_or_design[["variables"]][[fmlc[2]]]} ))){
      family<- quasibinomial()
    }else{family= gaussian()}}
  
  
  new_dod <- c(deparse(substitute(data_or_design)),
               if(!is.null(ifs))
                 paste("subset(",deparse(substitute(data_or_design)),
                       ",",ifs,")",sep = "")) 
  print(new_dod)
  ttt <- paste("%.",digits_p,"f",sep = "")
  dg_p <- list(1*(10^-digits_p),99,ttt,sprintf(ttt,99))
  dg_n <- paste("%.",digits_n,"f",sep = "")
  result <-list()
  result.t <- list()
  for (i in c(1:length(new_dod))) {
    if(is.null(data_or_design[["call"]])){
      X.level <- levels(data_or_design[[X.fml]])
      m<-glm(fml,
             eval(parse(text = new_dod[i])),
             family = family)
      result[[i]] <- data.frame(exp(coef(m)),
                                exp(confint.default(m)),
                                (summary(m))[["coefficients"]][,4])
    }else{
      X.level <- levels(data_or_design[["variables"]][[X.fml]])
      m<-svyglm(fml,
                eval(parse(text = new_dod[i])),
                family = family,subset = subset)
      result[[i]] <- data.frame(exp(coef(m)),
                                exp(confint(m)),
                                (summary(m))[["coefficients"]][,4])}
    colnames(result[[i]])<-c("est","lower","upper","pvalue")
    plabel <- case_when(result[[i]][,4]<0.001 ~ "***",
                        result[[i]][,4]<0.01 ~ "**",
                        result[[i]][,4]<0.05 ~ "*",
                        result[[i]][,4]<0.1 ~ ".",
                        TRUE~"")
    result[[i]][,"plabel"] <- plabel
    result[[i]][,"group"] <- i
    if(is.null(group.label)){
      result[[i]][,"group.label"] <- c(deparse(substitute(data_or_design)),ifs)[[i]]
    }else{result[[i]][,"group.label"] <- group.label[i]}
    res_c <- data.frame(Model.n=paste(sprintf(dg_n,result[[i]][,1])," (",
                                      sprintf(dg_n,result[[i]][,2]),"~",
                                      sprintf(dg_n,result[[i]][,3]),")",sep = ""))
    result[[i]][,"Model"]<-res_c
    pvalue <- result[[i]][,4] %>%
      if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
      sprintf(dg_p[[3]],.) %>%
      gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
    result[[i]][,"P-value"] <- pvalue
    result[[i]][,c(1:3)] <- round(result[[i]][,c(1:3)],digits = digits_n)}
  
 
  if(isTRUE(graphy)){
    result.g <- list()
    
    for (i in c(1:length(new_dod))) {
      result.g[[i]] <- data.frame(result[[i]][c(1:length(X.level)),
                                              c(1:3,9,5:7)])%>%
        mutate(Variables=X.level)%>%.[,c(6:8,1:5)]
      rownames(result.g[[i]])[1] <- "Ref"
      result.g[[i]][1,c(4:6)] <- c(1,1,1)}
    result.new <- list()
    result.new[[1]] <- result.g[[1]]
    rownames(result.new[[1]]) <- c(1:nrow(result.new[[1]]))
    for (i in c(1:length(new_dod))){
      if(i==1){next}
      result.new[[i]] <- rbind(result.new[[i-1]],result.g[[i]])
      rownames(result.new[[i]]) <- c(1:nrow(result.new[[i]]))}
    
    oup <- result.new[[length(result.new)]]
    oup$Variables <- factor(oup$Variables,
                            levels = X.level )
    
    
    
  }else {
    
    result.v <- list()
    if(vars_style==1){vars_t <- gsub(fmlc[3],pattern = " [+] ",replacement = "|")}
    
    d <- list()
    e <- list()
    for (i in view) {
      
      result.v[[as.character(i)]] <- result[[i]]%>%
        mutate(Variables=rownames(.))%>%.[,c(10,8,9,5,7,6)]
      
      if(vars_style==1){
        
        result.v[[as.character(i)]][,1] <- paste(vars_prefix,
                                                 gsub(result.v[[as.character(i)]][,1],
                                                      pattern = vars_t,replacement = ""),sep = "")
        d[[i]] <- which(result.v[[as.character(i)]][,1]=="")
        e[[i]] <- paste(vars_prefix,rownames(result[[i]])[d[[i]]],sep = "")
        result.v[[as.character(i)]][,1][d[[i]]] <- e[[i]]}
      rownames(result.v[[as.character(i)]])<- c(1:nrow(result.v[[as.character(i)]]))
      if(vars_style==2){
        result.v[[as.character(i)]][,1]<- paste(vars_prefix,vars_simple,sep = "")
      }}
    
    
    result.new <- list()
    result.new[[1]] <- result.v[[1]][if(is.null(view_row)){c(1:nrow(result.v[[1]]))}else{view_row[[1]]},
                                     if(is.null(view_col)){c(1:ncol(result.v[[1]]))}else{view_col[[1]]}] 
    for (i in c(1:length(result.v))) {
      if(i==1){next}
      result.new[[i]] <- bind_rows(result.new[[i-1]],
                                   result.v[[i]][if(is.null(view_row)){
                                     c(1:nrow(result.v[[i]]))}else{view_row[[i]]},
                                     if(is.null(view_col)){
                                       c(1:ncol(result.v[[i]]))}else{view_col[[i]]}])}
    oup <- result.new[[length(result.new)]]}
  if(isTRUE(na.rm)){
    row_na <-list()
    col_oup <- length(names(oup))
    for (i in c(1:col_oup)) {
      row_na[[i]] <- which(is.na(oup[,i]))
      oup[row_na[[i]],i] <-""}}
  
  print("Ĭ?ϲ鿴ȫģ?ͣ??ɵ??? view view_row view_col ???????鿴?ض?ģ?? ?к??е?????????")
  return(oup)
  
}
#############
flow_counts <- function(data,omitn=NULL,view=NULL,
                        ifs1=NULL,ifs2=NULL,ifs3=NULL,ifs4=NULL){
  data_all <- nrow(data)
  if(!is.null(ifs1)){
    data <- subset(data,ifs1)
    ifs1 <- c(nrow(data),
              data_all-nrow(data),
              data_all)}
  omi_count <- list()
  for (i in omitn) {
    if(is.numeric(omitn)){
      omi_count[[(colnames(data)[i])]] <- c(nrow(na.omit(data[,i])),
                                            nrow(data)-nrow(na.omit(data[,i])),
                                            nrow(data))} 
    if(is.character(omitn)){
      omi_count[[i]] <- c(nrow(na.omit(data[,i])),
                          nrow(data)-nrow(na.omit(data[,i])),
                          nrow(data))}
  }
  omi_all <- c(nrow(na.omit(data[,omitn])),
               nrow(data)-nrow(na.omit(data[,omitn])),
               nrow(data))
  omi_sum <- list(omi_count,omi_all,
                  if(!is.null(ifs1)){ifs1})
  
  
  return(if(!is.null(view)){omi_sum[[view]]}else{omi_sum})
  
}




#res_svy
####################
################
#??????һ??ģ????Crude??????????û?в???
res_glm <- function(fml,data_or_design,cut=NULL,family=NULL,subset=NULL,
                     digits_n=2,digits_p=3,digits_round=F,
                     vars_prefix=NULL,vars_style=2,
                     output=F,output_style="inner",output_name=NULL,
                     view=c(1:(length(cut)+1)),view_row=NULL,
                     RERI=F,RERI_row=NULL,RERI_conf_level=0.95,
                     RERI_p_df="",RERI_index=c("RERI","AP","S")){
  cut1 <- c(cut,"                ")
  fmlt <- list()
  a <-list()
  for(i in cut1){
    a <- strsplit(as.character(fml),paste("[+] ",i,sep = ""))
    fmlt[[i]] <-formula(paste(a[[2]],a[[1]],a[[3]][1],sep=""))}
  Y.fml<-a[[2]]
  X.fml<-strsplit(a[[3]]," [+] ")
  
  mc <- c(0:(length(cut1)-1))
  tail <-vector()
  fml.char<-vector()
  for (i in (mc+1)) {
    fml.char[i]<-as.character(fmlt[[i]])[3]%>%
      gsub(.,pattern =" [+] ",replacement = ", " )
    if(i==1){next}
    tail[i]<-paste("Model ",i-1,
                   ": Adjust for ",
                   ifelse(i>2,paste("the variables in ","Model ",
                                    i-2,"pulse "),sep = ""),
                   gsub(fml.char[i],
                        pattern =paste(fml.char[i-1],",",sep = ""),
                        replacement = "" ),".",sep = "")}
  
  
  if(is.null(family)){
    if(isTRUE(is.factor(  if(is.null(data_or_design[["call"]])){data_or_design[[a[[2]]]]}
                          else{data_or_design[["variables"]][[a[[2]]]]} ))){
      family<- quasibinomial()
      style_OUP <- "OR"
      refob <- "1 (Ref)"
    }else{
      family= gaussian()
      style_OUP <- "??"
      refob <- "0 (Ref)"}}
  
  
  result <-list()
  res_reri <- list()
  covmat <- list()
  reri_df <- list()
  tpb <- progress::progress_bar$new(total = length(cut1),clear = F,width = 40,
                                    format = "[:bar](:spin) :current models :percent :elapsed" )
  for (i in cut1) {
    tpb$tick()
    if(is.null(data_or_design[["call"]])){
      m<-glm(fmlt[[i]],data_or_design,family = family,subset = subset)
      result[[i]] <- data.frame(exp(coef(m)),exp(confint.default(m)),
                                (summary(m))[["coefficients"]][,4])%>%.[-1,]
      reri_df[[i]]<-df.residual(m)
    }else{
      m<-svyglm(fmlt[[i]],data_or_design,family = family)
      result[[i]] <- data.frame(exp(coef(m)),
                                exp(confint(m)),
                                (summary(m))[["coefficients"]][,4])%>%.[-1,]}
    colnames(result[[i]])<-c("est","lower","upper","pvalue")
    if(isTRUE(RERI)){
      res_reri[[i]] <- coefficients(summary(m))
      covmat[[i]]<- vcov(m)
      reri_df[[i]]<-df.residual(m)}}
  
  
  dg_n <- paste("%.",digits_n,"f",sep = "")
  ttt <- paste("%.",digits_p,"f",sep = "")
  dg_p <- list(1*(10^-digits_p),99,ttt,sprintf(ttt,99))
  
  cln <- list()
  ref <-list()
  for (i in mc) {
    cln[[(i+1)]]<-c(paste("Model ",i,"\n",
                          style_OUP," (95CI)",sep = ""),"P-value")
    ref[[i+1]] <- matrix(c(refob,""),ncol = 2,nrow = 1,
                         dimnames = list("RefGroupName",cln[[i+1]]))}
  
  
  
  if(isTRUE(RERI)){
    if(family[["link"]]!="logit")
      stop("Error: ??ʱֻ֧??logisticģ?ͣ?")
    if(is.null(RERI_row)){
      RERI_row<-vector()
      for (i in (mc+1)) {
        RERI_row[i]<-length(rownames(res_reri[[i]]))}}
    RERI_index <- toupper(RERI_index)
    
    N. <- 1 - ((1 - RERI_conf_level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    h1<- list()
    h2<- list()
    h3<- list()
    coe<-list()
    theta1<-list()
    theta2<-list()
    theta3<-list()
    theta1.se<-list()
    theta2.se<-list()
    theta3.se<-list()
    reri.var<-list()
    reri.p<-list()
    reri.l<-list()
    reri.u<-list()
    est.se<-list()
    est.z<-list()
    est.p<-list()
    reri<-list()
    
    for (i in (mc+1)) {
      coe[[i]] <- if(is.numeric(RERI_row)){c(2,3,RERI_row[i])
      }else if(is.list(RERI_row)){RERI_row[[i]]}
      theta1[[i]]<-res_reri[[i]][coe[[i]][1],1]
      theta2[[i]]<-res_reri[[i]][coe[[i]][2],1]
      theta3[[i]]<-res_reri[[i]][coe[[i]][3],1]
      theta1.se[[i]]<-res_reri[[i]][coe[[i]][1],2]
      theta2.se[[i]]<-res_reri[[i]][coe[[i]][2],2]
      theta3.se[[i]]<-res_reri[[i]][coe[[i]][3],2]
      
      h1[[i]]<-c(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - exp(theta1[[i]]),
                 ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 
                     exp(theta1[[i]]))/(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))) - 
                   ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - exp(theta1[[i]]) - 
                       exp(theta2[[i]]) + 1)/(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))),
                 (((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))/
                     (exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 1)) -  
                    (exp(theta1[[i]])/(exp(theta1[[i]]) + exp(theta2[[i]]) - 2))))
      h2[[i]]<-c(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - exp(theta2[[i]]),
                 ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 
                     exp(theta2[[i]]))/(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))) - 
                   ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 
                       exp(theta1[[i]]) - exp(theta2[[i]]) + 1)/(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))),
                 ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]))/(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 1)) - 
                   (exp(theta2[[i]])/(exp(theta1[[i]]) + exp(theta2[[i]]) - 2)))
      h3[[i]]<-c(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) ,
                 1 - ((exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - exp(theta1[[i]]) - 
                         exp(theta2[[i]]) + 1)/exp(theta1[[i]] + theta2[[i]] + theta3[[i]])),
                 exp(theta1[[i]] + theta2[[i]] + theta3[[i]])/(exp(theta1[[i]] + theta2[[i]] + 
                                                                     theta3[[i]]) - 1))
      names(h1[[i]])<- c("RERI","AP","S")
      names(h2[[i]])<- c("RERI","AP","S")
      names(h3[[i]])<- c("RERI","AP","S")
      
      
      reri.var[[i]]<-numeric()
      reri.p[[i]]<-numeric()
      reri.l[[i]]<-numeric()
      reri.u[[i]]<-numeric()
      est.se[[i]]<-numeric()
      est.z[[i]]<-numeric()
      est.p[[i]]<-numeric()
      reri[[i]]<-data.frame()
      for (k in RERI_index) {
        reri.var[[i]][k] <- 
          (h1[[i]][k]^2 * theta1.se[[i]]^2) + 
          (h2[[i]][k]^2 * theta2.se[[i]]^2) +
          (h3[[i]][k]^2 * theta3.se[[i]]^2) + 
          (2 * h1[[i]][k] * h2[[i]][k] * covmat[[i]][coe[[i]][1], coe[[i]][2]]) + 
          (2 * h1[[i]][k] * h3[[i]][k] * covmat[[i]][coe[[i]][1], coe[[i]][3]]) + 
          (2 * h2[[i]][k] * h3[[i]][k] * covmat[[i]][coe[[i]][2], coe[[i]][3]])
        reri.p[[i]][k] <- if(k=="RERI"){(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 
                                           exp(theta1[[i]]) - 
                                           exp(theta2[[i]]) + 1)}
        else if(k=="AP"){(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - exp(theta1[[i]]) - 
                            exp(theta2[[i]]) + 1)/exp(theta1[[i]] + theta2[[i]] + theta3[[i]])}
        else if(k=="S"){(exp(theta1[[i]] + theta2[[i]] + theta3[[i]]) - 1)/(exp(theta1[[i]]) + 
                                                                              exp(theta2[[i]]) - 2)}
        
        reri.l[[i]][k] <- if(k=="RERI"|k=="AP"){reri.p[[i]][k] - (z * sqrt(reri.var[[i]][k]))}
        else if(k=="S"){reri.p[[i]][k]/exp(z * sqrt(reri.var[[i]][k]))}
        
        reri.u[[i]][k] <- if(k=="RERI"|k=="AP"){reri.p[[i]][k] + (z * sqrt(reri.var[[i]][k]))}
        else if(k=="S"){reri.p[[i]][k] * exp(z * sqrt(reri.var[[i]][k]))}
        
        est.se[[i]][k] <- if(k=="RERI"|k=="AP"){(reri.u[[i]][k]-reri.l[[i]][k])/(2*z)}
        else if(k=="S"){(log(reri.u[[i]][k]) - log(reri.l[[i]][k]))/(2*z)}
        
        est.z[[i]][k] <- if(k=="RERI"|k=="AP"){reri.p[[i]][k]/est.se[[i]][k]}
        else if(k=="S"){abs(log(reri.p[[i]][k])/est.se[[i]][k])}
        
        est.p[[i]][k] <- if(is.null(RERI_p_df)){exp(-0.717*est.z[[i]][k]-0.416*(est.z[[i]][k]^2))}
        else {2*pt(abs(
          if(k=="RERI"|k=="AP"){reri.p[[i]][k]/sqrt(reri.var[[i]][k])}
          else if(k=="S"){log(reri.p[[i]][k])/sqrt(reri.var[[i]][k])}),
          df=if(class(RERI_p_df)=="numeric"){RERI_p_df}
          else{reri_df[[i]]},
          lower.tail = F)}
        reri[[i]][k,c(1:4)] <- data.frame(est = reri.p[[i]][k], 
                                          lower = reri.l[[i]][k], 
                                          upper = reri.u[[i]][k],
                                          pvalue = est.p[[i]][k])}
      result[[i]]<-rbind(result[[i]],reri[[i]])
    }
  }
  
  
  result_new <- list()
  pvalue <- list()
  for (i in (mc+1)) {
    if(isFALSE(digits_round)){
      pvalue[[i]] <- result[[i]][,4] %>%
        if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
        sprintf(dg_p[[3]],.) %>%
        gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep="")) 
    }else{
      pvalue[[i]] <- result[[i]][,4] %>%
        round(.,digits = digits_p)%>%
        if_else(.<dg_p[[1]],dg_p[[2]],.) %>%
        sprintf(dg_p[[3]],.) %>%
        gsub(.,pattern = dg_p[[4]],replacement = paste("<",sprintf(ttt,dg_p[[1]]),sep=""))}
    
    
    result_new[[i]]<- data.frame(paste(sprintf(dg_n,result[[i]][,1])," (",
                                       sprintf(dg_n,result[[i]][,2]),"~",
                                       sprintf(dg_n,result[[i]][,3]),")",sep = ""),
                                 pvalue[[i]])
    rownames(result_new[[i]])<- rownames(result[[i]])
    colnames(result_new[[i]]) <- cln[[i]]
    result_new[[i]] <-rbind(ref[[i]],result_new[[i]])%>%
      mutate(Variables=rownames(.))%>%.[,c(3,1,2)]}
  
  
  if(vars_style==1){vars_t <- gsub(a[[3]][1],pattern = " [+] ",replacement = "|")}
  vars_prefix <- paste(vars_prefix,"",sep = "")
  d <- list()
  e <- list()
  if(vars_style==1){
    for (i in (mc+1)) {
      result_new[[i]][,1] <- gsub(rownames(result_new[[i]]),
                                  pattern = vars_t,replacement = "")
      d[[i]] <- which(result_new[[i]][,1]=="")
      e[[i]] <- rownames(result_new[[i]])[d[[i]]]
      result_new[[i]][,1][d[[i]]] <- e[[i]]}
  }
  
  
  
  if(isTRUE(RERI)){
    reri_new<-data.frame()
    rwn.reri<-vector()
    mInt.name <- rownames(result_new[[1]])[nrow(result_new[[1]])-length(RERI_index)]
    mInt.name.s <-gsub(mInt.name,
                       pattern = gsub(a[[3]][1],pattern = " [+] ",replacement = "|"),
                       replacement = "")
    for (i in (mc+1)) {
      rwn.reri[i]<-paste("Model",i-1)
      for (k in c(mInt.name,RERI_index)) {
        k.s<-ifelse(k==mInt.name,mInt.name.s,k)
        k.p<-paste(k.s,".p",sep = "")
        
        reri_new[1,c(k.s,k.p)]<- data.frame(case_when(k==mInt.name ~ "Multiplicative interaction",
                                                      k=="RERI" ~ "Addictive interaction",  
                                                      TRUE ~ ""),"",row.names = "Models")
        reri_new[2,c(k.s,k.p)]<- c(ifelse(k==mInt.name,
                                          "OR\n(95 CI)",
                                          paste(k,"\n(95 CI)",sep = "")),"P-value")
        
        reri_new[i+2,c(k.s,k.p)]<- data.frame(result_new[[i]][k,2],
                                              result_new[[i]][k,3])}
      rownames(reri_new)[i+2]<-rwn.reri[i]}
    reri_new <-cbind(reri_new[,c(1:2)],
                     rep("",nrow(reri_new)),
                     reri_new[,-c(1:2)])
    colnames(reri_new)<-rep("",ncol(reri_new))
    
    
    
    mInt.name.v<-strsplit(mInt.name,
                          gsub(mInt.name.s,
                               pattern = ":",
                               replacement = ":|"))[[1]]
    AB.contras <- c(ifelse(is.null(data_or_design[["call"]]),
                           (levels(data_or_design[[mInt.name.v[1]]]))[1],
                           (levels(data_or_design[["variables"]][[mInt.name.v[1]]]))[1]),
                    ifelse(is.null(data_or_design[["call"]]),
                           (levels(data_or_design[[mInt.name.v[2]]])[1]),
                           (levels(data_or_design[["variables"]][[mInt.name.v[2]]]))[1]))
    AB.exposure <- strsplit(mInt.name.s,":")[[1]]
    reri.tail1 <- paste(tail[view],collapse = "\n")%>%
      gsub(.,pattern = "NA\n",replacement = "")
    reri.tail2 <- vector()
    for (i in RERI_index) {
      reri.tail2[i]<-paste(i,case_when(i=="RERI" ~ "relative excess risk due to interaction.",
                                       i=="AP" ~ "attributable proportion due to interaction.",
                                       i=="S" ~ "the synergy index."),sep = ", ")}
    reri.tail2 <- paste("Abbreviations: ",
                        paste(reri.tail2,collapse = " "),
                        sep = "")
    reri.tail3 <-paste("Notes: ","Multiplicative interaction was evaluated using odds ratios for the product term between ",mInt.name.v[1]," (",AB.exposure[1]," vs ",AB.contras[1],")"," and ",mInt.name.v[2]," (",AB.exposure[2]," vs ",AB.contras[2],")"," and the multiplicative interaction was statistically significant when its confidence interval did not include 1. Additive interaction was evaluated using relative excess risk due to interaction (RERI) between ",mInt.name.v[1]," (",AB.exposure[1]," vs ",AB.contras[1],")"," and ",mInt.name.v[2]," (",AB.exposure[2]," vs ",AB.contras[2],")"," and the additive interaction was statistically significant when its confidence interval did not include 0.",
                       sep = "")
    reri.tail <- paste(reri.tail1,
                       reri.tail2,
                       reri.tail3,sep = "\n")
    reri.title <- paste("Table n Interaction analysis for ",
                        mInt.name.v[1]," and ",
                        mInt.name.v[2])
    
    
    reri_oup <- rbind(rep("",nrow(reri_new)),
                      reri_new[c(1,2),],
                      reri_new[rwn.reri[view],],
                      rep("",nrow(reri_new)))
    rownames(reri_oup)[c(1,3,nrow(reri_oup))] <- c(reri.title,"",reri.tail)
  }
  
  
  rc <- list()
  print_view<- list()
  #view_t <- paste("model",view,sep = " ")
  for (i in view) {
    rc[[as.character(i)]] <- length(rownames(result_new[[i]]))
    rownames(result_new[[i]])<- c(1:rc[[as.character(i)]])
    print_view[[as.character(i)]]<-result_new[[i]][
      if(is.null(view_row)){c(1:rc[[as.character(i)]])}
      else if(is.list(view_row)){view_row[[which(view==i)]]}
      else if(is.numeric(view_row)){view_row},]}
  if(isFALSE(output)){
    print(print_view)
    print("Ŀǰ?鿴ģʽ, ??ͨ??view and view_row????ѡ??????ģ?????е?????, ????????????????output!")
  }else{
    
    full_inner <-list()
    full_inner[[1]]<-print_view[[1]]
    
    for (i in c(1:length(print_view))) {
      if(i==1){next}
      full_inner[[i]]<-if(output_style=="full"){
        full_join(full_inner[[i-1]],print_view[[i]],
                  by="Variables")}
      else if(output_style=="inner"){
        inner_join(full_inner[[i-1]],print_view[[i]],
                   by="Variables")}}
    
    oup<- data.frame(full_inner[[length(print_view)]])
    for (i in c(1:length(print_view))) {
      colnames(oup)[c((2*i):(2*i+1))] <- colnames(print_view[[i]])[c(2,3)]}
    row_na <-list()
    col_oup <- length(names(oup))
    for (i in c(1:col_oup)) {
      row_na[[i]] <- which(is.na(oup[,i]))
      oup[row_na[[i]],i] <-""}
    
    oup.title1<-paste("Table n The association between ",
                      X.fml[[1]][1]," and risk of ",Y.fml," incidence by Z",sep = "")
    oup.title2<-colnames(oup)
    oup.tail<-paste(c(tail[view],"Abbreviations: "),collapse = "\n")%>%
      gsub(.,pattern = "NA\n",replacement = "")
    
    oup.new<-rbind(c(oup.title1,rep("",ncol(oup)-1)),
                   oup.title2,
                   oup,
                   c(oup.tail,rep("",ncol(oup)-1)))
    colnames(oup.new)<-rep("",ncol(oup.new)) 
    
    if(is.null(output_name)){
      nullname <- paste("Model",
                        paste(view,collapse = ""),
                        output_style,
                        style_OUP,"csv",sep = ".")
      write.csv(oup.new,nullname)
      if(isTRUE(RERI)){
        nullname.reri <- paste("Model",
                               paste(view,collapse = ""),
                               paste(RERI_index,collapse = "."),
                               "csv",sep = ".")
        write.csv(reri_oup,nullname.reri)}
    }else{
      write.csv(oup.new,paste(output_name,style_OUP,"csv",sep = "."))
      if(isTRUE(RERI)){
        write.csv(reri_oup,paste(output_name,"INT","csv",sep = "."))}} 
    return(oup) 
  }
  
  
}









