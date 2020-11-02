
### Intervention 4: Education ###

# keep a random group of women
# in education up to age 22 so that at least
# 50% of women are in education up to that age

fill.dat <- read.csv(file='C:/...fertdat.csv',head=T)
names(fill.dat)

fill.dat$X <- NULL
fill.dat$sex <- NULL
str(fill.dat)
fill.dat$bcsid <- as.character(fill.dat$bcsid)
fill.dat$bcsid.part <- as.character(fill.dat$bcsid.part)
# the other ones labeled as factors don't matter
# since we don't use those (instead we use variables
# that were derived from them in the data handling file)

# only include individuals for whom we have data in 1986
id1986 <- unique(fill.dat$bcsid[fill.dat$year==1986])
fill.dat <- fill.dat[which(fill.dat$bcsid %in% id1986),]

# might need a lag function
lag1.func <- function(variabletolag, idvariable) {
  
  c(NA,ifelse(idvariable[2:length(idvariable)] == idvariable[1:(length(idvariable)-1)],
              variabletolag[1:(length(idvariable)-1)],NA))
  
}

# remove individuals for which we don't have agemother info
fill.dat <- fill.dat[!is.na(fill.dat$agemother),]

# to improve predictions
fill.dat$totalbirthcat0 <- ifelse(fill.dat$totalbirth==0,1,0)
fill.dat$totalbirthcat1 <- ifelse(fill.dat$totalbirth==1,1,0)
fill.dat$totalbirthcat2 <- ifelse(fill.dat$totalbirth==2,1,0)
fill.dat$totalbirthcat3 <- ifelse(fill.dat$totalbirth==3,1,0)
fill.dat$totalbirthcat4pl <- ifelse(fill.dat$totalbirth>=4,1,0)

fill.dat$l.totalbirthcat0 <- lag1.func(fill.dat$totalbirthcat0,fill.dat$bcsid)
fill.dat$l.totalbirthcat1 <- lag1.func(fill.dat$totalbirthcat1,fill.dat$bcsid)
fill.dat$l.totalbirthcat2 <- lag1.func(fill.dat$totalbirthcat2,fill.dat$bcsid)
fill.dat$l.totalbirthcat3 <- lag1.func(fill.dat$totalbirthcat3,fill.dat$bcsid)
fill.dat$l.totalbirthcat4pl <- lag1.func(fill.dat$totalbirthcat4pl,fill.dat$bcsid)

# determine chance at 2nd and 3rd birth
# if you have a single birth
table(fill.dat$pliv[fill.dat$birth==1])
birthprobs <- table(fill.dat$pliv[fill.dat$birth==1])/sum(table(fill.dat$pliv[fill.dat$birth==1]))
# among those who get a birth
# there's a 98% chance of it being a single child
# a 2% chance of a twin
# and a very small chance of it being a triplet
twinprob <- birthprobs[2]
tripprob <- birthprobs[3]

# make group indicator
# this is used in the bs to compare natural course vs intervention
fill.dat$group <- NA
output.dat <- NULL

fill.sample.temp <- fill.sample.2.temp <- NULL
## Start for-loop here: I intend to bootstrap 150 times ##
bssize <- 999

output.dat <- rep(NA,bssize*4*(38-15)) # row, col, 3rd dim
dim(output.dat) <- c(bssize,4,38-15)

babytable <- babytable.2 <- rep(NA,bssize*15*(38-15))
dim(babytable) <- c(bssize,15,38-15)
dim(babytable.2) <- c(bssize,15,38-15)

# predict functions needed
multinomial.predict.5var <- function(predict.x1,predict.x2,predict.x3,predict.x4,
                                     dataset) {
  
  # predict probabilities for each of the 3 outcome possibilities
  x1 <- predict(predict.x1,dataset,type='response')
  x2 <- predict(predict.x2,dataset,type='response')
  x3 <- predict(predict.x3,dataset,type='response')
  x4 <- predict(predict.x4,dataset,type='response')
  # x5 does not need to be modelled
  
  # draw random variables
  decider.x1 <- runif(length(x1),0,1)
  decider.x2 <- runif(length(x2),0,1)
  decider.x3 <- runif(length(x3),0,1)
  decider.x4 <- runif(length(x4),0,1)
  # decider.x5 not needed
  
  # determine category
  x1c <- ifelse(decider.x1 <= x1,1,0)
  x2c <- ifelse(decider.x2 <= x2 & x1c==0,1,0)
  x3c <- ifelse(decider.x3 <= x3 & x1c==0 & x2c==0,1,0)
  x4c <- ifelse(decider.x4 <= x4 & x1c==0 & x2c==0 & x3c==0,1,0)
  x5c <- ifelse(x1c==0 & x2c==0 & x3c==0 & x4c==0,1,0)
  
  return(cbind(x1c,x2c,x3c,x4c,x5c))
}

multinomial.predict.3var <- function(predict.x1,predict.x2,
                                     dataset) {
  
  # predict probabilities for each of the 3 outcome possibilities
  x1 <- predict(predict.x1,dataset,type='response')
  x2 <- predict(predict.x2,dataset,type='response')
  # x3 does not need to be modelled
  
  # draw random variables
  decider.x1 <- runif(length(x1),0,1)
  decider.x2 <- runif(length(x2),0,1)
  # decided.x3 not needed
  
  # determine category
  x1c <- ifelse(decider.x1 <= x1,1,0)
  x2c <- ifelse(decider.x2 <= x2 & x1c==0,1,0)
  x3c <- ifelse(x1c==0 & x2c==0,1,0)
  
  return(cbind(x1c,x2c,x3c))
}

multinomial.predict.2var <- function(predict.x1,
                                     dataset) {
  x1 <- predict(predict.x1,dataset,type='response')
  
  # determine n
  n2 <- length(x1)
  
  # draw random variables
  decider <- runif(n2,0,1)
  
  # determine category
  x1c <- ifelse(decider <= x1,1,0)
  x2c <- ifelse(decider > x1,1,0)
  
  return(cbind(x1c,x2c))
} # same as ordinary binomial but more easily provides some extra info if needed

binomial.predict <- function(predict.x1,dataset) {
  
  x1 <- predict(predict.x1,dataset,type='response')
  x1c <- rbinom(length(x1),1,x1)
  
  return(x1c)
}

long.sample <- function(originaldata, originaldataid) {
  # select a bunch of IDs
  IDs <- unique(originaldataid)
  y <- sample(IDs,length(IDs),replace=T)
  z <- table(table(y))
  
  # from there, select a group once
  selectID <- sample(IDs,size=z[1],replace=F)
  newdata <- originaldata[which(originaldataid %in% selectID),]
  
  if(length(z) > 1) {
    
    for(i in 2:length(z)) {
      
      # select a new group of IDs that was not yet selected
      IDs2 <- setdiff(IDs,selectID)
      
      # from there, randomly select a group of people of the right size
      selectID2 <- sample(IDs2,size=z[i],replace=F)
      selectID <- c(selectID,selectID2) # so we don't re-select the newly
      # selected people either
      
      for(j in 1:i) {
        
        # copy the new dataset i number of times
        newdata <- rbind(newdata,originaldata[which(originaldataid %in% selectID2),])
        
      }
      
    }
    
    return(newdata)
  }
}

# make model formulas
formula.outcome <- c("outcome ~ 
                     # constants #
                     inc80.med + inc80.high + # low as ref
                     
                     region.scot + region.wales + # london as ref
                     region.north + region.south +
                     region.midl +
                     
                     agemoth1522 + agemoth30pl + # agemoth 23-29 as ref
                     
                     siblingsD + # make this categorical?                    
                     
                     # time varying #
                     age1522 + age30pl + # age 23-29 as ref
                     
                     l.birth + 
                     l.totalbirthcat0 + l.totalbirthcat1 +
                     l.totalbirthcat2 +
                     
                     (age1522 + age30pl)*
                     (l.partner.cohab + l.partner.married) + # single as ref
                     
                     l.totalbirthcat0*l.totaledu +
                     
                     l.totalbirthcat0*(l.partner.cohab + l.partner.married) +
                     
                     l.totalbirthcat0*(l.job.full + l.job.part + 
                     l.job.edu + l.job.perdis) +
                     
                     l.jobses.low *
                     (l.job.full + l.job.part) - l.jobses.low +
                     
                     l.jobses.high *
                     (l.job.part) - l.jobses.high")

formula.birth <- as.formula(sub('outcome','birth',formula.outcome))

formula.job.full <- as.formula(sub('outcome','job.full',formula.outcome))
formula.job.part <- as.formula(sub('outcome','job.part',formula.outcome))
formula.job.less <- as.formula(sub('outcome','job.less',formula.outcome))
formula.job.edu <- as.formula(sub('outcome','job.edu',formula.outcome))
formula.job.perdis <- as.formula(sub('outcome','job.perdis',formula.outcome))

formula.jobses.high <- as.formula(sub('outcome','jobses.high',formula.outcome))
formula.jobses.low <- as.formula(sub('outcome','jobses.low',formula.outcome))
formula.jobses.nojob <- as.formula(sub('outcome','jobses.nojob',formula.outcome))

formula.partner.married <- as.formula(sub('outcome','partner.married',formula.outcome))
formula.partner.cohab <- as.formula(sub('outcome','partner.cohab',formula.outcome))
formula.partner.single <- as.formula(sub('outcome','partner.single',formula.outcome))

# save column location info
col.index.from <- NULL
col.index.from[1] <- grep(paste0('^','birth','$'), colnames(fill.dat))
col.index.from[2] <- grep(paste0('^','totalbirth','$'), colnames(fill.dat))
col.index.from[3] <- grep(paste0('^','totalbirthcat0','$'), colnames(fill.dat))
col.index.from[4] <- grep(paste0('^','totalbirthcat1','$'), colnames(fill.dat))
col.index.from[5] <- grep(paste0('^','totalbirthcat2','$'), colnames(fill.dat))
col.index.from[6] <- grep(paste0('^','totalbirthcat3','$'), colnames(fill.dat))
col.index.from[7] <- grep(paste0('^','totalbirthcat4pl','$'), colnames(fill.dat))
col.index.from[8] <- grep(paste0('^','job.full','$'), colnames(fill.dat))
col.index.from[9] <- grep(paste0('^','job.part','$'), colnames(fill.dat))
col.index.from[10] <- grep(paste0('^','job.less','$'), colnames(fill.dat))
col.index.from[11] <- grep(paste0('^','job.edu','$'), colnames(fill.dat))
col.index.from[12] <- grep(paste0('^','job.perdis','$'), colnames(fill.dat))
col.index.from[13] <- grep(paste0('^','totaledu','$'), colnames(fill.dat))
col.index.from[14] <- grep(paste0('^','jobses.high','$'), colnames(fill.dat))
col.index.from[15] <- grep(paste0('^','jobses.low','$'), colnames(fill.dat))
col.index.from[16] <- grep(paste0('^','jobses.nojob','$'), colnames(fill.dat))
col.index.from[17] <- grep(paste0('^','partner.married','$'), colnames(fill.dat))
col.index.from[18] <- grep(paste0('^','partner.cohab','$'), colnames(fill.dat))
col.index.from[19] <- grep(paste0('^','partner.single','$'), colnames(fill.dat))

col.index.to <- NULL
col.index.to[1] <- grep(paste0('^','l.birth','$'), colnames(fill.dat))
col.index.to[2] <- grep(paste0('^','l.totalbirth','$'), colnames(fill.dat))
col.index.to[3] <- grep(paste0('^','l.totalbirthcat0','$'), colnames(fill.dat))
col.index.to[4] <- grep(paste0('^','l.totalbirthcat1','$'), colnames(fill.dat))
col.index.to[5] <- grep(paste0('^','l.totalbirthcat2','$'), colnames(fill.dat))
col.index.to[6] <- grep(paste0('^','l.totalbirthcat3','$'), colnames(fill.dat))
col.index.to[7] <- grep(paste0('^','l.totalbirthcat4pl','$'), colnames(fill.dat))
col.index.to[8] <- grep(paste0('^','l.job.full','$'), colnames(fill.dat))
col.index.to[9] <- grep(paste0('^','l.job.part','$'), colnames(fill.dat))
col.index.to[10] <- grep(paste0('^','l.job.less','$'), colnames(fill.dat))
col.index.to[11] <- grep(paste0('^','l.job.edu','$'), colnames(fill.dat))
col.index.to[12] <- grep(paste0('^','l.job.perdis','$'), colnames(fill.dat))
col.index.to[13] <- grep(paste0('^','l.totaledu','$'), colnames(fill.dat))
col.index.to[14] <- grep(paste0('^','l.jobses.high','$'), colnames(fill.dat))
col.index.to[15] <- grep(paste0('^','l.jobses.low','$'), colnames(fill.dat))
col.index.to[16] <- grep(paste0('^','l.jobses.nojob','$'), colnames(fill.dat))
col.index.to[17] <- grep(paste0('^','l.partner.married','$'), colnames(fill.dat))
col.index.to[18] <- grep(paste0('^','l.partner.cohab','$'), colnames(fill.dat))
col.index.to[19] <- grep(paste0('^','l.partner.single','$'), colnames(fill.dat))

# the order of the above variables in the index.from and index.to
# must be the same!
# and of course, the length of both vectors must also be the same
length(col.index.from)
length(col.index.to)

t1 <- Sys.time()

for(bs in 1:bssize) {
  
  # sample individuals from fill.dat
  fill.sample <- long.sample(fill.dat,fill.dat$bcsid)
  
  # (re)fit models to fill.sample
  # bsf = bootstrap fit
  # estimate relations
  fit.bsf.birth <- glm(formula.birth, family=binomial, data=fill.sample)
  
  fit.bsf.job.full <- glm(formula.job.full, family=binomial, data=fill.sample)
  fit.bsf.job.part <- glm(formula.job.part, family=binomial, data=fill.sample,subset=fill.sample$job.full==0)
  fit.bsf.job.less <- glm(formula.job.less, family=binomial, data=fill.sample,subset=fill.sample$job.full==0 & fill.sample$job.part==0)
  fit.bsf.job.edu <-  glm(formula.job.edu,  family=binomial, data=fill.sample,subset=fill.sample$job.full==0 & fill.sample$job.part==0 & fill.sample$job.less==0)
  # fit.bsf.job.perdis <- glm(formula.job.perdis, family=binomial, data=fill.sample) No longer needed

  fit.bsf.jobses.high <- glm(formula.jobses.high, family=binomial, data=fill.sample)
  fit.bsf.jobses.low <- glm(formula.jobses.low, family=binomial, data=fill.sample,subset=fill.sample$jobses.high==0)
  # fit.bsf.jobses.nojob <- glm(formula.jobses.nojob, family=binomial, data=fill.sample) No longer needed
  
  fit.bsf.partner.married <- glm(formula.partner.married, family=binomial, data=fill.sample)
  fit.bsf.partner.cohab <- glm(formula.partner.cohab, family=binomial, data=fill.sample,subset=fill.sample$partner.married==0)
  # fit.bsf.partner.single <- glm(formula.partner.single, family=binomial, data=fill.sample) No longer needed
  
  # take individuals at time 0
  fill.sample <- fill.sample[fill.sample$year==1986,]
  
  # give each individual a new ID, since
  # otherwise the ordering below will go wrong when
  # individuals are ordered by ID and time (since
  # due to resampling with replacement, multiple individuals
  # can have the same ID)
  fill.sample$idnr <- 1:length(fill.sample$bcsid)
  
  # and the same file, but then for the intervention loop
  fill.sample.2 <- fill.sample
  
  fill.sample$group <- 0
  ## NATURAL LOOP ##
  # start a loop that moves through the follow-up time units
  
  for(t in 1987:2008) {
    
    fill.sample.temp <- fill.sample[fill.sample$year==(t-1),]
    fill.sample.temp$year <- fill.sample.temp$year+1
    fill.sample.temp$age <- fill.sample.temp$age+1
    fill.sample.temp$age1522 <- ifelse(fill.sample.temp$age < 23,1,0)
    fill.sample.temp$age2329 <- ifelse(fill.sample.temp$age > 22 & fill.sample.temp$age < 30,1,0)
    fill.sample.temp$age30pl <- ifelse(fill.sample.temp$age > 30,1,0)
    
    # lag values: since the new values for t have not yet been produced
    # I can actually take the column information from a row at t
    # and put it in the same row at t but then in a column meant for
    # lagged values
    # the first are the 'from' columns, and the second the 'to' columns
    # so: take values from the the 'from' column and put them in the 'to' column
    fill.sample.temp[,col.index.to] <- fill.sample.temp[,col.index.from]
    
    # put together with entire dataset
    fill.sample <- rbind(fill.sample,fill.sample.temp)
    rm(fill.sample.temp)
    
    # order by ID variable (needed for lags below)
    fill.sample <- fill.sample[order(fill.sample$idnr,fill.sample$year),]
    
    ## predict functions here
    
    # predict birth
    fill.sample$birth[fill.sample$year==t] <- binomial.predict(fit.bsf.birth, fill.sample[fill.sample$year==t,])
    
    # stochastic extra birth
    nb <- length(fill.sample$birth[fill.sample$year==t])
    fill.sample$birth2 <- fill.sample$birth3 <- NULL
    fill.sample$birth2[fill.sample$year==t] <- rbinom(nb,1,twinprob)
    fill.sample$birth3[fill.sample$year==t] <- rbinom(nb,1,tripprob)
    # everyone gets a chance, but only those who actually have a single birth
    # get the 2nd and 3rd births counted when I update
    
    # update totalbirth (incl. multiple births)
    fill.sample$totalbirth[fill.sample$year==t] <- fill.sample$l.totalbirth[fill.sample$year==t] +
      fill.sample$birth[fill.sample$year==t] +
      # including multiple birth probs
      fill.sample$birth[fill.sample$year==t]*fill.sample$birth2[fill.sample$year==t] +
      fill.sample$birth[fill.sample$year==t]*fill.sample$birth3[fill.sample$year==t]
    
    # get rid of those who still create NA
    # because some predictor is missing
    fill.sample <- fill.sample[!is.na(fill.sample$birth),]
    
    # update totalbirthcat
    fill.sample$totalbirthcat0[fill.sample$year==t] <- ifelse(fill.sample$totalbirth[fill.sample$year==t]==0,1,0)
    fill.sample$totalbirthcat1[fill.sample$year==t] <- ifelse(fill.sample$totalbirth[fill.sample$year==t]==1,1,0)
    fill.sample$totalbirthcat2[fill.sample$year==t] <- ifelse(fill.sample$totalbirth[fill.sample$year==t]==2,1,0)
    
    # predict job
    x <- multinomial.predict.5var(fit.bsf.job.full,fit.bsf.job.part,fit.bsf.job.less,
                                  fit.bsf.job.edu,
                                  fill.sample[fill.sample$year==t,])
    fill.sample$job.full[fill.sample$year==t] <- x[,1]
    fill.sample$job.part[fill.sample$year==t] <- x[,2]
    fill.sample$job.less[fill.sample$year==t] <- x[,3]
    fill.sample$job.edu[fill.sample$year==t] <- x[,4]
    fill.sample$job.perdis[fill.sample$year==t] <- x[,5]
    rm(x)
    
    # update totaledu
    fill.sample$totaledu[fill.sample$year==t] <- fill.sample$l.totaledu[fill.sample$year==t] +
      fill.sample$job.edu[fill.sample$year==t]
    
    # predict job.ses
    # this prediction only has to be done for those
    # who have a full or partime job
    fill.sample$jobind[fill.sample$year==t] <- ifelse(fill.sample$job.full[fill.sample$year==t]==1 |
                                                        fill.sample$job.part[fill.sample$year==t]==1,
                                                      1,0)
    x <- multinomial.predict.2var(fit.bsf.jobses.high,
                                  fill.sample[fill.sample$year==t &
                                                fill.sample$jobind==1,])
    fill.sample$jobses.high[fill.sample$year==t & fill.sample$jobind==1] <- x[,1]
    fill.sample$jobses.low[fill.sample$year==t & fill.sample$jobind==1] <- x[,2]
    rm(x)
    
    # predict partnership
    x <- multinomial.predict.3var(fit.bsf.partner.married,fit.bsf.partner.cohab,
                                  fill.sample[fill.sample$year==t,])
    fill.sample$partner.married[fill.sample$year==t] <- x[,1]
    fill.sample$partner.cohab[fill.sample$year==t] <- x[,2]
    fill.sample$partner.single[fill.sample$year==t] <- x[,3]
    rm(x)
    
    # end loop
  }
  
  ## intervention loop ##
  fill.sample.2$group <- 1
  
  # select a specific group of women who are in education at age 16
  # part of this could actually be done outside the loop
  fill.sample.2$edu1986 <- 0
  fill.sample.2$edu1986 <- ifelse(fill.sample.2$job.edu[fill.sample.2$year==1986]==1,1,0)
  
  # take a % of women from this group, such that over the entire sample
  # 50% of women are selected
  fill.sample.2$eduselect <- 0
  fill.sample.2$eduselect[fill.sample.2$edu1986==1] <- rbinom(length(fill.sample.2$eduselect[fill.sample.2$edu1986==1]),1,0.5/mean(fill.sample.2$edu1986,na.rm=T))
  
  # start a loop that moves through the follow-up time units
  for(t in 1987:2008) {
    
    fill.sample.2.temp <- fill.sample.2[fill.sample.2$year==(t-1),]
    fill.sample.2.temp$year <- fill.sample.2.temp$year+1
    fill.sample.2.temp$age <- fill.sample.2.temp$age+1
    fill.sample.2.temp$age1522 <- ifelse(fill.sample.2.temp$age < 23,1,0)
    fill.sample.2.temp$age2329 <- ifelse(fill.sample.2.temp$age > 22 & fill.sample.2.temp$age < 30,1,0)
    fill.sample.2.temp$age30pl <- ifelse(fill.sample.2.temp$age > 30,1,0)
    
    # lag values: since the new values for t have not yet been produced
    # I can actually take the column information from a row at t
    # and put it in the same row at t but then in a column meant for
    # lagged values
    # the first are the 'from' columns, and the second the 'to' columns
    # so: take values from the the 'from' column and put them in the 'to' column
    fill.sample.2.temp[,col.index.to] <- fill.sample.2.temp[,col.index.from]
    
    # put together with entire dataset
    fill.sample.2 <- rbind(fill.sample.2,fill.sample.2.temp)
    rm(fill.sample.2.temp)
    
    # order by ID variable (needed for lags below)
    fill.sample.2 <- fill.sample.2[order(fill.sample.2$idnr,fill.sample.2$year),]
    
    ## predict functions here
    
    # predict birth
    fill.sample.2$birth[fill.sample.2$year==t] <- binomial.predict(fit.bsf.birth, fill.sample.2[fill.sample.2$year==t,])
    
    # stochastic extra birth
    nb <- length(fill.sample.2$birth[fill.sample.2$year==t])
    fill.sample.2$birth2 <- fill.sample.2$birth3 <- NULL
    fill.sample.2$birth2[fill.sample.2$year==t] <- rbinom(nb,1,twinprob)
    fill.sample.2$birth3[fill.sample.2$year==t] <- rbinom(nb,1,tripprob)
    # everyone gets a chance, but only those who actually have a single birth
    # get the 2nd and 3rd births counted when I update
    
    # update totalbirth (incl. multiple births)
    fill.sample.2$totalbirth[fill.sample.2$year==t] <- fill.sample.2$l.totalbirth[fill.sample.2$year==t] +
      fill.sample.2$birth[fill.sample.2$year==t] +
      # including multiple birth probs
      fill.sample.2$birth[fill.sample.2$year==t]*fill.sample.2$birth2[fill.sample.2$year==t] +
      fill.sample.2$birth[fill.sample.2$year==t]*fill.sample.2$birth3[fill.sample.2$year==t]
    
    # get rid of those who still create NA
    # because some predictor is missing
    fill.sample.2 <- fill.sample.2[!is.na(fill.sample.2$birth),]
    
    # update totalbirthcat
    fill.sample.2$totalbirthcat0[fill.sample.2$year==t] <- ifelse(fill.sample.2$totalbirth[fill.sample.2$year==t]==0,1,0)
    fill.sample.2$totalbirthcat1[fill.sample.2$year==t] <- ifelse(fill.sample.2$totalbirth[fill.sample.2$year==t]==1,1,0)
    fill.sample.2$totalbirthcat2[fill.sample.2$year==t] <- ifelse(fill.sample.2$totalbirth[fill.sample.2$year==t]==2,1,0)
    
    # predict job
    x <- multinomial.predict.5var(fit.bsf.job.full,fit.bsf.job.part,fit.bsf.job.less,
                                  fit.bsf.job.edu,
                                  fill.sample.2[fill.sample.2$year==t,])
    fill.sample.2$job.full[fill.sample.2$year==t] <- x[,1]
    fill.sample.2$job.part[fill.sample.2$year==t] <- x[,2]
    fill.sample.2$job.less[fill.sample.2$year==t] <- x[,3]
    fill.sample.2$job.edu[fill.sample.2$year==t] <- x[,4]
    fill.sample.2$job.perdis[fill.sample.2$year==t] <- x[,5]
    rm(x)
    
    # update totaledu
    fill.sample.2$totaledu[fill.sample.2$year==t] <- fill.sample.2$l.totaledu[fill.sample.2$year==t] +
      fill.sample.2$job.edu[fill.sample.2$year==t]
    
    # predict job.ses
    # this prediction only has to be done for those
    # who have a full or partime job
    fill.sample.2$jobind[fill.sample.2$year==t] <- ifelse(fill.sample.2$job.full[fill.sample.2$year==t]==1 |
                                                            fill.sample.2$job.part[fill.sample.2$year==t]==1,
                                                          1,0)
    x <- multinomial.predict.2var(fit.bsf.jobses.high,
                                  fill.sample.2[fill.sample.2$year==t &
                                                  fill.sample.2$jobind==1,])
    fill.sample.2$jobses.high[fill.sample.2$year==t & fill.sample.2$jobind==1] <- x[,1]
    fill.sample.2$jobses.low[fill.sample.2$year==t & fill.sample.2$jobind==1] <- x[,2]
    rm(x)
    
    # predict partnership
    x <- multinomial.predict.3var(fit.bsf.partner.married,fit.bsf.partner.cohab,
                                  fill.sample.2[fill.sample.2$year==t,])
    fill.sample.2$partner.married[fill.sample.2$year==t] <- x[,1]
    fill.sample.2$partner.cohab[fill.sample.2$year==t] <- x[,2]
    fill.sample.2$partner.single[fill.sample.2$year==t] <- x[,3]
    rm(x)
    
    ### intervention ### 
    
    # first make sure that we're below age 24
    if(t <= 1992) {
      
      # set the other job statuses of selected women to 0
      fill.sample.2$job.full[fill.sample.2$year==t & fill.sample.2$eduselect==1] <- 0
      fill.sample.2$job.part[fill.sample.2$year==t & fill.sample.2$eduselect==1] <- 0
      fill.sample.2$job.less[fill.sample.2$year==t & fill.sample.2$eduselect==1] <- 0
      fill.sample.2$job.perdis[fill.sample.2$year==t & fill.sample.2$eduselect==1] <- 0
      
      # then set their education to 1
      fill.sample.2$job.edu[fill.sample.2$year==t & fill.sample.2$eduselect==1] <- 1
      
      # update totaledu
      fill.sample.2$totaledu[fill.sample.2$year==t] <- fill.sample.2$l.totaledu[fill.sample.2$year==t] +
        fill.sample.2$job.edu[fill.sample.2$year==t]
      
    }
    
    # end loop
  }
  
  # end loop
  
  # total number of children in birth groups at various ages/years
  # and mean number of children in birth groups at various ages/years
  for(i in 1986:2008-1985) {
    output.dat[bs,1,i] <- sum(fill.sample$birth[fill.sample$year<=(i+1985)],na.rm=T)
    output.dat[bs,2,i] <- sum(fill.sample.2$birth[fill.sample.2$year<=(i+1985)],na.rm=T)
    
    output.dat[bs,3,i] <- mean(fill.sample$totalbirth[fill.sample$year==(i+1985)],na.rm=T)
    output.dat[bs,4,i] <- mean(fill.sample.2$totalbirth[fill.sample.2$year==(i+1985)],na.rm=T)
  }
  
  ## frequency table of children at various ages/years
  
  for(i in 1986:2008-1985) {
    # natural course
    temp.table <- table(fill.sample$totalbirth[fill.sample$year==(i+1985)])
    
    if(length(temp.table) < 15) {
      
      extra <- 15-length(temp.table)
      
      temp.table <- c(temp.table,rep(0,extra))  
    }
    babytable[bs,,i] <- temp.table
    
    # intervention
    temp.table.2 <- table(fill.sample.2$totalbirth[fill.sample.2$year==(i+1985)])
    
    if(length(temp.table.2) < 15) {
      
      extra <- 15-length(temp.table.2)
      
      temp.table.2 <- c(temp.table.2,rep(0,extra))  
    }
    babytable.2[bs,,i] <- temp.table.2
    
  }
  print(bs)
}

t2 <- Sys.time()

t2 - t1

# turn output.dat into a nice figure over time
mean(output.dat[,1,1])
mean(output.dat[,1,2])
mean(output.dat[,1,3])

babies.nc <- apply(output.dat[,1,],2,mean)
babies.int <- apply(output.dat[,2,],2,mean)

tfr.nc <- apply(output.dat[,3,],2,mean)
tfr.int <- apply(output.dat[,4,],2,mean)

# export births (total) and births int (for direct)
write.csv(cbind(babies.nc, babies.int),file='int4_output_multinom.csv')

plot(16:38,babies.nc,type='l',lwd=2,xlab='Age',ylab='Number of children ever born',
     main='Education Scenario',bty='l')
lines(16:38,babies.int,lwd=2,lty=5)
legend(legend=c('Natural course', 'Scenario'),16,5000,lwd=2,
       lty=c(1,5),bty='n')

# take covariance into account
tfr.ncint <- apply(output.dat[,4,]/output.dat[,3,],2,mean)

applyquantile95ci <- function(x) {
  quantile(x,probs=c(0.025,0.975),na.rm=T)
}

plot(16:38,c(NA,tfr.ncint[-1]),type='l',lty=1,lwd=2,xlab='Age',ylab='Ratio of number of children ever born',
     main='Education Scenario',ylim=c(0.70,1.30),bty='l')
lines(16:38,c(NA,apply(output.dat[,4,]/output.dat[,3,],2,applyquantile95ci)[1,-1]),lty=2)
lines(16:38,c(NA,apply(output.dat[,4,]/output.dat[,3,],2,applyquantile95ci)[2,-1]),lty=2)
legend(legend=c('Estimate', '95% confidence bounds'),25,1.3,lwd=c(2,1),
       lty=c(1,2),bty='n')

apply(output.dat[,4,]/output.dat[,3,],2,applyquantile95ci)
# significant diff after year 9 (age 24)

# check baby table distributions
# we're especially interested in the last one (23)
apply(babytable[,,23],2,mean)

# compare to empirical one
table(fill.dat$totalbirth[fill.dat$year==2008])

# empirical percentages age 28
round(table(fill.dat$totalbirth[fill.dat$year==1998])/sum(table(fill.dat$totalbirth[fill.dat$year==2008])),digits=2)

# empirical percentages age 38
round(table(fill.dat$totalbirth[fill.dat$year==2008])/sum(table(fill.dat$totalbirth[fill.dat$year==2008])),digits=2)

# natural course percentages age 28
round(apply(babytable[,,13],2,mean)/sum(apply(babytable[,,13],2,mean)),2)

# natural course percentages age 38
round(apply(babytable[,,23],2,mean)/sum(apply(babytable[,,23],2,mean)),2)

# intervention percentages age 28
round(apply(babytable.2[,,13],2,mean)/sum(apply(babytable.2[,,13],2,mean)),2)

# intervention percentages age 38
round(apply(babytable.2[,,23],2,mean)/sum(apply(babytable.2[,,23],2,mean)),2)

