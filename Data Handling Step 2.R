
#######################
## Data Handling of  ##
## the G-formula     ##
## Fertility study   ##
## M.J. Bijlsma      ##
#######################

# run first the .do script named
# 'Data Handling Step 1.do'
# which was produced by student assistant Joshua Perleberg
# this produces a number of files that we will load here
# using STATA, turn those files into .csv files

## load datasets ##
Preg.dat <- read.csv(file='Pregnancy_long.csv',head=T)
Par.dat <- read.csv(file='Partnership_long.csv',head=T)
Emp.dat <- read.csv(file='Employment_long.csv',head=T)
timeconstant.dat <- read.csv(file='time_constant.csv',head=T)

# compare IDs for women
Preg.dat.f <- Preg.dat[Preg.dat$sex=='2. Female',]
timeconstant.dat.f <- timeconstant.dat[timeconstant.dat$sex=='Female',]

length(unique(as.character(Preg.dat.f$bcsid)) )#  6197
length(unique(as.character(timeconstant.dat.f$bcsid))) # 8762

setdiff(unique(as.character(Preg.dat.f$bcsid)), unique(as.character(timeconstant.dat.f$bcsid))) # 18
setdiff(unique(as.character(timeconstant.dat.f$bcsid)),unique(as.character(Preg.dat.f$bcsid))) # 2582
# conclusion: baseline has more IDs, and only 18 people that are present
# in the pregnancy file are not in the baseline file

length(unique(Preg.dat$bcsid)) # this is the smallest file but not the source of the prob
length(unique(Par.dat$bcsid))
length(unique(Emp.dat$bcsid)) 
length(unique(timeconstant.dat$bcsid))
# Par and Emp both have roughly a 1000 IDs more than timeconstant

# let's merge them now before the original datahandling
# to figure out where the missing stuff could be coming from
test1 <- merge(Preg.dat,timeconstant.dat,by='bcsid',all.x=T)

test2 <- test1[test1$sex.x == '2. Female',]
test1 <- merge(test1,timeconstant.dat,by='bcsid',all.x=T)

table(test2$siblingsize)
length(test2$siblingsize)
48936/148728 # 32%

table(is.na(test2$siblingsD))
# 37536 / (111192 +37536 ) = 0.2523802, 25%
# so it is not caused by the data handling, it is inherent
# in the two datasets

table(is.na(timeconstant.dat.f$siblingsB))
# is it actually just inherent to the timeconstant dataset

# Ok so let's begin data handling now

# the data in these datasets (except for time constant)
# is information by CMC, but we will turn this into yearly info
# the rule that will be applied here is that we will take the
# last reported activity in a calendar year
# another option would be to take the activity in each year
# that the respondent spent most time in

# For the preg, par and emp.dat's the approach is simple here
# remove superfluous rows
# then turn CMC codes into years
# then join datasets on ID and year
# then join the time constant info with that dataset

# let's check Preg.dat first
names(Preg.dat)

# X_j (in STATA just _j), date and event are not used
# same for merge_08_05, merge_08_05_00, wave, and dif
Preg.dat$X_j <- Preg.dat$date <- Preg.dat$event <- NULL
Preg.dat$merge_08_05 <- Preg.dat$merge_08_05_00 <- 
  Preg.dat$wave <- Preg.dat$dif <- NULL

# the curp variable (currently pregnant) does not
# have a date next to it. Should be at time of interview
# we don't use this info so I remove it
Preg.dat$curp <- NULL

# pmis, pdea and pnum are possibly useful if we want to
# model pregnancies instead of live births.
# but for now I exclude them
Preg.dat$pmis <- Preg.dat$pdea <- Preg.dat$pnum <- NULL

# for pregnancy, I therefore keep
# id
# bcsid
# pliv (number of live births)
# pcmc (cmc of pregnancy)
# sex
# j (in case I need to re-order)

# I will save all the unique IDs in Preg.dat
# since many women don't have any child
id.Preg.dat <- unique(Preg.dat$id)
bcsid.Preg.dat <- unique(Preg.dat$bcsid)

# all live births are accompanied by a pcmc, so
# remove all observations that don't have a pcmc
head(Preg.dat$pcmc)
Preg.dat <- Preg.dat[!is.na(Preg.dat$pcmc),]

# so Preg.dat now only has women
# that have a baby


# Let's check Par.dat
names(Par.dat)

# are there any individuals not in ANY sweep?
head(Par.dat$WSWEEP05)
head(Par.dat$WSWEEP06)
head(Par.dat$WSWEEP07)
head(Par.dat$WSWEEP08)
length(Par.dat$bcsid[Par.dat$WSWEEP05=='CM not in sweep5' &
                       Par.dat$WSWEEP06=='CM not in sweep6' &
                       Par.dat$WSWEEP07=='CM not in sweep7' &
                       Par.dat$WSWEEP08=='CM not in sweep8'])
# 6056

# Let's have a look at these people
View(Par.dat[Par.dat$WSWEEP05=='CM not in sweep5' &
               Par.dat$WSWEEP06=='CM not in sweep6' &
               Par.dat$WSWEEP07=='CM not in sweep7' &
               Par.dat$WSWEEP08=='CM not in sweep8',])
# these people indeed report no information
# whatsoever
# I put their IDs in a vector and will
# remove them from our analyses entirely
bcsid.nosweep.remove <- unique(Par.dat$bcsid[Par.dat$WSWEEP05=='CM not in sweep5' &
                                               Par.dat$WSWEEP06=='CM not in sweep6' &
                                               Par.dat$WSWEEP07=='CM not in sweep7' &
                                               Par.dat$WSWEEP08=='CM not in sweep8'])
# (quick extra check)
View(Par.dat[which(Par.dat$bcsid %in% bcsid.nosweep.remove),])
# yep, no obs

# remove these people from Par.dat
bcsid.Par.dat <- unique(Par.dat$bcsid)
bcsid.Par.dat.keep <- setdiff(bcsid.Par.dat,bcsid.nosweep.remove)

Par.dat <- Par.dat[which(Par.dat$bcsid %in% bcsid.Par.dat.keep),]

table(Par.dat$WSWEEP05)
table(Par.dat$WSWEEP06) # most are in Sweep 6
table(Par.dat$WSWEEP07) # 50/50 present and missing
table(Par.dat$WSWEEP08) # 2/3 present

# what we can do is we make an indicator for when a person
# is missing
# then we can censor those persons from conditional models
# when they are missing
# I keep the SWEEP info for now
# and then we can use those colums for censoring later
# what happens is that individuals who were not in Sweep t
# but were in Sweep t+k get all their information updated
# but of course if they are missing from the last Sweep
# then they don't get that information updated
# so what we need to censor are only those people who are
# not in any Sweep, and those years for individuals who were
# not in the later Sweeps

names(Par.dat)
# not useful are
# numpshps
# partner
# pship
# poutcome
# psex
# ppage
# ppms
# pplegs
# pdivce
# cerrors
# perrors
# all the PERROR# (1 to 15) (these indicate if rel overlap)
# but I deal with that myself
# all the CERROR1, 2, 2A, 3
# SWPFLG05, 06, 07, 08
# INTCMC05, 06, 07, 08 <- still useful to know when sweeps were done
# e.g. sort(names(table(Par.dat$INTCMC06)))
# -> Sweep 5 at 1161, Sweep 6 between 1194 and 1209
# -> Sweep 7 between 1250 and 1266, Sweep 8 between 1306 and 1313
# pslot
# pswp
# pfstswp
# page

# useful are
# cmsex
# pstcmc
# pdur (pcmc + pdur = enddate)
# pendcmc (I either use this or pdur)
# ptype (type of partnership)
# psamesex (perhaps? Can still get pregnant if women)
# pdivcmc

Par.dat$numpshps <-
  Par.dat$partner <- Par.dat$pship <- Par.dat$poutcome <-
  Par.dat$psex <- Par.dat$ppage <- Par.dat$ppms <-
  Par.dat$pplegs <- Par.dat$pdivce <- Par.dat$cerrors <-
  Par.dat$perrors <-
  Par.dat$PERROR1 <- Par.dat$PERROR2 <- Par.dat$PERROR3 <- 
  Par.dat$PERROR4 <- Par.dat$PERROR5 <- Par.dat$PERROR6 <- 
  Par.dat$PERROR7 <- Par.dat$PERROR8 <- Par.dat$PERROR9 <- 
  Par.dat$PERROR10 <- Par.dat$PERROR11 <- Par.dat$PERROR12 <- 
  Par.dat$PERROR13 <- Par.dat$PERROR14 <- Par.dat$PERROR15 <-
  Par.dat$CERROR1 <- Par.dat$CERROR2 <- 
  Par.dat$CERROR2A <- Par.dat$CERROR3 <- 
  Par.dat$SWPFLG05 <- Par.dat$SWPFLG06 <- Par.dat$SWPFLG07 <-
  Par.dat$SWPFLG08 <-
  Par.dat$INTCMC05 <- Par.dat$INTCMC06 <- Par.dat$INTCMC07 <-
  Par.dat$INTCMC08 <-
  Par.dat$pslot <- Par.dat$pwsp <- Par.dat$pfstswp <-
  Par.dat$page <- Par.dat$pswp <- NULL

# actually, since pdur is up to
# the date of interview, I remove dur as well
# since it is less likely to get into trouble
# by using pendcmc (as it there specifically says 'still current')
Par.dat$pdur <- NULL

# I also remove pdivcmc since that is the legal divorce date
# whereas the pendcmc gives the empirical end date
Par.dat$pdivcmc <- NULL


# Now let's check Emp.dat
names(Emp.dat)
table(Emp.dat$WSWEEP08) # most are in sweep08
# will treat it similar to the sweep stuff in Par.dat
# I put their IDs in a vector and will
# remove them from our analyses entirely
# first the individuals who have are not in any Sweep
# (the rest will be done later)
bcsid.nosweep.Emp.remove <- unique(Emp.dat$bcsid[Emp.dat$WSWEEP05=='CM not in sweep5' &
                                                   Emp.dat$WSWEEP06=='CM not in sweep6' &
                                                   Emp.dat$WSWEEP07=='CM not in sweep7' &
                                                   Emp.dat$WSWEEP08=='CM not in sweep8'])

# remove these people from Emp.dat
bcsid.Emp.dat <- unique(Emp.dat$bcsid)
bcsid.Emp.dat.keep <- setdiff(bcsid.Emp.dat,bcsid.nosweep.Emp.remove)

Emp.dat <- Emp.dat[which(Emp.dat$bcsid %in% bcsid.Emp.dat.keep),]

# note that the number of IDs in
# bcsid.Emp.dat.keep and bcsid.Par.dat.keep are identical
# these are likely the same people

# jactiv is super handy since it also has education
# jftpt also (full time, part time)

# I will remove
# numacts
# actnum
# jdur
# histlen
# overlap
# poverlap
# gap
# pgap
# cerrors
# aerrors
# AERROR1 to 8
# jyleft
# jyleftx
# INTCM05 to 08
# SWPFLG05 to 08
# jlstswp
# jlstcmc
# jslot

# J8NSSEC
# jtorg

# check if we want to use jempst, J91SC and/or J91SEG
# they have lots of observations on it
# and do give socio-economic info

Emp.dat$numacts <- Emp.dat$actnum <- Emp.dat$jdur <-
  Emp.dat$histlen <- Emp.dat$overlap <- Emp.dat$poverlap <-
  Emp.dat$gap <- Emp.dat$pgap <- Emp.dat$cerrors <-
  Emp.dat$aerrors <-
  Emp.dat$AERROR1 <- Emp.dat$AERROR2 <- Emp.dat$AERROR3 <- 
  Emp.dat$AERROR4 <- Emp.dat$AERROR5 <- Emp.dat$AERROR6 <- 
  Emp.dat$AERROR7 <- Emp.dat$AERROR8 <- 
  Emp.dat$jyleft <- Emp.dat$jyleftx <-
  Emp.dat$INTCMC05 <- Emp.dat$INTCMC06 <- Emp.dat$INTCMC07 <- 
  Emp.dat$INTCMC08 <- 
  Emp.dat$SWPFLG05 <- Emp.dat$SWPFLG06 <- Emp.dat$SWPFLG07 <-
  Emp.dat$SWPFLG08 <-
  Emp.dat$jlstswp <- Emp.dat$jlstcmc <- Emp.dat$jslot <-
  Emp.dat$J8NSSEC <- Emp.dat$jtorg <- NULL

names(Emp.dat)

##

# check overlap in IDs between the three
# time-varying datasets

# Emp and Par
setdiff(bcsid.Emp.dat.keep, bcsid.Par.dat.keep)
setdiff(bcsid.Par.dat.keep,bcsid.Emp.dat.keep)
# the sets are the same

# Emp and Preg (and given previous finding, automatically
# Par and Preg as well)
setdiff(bcsid.Emp.dat.keep, bcsid.Preg.dat) # 817 entries diff
setdiff(bcsid.Preg.dat,bcsid.Emp.dat.keep)

# now make a new dataset with the bcsids from
# Emp.dat and 38-15 years of data
# and start filling it with info like previously
# then add baseline info onto that

fill.dat <- data.frame(cbind(rep(bcsid.Emp.dat.keep,each=23),rep(c(1986:2008),length(bcsid.Emp.dat.keep))))
names(fill.dat) <- c('bcsid','year')
fill.dat$bcsid <- as.character(fill.dat$bcsid)
fill.dat$year <- as.numeric(as.character(fill.dat$year))
fill.dat$age <- fill.dat$year-1970

# let's begin by adding sex
fill.dat$sex <- NA
for(id in unique(fill.dat$bcsid)) {
  
  fill.dat$sex[fill.dat$bcsid==id] <- as.character(Emp.dat$cmsex[Emp.dat$bcsid==id][1])
  
  print(id)
}
# there are 115 persons with an unknown gender
table(fill.dat$sex)
# let's remove those
fill.dat <- fill.dat[fill.dat$sex != 'Not known',]

# now let's add birth info
# first I turn CMC codes in Preg.dat into years
# year = 1900 + int((CMC-1/12))
Preg.dat$year <- 1900+floor((Preg.dat$pcmc-1)/12)

# I remove the pregnancies with 0 live births (and the 4 with -1?)
Preg.dat <- Preg.dat[Preg.dat$pliv > 0,]

# and like last time, if someone has two live pregnancies in
# a year, then we bump one pregnancy to the next year
# I use a lag function (data are already chronologically sorted)

lag1.func <- function(variabletolag, idvariable) {
  
  c(NA,ifelse(idvariable[2:length(idvariable)] == idvariable[1:(length(idvariable)-1)],
              variabletolag[1:(length(idvariable)-1)],NA))
  
}

Preg.dat$l.year <- lag1.func(Preg.dat$year,Preg.dat$bcsid)

table((Preg.dat$l.year == Preg.dat$year))
# only 9 births have this 
# (many have NA for logical reasons, which is why FALSE + TRUE =/= n)
# View(Preg.dat[Preg.dat$l.year == Preg.dat$year & !is.na(Preg.dat$l.year),])
# for these people, I bump the birth to the next year
Preg.dat$bumpyear <- ifelse(Preg.dat$year==Preg.dat$l.year,1,0)
Preg.dat$bumpyear <- ifelse(is.na(Preg.dat$bumpyear),0,Preg.dat$bumpyear)
# indeed 7 people
Preg.dat$year[Preg.dat$bumpyear==1] <- Preg.dat$year[Preg.dat$bumpyear==1]+1

# check if this solves all problems
# (as maybe there was another birth the year later)
table((Preg.dat$l.year == Preg.dat$year))
# yes, all double live pregnancies solved

# check if anyone had births before 1986
table(Preg.dat$year)

# yes, 8 people had a birth before 1986
# 1 supposedly already in 1980
# for these people, we set pregnancy counter
# at 1 in year 1986 (the year of start to follow-up)
# let's check their IDs
earlypreg.id <- Preg.dat$bcsid[Preg.dat$year < 1986]
View(Preg.dat[which(Preg.dat$bcsid %in% earlypreg.id),])
# for all these people it happened once before 1986,
# and they did not have a birth in 1986 itself
# I will give these people a higher 'totalbirth' (see below)

# no, first birth in 1987
# that makes joining everything easier

# I will move through the pregnancies
# and set pregnancies to pliv if Preg.dat$year == fill.dat$year
fill.dat$pliv <- 0
for(yr in 1986:2008) {
  
  for(id in unique(Preg.dat$bcsid[Preg.dat$year==yr])) {
    
    fill.dat$pliv[fill.dat$year==yr & fill.dat$bcsid==id] <-
      Preg.dat$pliv[Preg.dat$year==yr & Preg.dat$bcsid==id]
    
  }
  
  print(yr)
  
}

table(fill.dat$pliv)
table(Preg.dat$pliv)
# these are about the same

# first let's add a birth indicator
# which is 1 if there is a birth and 0 if not
fill.dat$birth <- ifelse(fill.dat$pliv > 0,1,0)

# now let's make a totalbirth var
# and for those who belonged to the earlypreg.id, I will set
# a birth in 1986 temporarily
# so that it gets counted towards the totalbirth, but then I remove it afterwards
fill.dat$birth[fill.dat$bcsid %in% earlypreg.id & fill.dat$year==1986] <- 1

fill.dat$totalbirth <- 0
for(id in unique(Preg.dat$bcsid)) {
  
  fill.dat$totalbirth[fill.dat$bcsid==id] <- cumsum(fill.dat$pliv[fill.dat$bcsid==id])
  
  print(id)
}

# set birth in 1986 back to 0 for the early preggers
fill.dat$birth[fill.dat$bcsid %in% earlypreg.id & fill.dat$year==1986] <- 0

# Now I can add partnership info
str(Par.dat)

table(Par.dat$pstcmc)
table(Par.dat$pendcmc)
# in both variables
# there is a group of 1613 obs where the var
# is set to 'Not applicable'
# let's have a look at them
View(Par.dat[Par.dat$pstcmc=='Not applicable',])

# these are people for whom we have no Partnership info
# but were not yet previously removed
# because they are present is some sweeps 
# but simply did not provide partnership info
# this is probably be because they are continuously single
# because apparently it is only registered if
# individuals cohabit or are married in the data as I now have it
# otherwise they would have been categorized as
# 'Don't know / Not enough info.'

# then there is a group of people for whom we have 
# 'Don't know/ Not enough info.'
# 25 in start
# and 438 in endpa
# let's look at them
View(Par.dat[Par.dat$pstcmc=="Don't know/ Not enough info.",])
View(Par.dat[Par.dat$pendcmc=="Don't know/ Not enough info.",])
length(unique(Par.dat$bcsid[Par.dat$pstcmc=="Don't know/ Not enough info."]))
length(unique(Par.dat$bcsid[Par.dat$pendcmc=="Don't know/ Not enough info."]))
# all people who have "don't know" in start have only 1 observation in Part.dat
# they all claim to be cohabiting
# and they also all exist in the category which has that answer for pendcmc
# the vast majority of people who have "don't know" in end also have only 1 observation in Part.dat
# these are either cohabiting or married

# these are people I cannot use for now, so I will also make an indicator for them
# I suppose I will remove them later
bcsid.dontknow.Par.info <- unique(Par.dat$bcsid[Par.dat$pendcmc=="Don't know/ Not enough info."])

fill.dat$dontknow.Par.info <- 0
fill.dat$dontknow.Par.info[which(fill.dat$bcsid %in% bcsid.dontknow.Par.info)] <- 1 

# that leaves people for whom the end date is 'Partnership still current at most recent sweep'
# I will simply set their endyear to 2009
# (see a few lines from here)

# let's first turn the beginning and ending cmcs into years
# year = 1900 + int((CMC-1/12))
Par.dat$pstcmc2 <- as.numeric(as.character(Par.dat$pstcmc))
Par.dat$pendcmc2 <- as.numeric(as.character(Par.dat$pendcmc))
# NA's are not a problem here

Par.dat$pstyear <- 1900+floor((Par.dat$pstcmc2-1)/12)
Par.dat$pendyear <- 1900+floor((Par.dat$pendcmc2-1)/12)
Par.dat$pendyear <- ifelse(Par.dat$pendcmc=="Partnership still current at most recent sweep",2009,Par.dat$pendyear)

# since multiple relationships in a year are likely a lot more
# common than multiple births, I will sort out multiple
# relationships in a year in the for-loop
table(Par.dat$pstyear)
table(Par.dat$pendyear)
table(Par.dat$ptype)

Par.dat$ptype <- as.character(Par.dat$ptype)

# first I set people to single
fill.dat$partype <- 'Single'

for(id in unique(Par.dat$bcsid)) {
  
  for(yr in unique(Par.dat$pstyear[Par.dat$bcsid==id])) {
    
    # check how many pstyear entries there are in a year
    k <- length(Par.dat$bcsid[Par.dat$bcsid==id & Par.dat$pstyear==yr])
    # and take the partnership type entry registered last in that year
    partype <- Par.dat$ptype[Par.dat$bcsid==id & Par.dat$pstyear==yr][k]
    # and take the end-date entry registered last in that year
    part.endyear <- Par.dat$pendyear[Par.dat$bcsid==id & Par.dat$pstyear==yr][k]
    
    # then fill the years corresponding to the period
    # described by the startyear and endyear with this partype
    # in fill.dat
    fill.dat$partype[fill.dat$bcsid==id &
                       fill.dat$year >= yr &
                       fill.dat$year <= part.endyear] <- partype
    
    rm(partype)
    # if there is overlap in a year, then the later relationship
    # has the priority
    
    
  }
  
  print(id)
}

# comparing fill.dat with Par.dat shows that this worked
# I'll add Sweep presence info later

# now let's go on to Emp.dat

# jstcmc: start time
# jendcmc: end time
# jactiv: activity type
# jftp: full time or part time
# jempst: activity employment status
# J91SEG: activity SEG?
# J1SC: activity social class?
# J8NSSEC: ??

table(Emp.dat$jstcmc) # like previously: some NA, and a minor group "Don't know"
table(Emp.dat$jendcmc) # about the same numbers as NA, only 1 "Don't know"
table(Emp.dat$jactiv) # 855 "Don't know", also 827 NA (similar number as jst and jend)
table(Emp.dat$jftp) # 158 "Don't know", many, many NA. Should check this out
table(Emp.dat$jempst) # 414 "Don't know", very many NA
table(Emp.dat$J91SEG) # Job hierarhicacal info. We could use this
table(Emp.dat$J91SC) # Job skill level info. We could use this

# it turns out that jftp and jempst only
# have values if the person has a job, which makes sense

# check jactiv people who answer 'Don't know'
table(Emp.dat$jactiv)
View(Emp.dat[Emp.dat$jactiv=="Don't know/ Not enough info.",])
# there is a specific time period in which the person
# says that they don't know what kind of activity they did
# so I will simply copy that for now as an activity type

# the only thing that remains to be established is
# to do something with the durations data (jstcmc and jendcmc)
# where we have no info
View(Emp.dat[Emp.dat$jstcmc=="Don't know/ Not enough info.",])
# fairly often these also have 'NA' or so in jftp and jempst
View(Emp.dat[Emp.dat$jstcmc=="Not applicable",])
# These didn't provide employment info in any of the Emp sweeps
View(Emp.dat[Emp.dat$jendcmc=="Not applicable",])
# same

# so they can be removed?
# I'll give them an indicator in fill.dat
length(Emp.dat$bcsid[Emp.dat$jstcmc=="Not applicable"])
length(Emp.dat$bcsid[Emp.dat$jendcmc=="Not applicable"])
# these are the same people in both variables
# which makes sense

bcsid.Emp.dat.NA <- unique(Emp.dat$bcsid[Emp.dat$jstcmc=="Not applicable"])

fill.dat$no.Emp.info <- 0
fill.dat$no.Emp.info[which(fill.dat$bcsid %in% bcsid.Emp.dat.NA)] <- 1

# now I remove those people from Emp.dat
bcsid.Emp.keep <- setdiff(unique(Emp.dat$bcsid),bcsid.Emp.dat.NA)
Emp.dat <- Emp.dat[which(Emp.dat$bcsid %in% bcsid.Emp.keep),]

# can part-time people have multiple
# entries in the same time period?
bcsid.temp <- unique(Emp.dat$bcsid[Emp.dat$jactiv=="P/t paid employee (lt 30 hrs)" |
                                     Emp.dat$jactiv=="P/t self-employed" |
                                     Emp.dat$jactiv=="Part-time education"])
View(Emp.dat[which(Emp.dat$bcsid %in% bcsid.temp),])
# no, only 1 part-time activity allowed
# that makes putting this info into fill.dat easier

# make an indicator variable for if people
# have their activity still going on in 2009
Emp.dat$jactstillcurrent <- ifelse(Emp.dat$jendcmc=="Activity still current at most recent sweep",1,0)

# then transform jstcmc and jendcmc into numbers
Emp.dat$jstcmc <- as.numeric(as.character(Emp.dat$jstcmc))
Emp.dat$jendcmc <- as.numeric(as.character(Emp.dat$jendcmc))

# transform cmc to years
# year = 1900 + int((CMC-1/12))
Emp.dat$jstyear <- 1900+floor((Emp.dat$jstcmc-1)/12)
Emp.dat$jendyear <- 1900+floor((Emp.dat$jendcmc-1)/12)

# set end to 2009 if indicator is 1
Emp.dat$jendyear <- ifelse(Emp.dat$jactstillcurrent==1,2009,Emp.dat$jendyear)

# now I can add Emp.dat info to fill.dat
# I will use code very similar to that found when adding partype to fill.dat
# I will not add jactiv and jftp
# that means that the last activity in a year will overwrite
# other activities in that year if there are multiple
# ones in a year

# first I set activity type to 'Nothing copied'
# this is different from NA on purpose, so I can identify
# that the reason the info is not present
# is because I did not copy it (instead of Emp.dat saying NA)

fill.dat$actype <- 'Nothing copied'
fill.dat$J91SEG <- 'Nothing copied'
fill.dat$J91SC <- 'Nothing copied'

Emp.dat$jactiv <- as.character(Emp.dat$jactiv)
Emp.dat$bcsid <- as.character(Emp.dat$bcsid)
Emp.dat$J91SC <- as.character(Emp.dat$J91SC)

Emp.dat$bcsid.part <- substr(Emp.dat$bcsid,7,7)
fill.dat$bcsid.part <- substr(fill.dat$bcsid,7,7)

for(part in unique(Emp.dat$bcsid.part)) {
  
  Emp.dat.part <- Emp.dat[Emp.dat$bcsid.part==part,]
  fill.dat.part <- fill.dat[fill.dat$bcsid.part==part,]
  
  for(id in unique(Emp.dat.part$bcsid)) {
    
    for(yr in unique(Emp.dat.part$jstyear[Emp.dat.part$bcsid==id])) {
      
      # check how many pstyear entries there are in a year
      k <- length(Emp.dat.part$bcsid[Emp.dat.part$bcsid==id & Emp.dat.part$jstyear==yr])
      # and take the employment activity and SES info type entry registered last in that year
      actype <- Emp.dat.part$jactiv[Emp.dat.part$bcsid==id & Emp.dat.part$jstyear==yr][k]
      J91SEG <- Emp.dat.part$J91SEG[Emp.dat.part$bcsid==id & Emp.dat.part$jstyear==yr][k]
      J91SC <- Emp.dat.part$J91SC[Emp.dat.part$bcsid==id & Emp.dat.part$jstyear==yr][k]
      
      # and take the end-date entry registered last in that year
      act.endyear <- Emp.dat.part$jendyear[Emp.dat.part$bcsid==id & Emp.dat.part$jstyear==yr][k]
      
      # then fill the years corresponding to the period
      # described by the startyear and endyear with this actype
      # in fill.dat
      fill.dat.part$actype[fill.dat.part$bcsid==id &
                             fill.dat.part$year >= yr &
                             fill.dat.part$year <= act.endyear] <- actype
      
      # and also copy the job SES-type info
      fill.dat.part$J91SEG[fill.dat.part$bcsid==id &
                             fill.dat.part$year >= yr &
                             fill.dat.part$year <= act.endyear] <- J91SEG
      
      fill.dat.part$J91SC[fill.dat.part$bcsid==id &
                            fill.dat.part$year >= yr &
                            fill.dat.part$year <= act.endyear] <- J91SC
      
      rm(actype)
      # if there is overlap in a year, then the later activity
      # has the priority
      
    }
    
  }
  
  fill.dat[fill.dat$bcsid.part==part,] <- fill.dat.part
  
  Emp.dat.part <- NULL
  fill.dat.part <- NULL
  
  print(part)
  
}

# check if this was done correctly
table(fill.dat$actype)
# this does indicate that there are quite a few patients
# who have 'Nothing copied' as the option, which means that they did not
# have info from Emp.dat for that particular year

# I investigated this by checking IDs in fill.dat which had
# actype == 'Nothing copied' and it turns out that these are often
# observations of people for whom we don't have information
# from various Sweeps or activity data not recorded for those Sweeps
# we can still use them when estimating conditional relations
# though. But it means we must exclude/censor them from
# the years in which we don't have the Sweep info
# I will do this below

## Baseline info ##

# I will only keep baseline stuff
# that I will actually use at first
str(timeconstant.dat)
names(timeconstant.dat)

# which, for now, is
# c3_4 (father's corrected social class 1980)
# c3_5 (father's corrected social class 1970)
# edufath86
# birthcountry80 (probably won't use though)
# income80 (total gross family income)
# income86 (total gross family income)
# rural86 (probably won't use though)
# siblingyounger86, twins86, siblingolder86
# twin

# order fill.dat and timeconstant.dat2 the same
# not sure if needed in R for merging, but it doesn't hurt
fill.dat <- fill.dat[order(fill.dat$bcsid),]
timeconstant.dat <- timeconstant.dat[order(timeconstant.dat$bcsid),]

fill.dat2 <- merge(fill.dat,timeconstant.dat,all.x=T)
fill.dat2 <- fill.dat2[order(fill.dat2$bcsid,fill.dat2$year),]

# this looks fine, so I can get rid of fill.dat
rm(fill.dat)
fill.dat <- fill.dat2
rm(fill.dat2)

# turn the factors into characters, so I don't have mistakes later
fill.dat$bcsid <- as.character(fill.dat$bcsid)
fill.dat$sex <- as.character(fill.dat$sex)
fill.dat$partype <- as.character(fill.dat$partype)
fill.dat$actype <- as.character(fill.dat$actype)
fill.dat$J8NSSEC <- as.character(fill.dat$J8NSSEC)
fill.dat$agemother  <- as.character(fill.dat$agemother)
fill.dat$parity70 <- as.character(fill.dat$parity70)
fill.dat$agefather <- as.character(fill.dat$agefather)
fill.dat$income80 <- as.character(fill.dat$income80)
fill.dat$region86  <- as.character(fill.dat$region86)
fill.dat$c5k45  <- as.character(fill.dat$c5k45 )
fill.dat$famdisruption86 <- as.character(fill.dat$famdisruption86)
fill.dat$rural86 <- as.character(fill.dat$rural86)
fill.dat$income86 <- as.character(fill.dat$income86)
fill.dat$FatherSocialClass86 <- as.character(fill.dat$FatherSocialClass86)
fill.dat$MotherSocialClass86 <- as.character(fill.dat$MotherSocialClass86)
fill.dat$FatherEmployStat86 <- as.character(fill.dat$FatherEmployStat86)
fill.dat$MotherEmployStat86 <- as.character(fill.dat$MotherEmployStat86)
fill.dat$siblingsize <- as.character(fill.dat$siblingsize)
fill.dat$twin <- as.character(fill.dat$twin)
fill.dat$foreignborn <- as.character(fill.dat$foreignborn)

## based on Sweep info, see who should be excluded ##
# The possibilities are:
# not in Sweep 8 (remove Sweep 8 rows)
# not in Sweep 7 and 8 (remove sweep 7 and 8 rows)
# not in Sweep 6, 7 and 8 (remove sweep 6, 7 and 8 rows)
# I already removed individuals not present in any of the Sweeps

Emp.dat$WSWEEP05 <- as.character(Emp.dat$WSWEEP05)
Emp.dat$WSWEEP06 <- as.character(Emp.dat$WSWEEP06)
Emp.dat$WSWEEP07 <- as.character(Emp.dat$WSWEEP07)
Emp.dat$WSWEEP08 <- as.character(Emp.dat$WSWEEP08)

Par.dat$WSWEEP05 <- as.character(Par.dat$WSWEEP05)
Par.dat$WSWEEP06 <- as.character(Par.dat$WSWEEP06)
Par.dat$WSWEEP07 <- as.character(Par.dat$WSWEEP07)
Par.dat$WSWEEP08 <- as.character(Par.dat$WSWEEP08)

# not in Sweep 8
emp.not8 <- unique(Emp.dat$bcsid[Emp.dat$WSWEEP08 == 'CM not in sweep8'])
par.not8 <- unique(Par.dat$bcsid[Par.dat$WSWEEP08 == 'CM not in sweep8'])

length(emp.not8) # 3354 individuals
length(par.not8) # 4171 individuals

setdiff(emp.not8,par.not8) # the 3354 in emp are all also in par

# not in Sweep 7 AND 8
emp.not7n8 <- unique(Emp.dat$bcsid[Emp.dat$WSWEEP08 == 'CM not in sweep8' &
                                   Emp.dat$WSWEEP07 == 'CM not in sweep7'])
par.not7n8 <- unique(Par.dat$bcsid[Par.dat$WSWEEP08 == 'CM not in sweep8' &
                                   Par.dat$WSWEEP07 == 'CM not in sweep7'])

length(emp.not7n8) # 1504 individuals
length(par.not7n8) # 2305 individuals

setdiff(emp.not7n8,par.not7n8) # and again those in emp are all in par

# not in Sweep 6, 7 AND 8
emp.not6n7n8 <- unique(Emp.dat$bcsid[Emp.dat$WSWEEP08 == 'CM not in sweep8' &
                                     Emp.dat$WSWEEP07 == 'CM not in sweep7' &
                                       Emp.dat$WSWEEP06 == 'CM not in sweep6'])
par.not6n7n8 <- unique(Par.dat$bcsid[Par.dat$WSWEEP08 == 'CM not in sweep8' &
                                     Par.dat$WSWEEP07 == 'CM not in sweep7' &
                                       Par.dat$WSWEEP06 == 'CM not in sweep6'])

length(emp.not6n7n8) # 35 individuals
length(par.not6n7n8) # 817 individuals

setdiff(emp.not6n7n8,par.not6n7n8) # same

# not in any Sweep (just to be sure)
emp.not5n6n7n8 <- unique(Emp.dat$bcsid[Emp.dat$WSWEEP08 == 'CM not in sweep8' &
                                       Emp.dat$WSWEEP07 == 'CM not in sweep7' &
                                       Emp.dat$WSWEEP06 == 'CM not in sweep6' &
                                       Emp.dat$WSWEEP05 == 'CM not in sweep5'])
par.not5n6n7n8 <- unique(Par.dat$bcsid[Par.dat$WSWEEP08 == 'CM not in sweep8' &
                                       Par.dat$WSWEEP07 == 'CM not in sweep7' &
                                       Par.dat$WSWEEP06 == 'CM not in sweep6' &
                                       Par.dat$WSWEEP05 == 'CM not in sweep5'])

length(emp.not5n6n7n8) # 0 individuals
length(par.not5n6n7n8) # 0 individuals
# good, as described, they were already removed

# now let's make indicator variables for removing rows
par.not6n7n8 <- as.character(par.not6n7n8)
par.not7n8 <- as.character(par.not7n8)
par.not8 <- as.character(par.not8)

# Sweep 1 was performed in 1970
# Sweep 4 was performed in 1986
# Sweep 5 was performed in 1996
# Sweep 6 was performed in 2000
# Sweep 7 was performed in 2004
# Sweep 8 was performed in 2008

1900+floor((c(1161)-1)/12) # Sweep 5
1900+floor((c(1194,1209)-1)/12) # Sweep 6
1900+floor((c(1250,1266)-1)/12) # Sweep 7
1900+floor((c(1306,1313)-1)/12) # Sweep 8

fill.dat$par.not6n7n8 <- 0
fill.dat$par.not6n7n8[which(fill.dat$bcsid %in% par.not6n7n8)] <- 1

fill.dat$par.not7n8 <- 0
fill.dat$par.not7n8[which(fill.dat$bcsid %in% par.not7n8)] <- 1

fill.dat$par.not8 <- 0
fill.dat$par.not8[which(fill.dat$bcsid %in% par.not8)] <- 1

# double check
length(unique(fill.dat$bcsid[fill.dat$par.not6n7n8==1]))
length(unique(fill.dat$bcsid[fill.dat$par.not7n8==1]))
length(unique(fill.dat$bcsid[fill.dat$par.not8==1]))
# yep, worked

# if someone is in par.not6n7n8 we indicate all rows 1997+
# if someone is in par.not7n8 we indicate all rows 2001+
# if someone is in par.not8 we indicate all rows 2005+

fill.dat$removerow <- 0
fill.dat$removerow[fill.dat$par.not6n7n8==1] <- 1
fill.dat$removerow[par.not7n8==1] <- 1
fill.dat$removerow[par.not8==1] <- 1

table(fill.dat$removerow)
table(fill.dat$removerow)/length(fill.dat$removerow)
# we 'lose' 6.3% of rows
# save this dataset before I remove these rows
write.csv(fill.dat,'filldat_notremoved.csv')

fill.dat <- fill.dat[fill.dat$removerow==0,]

# Let's see what other rows I should also omit
table(fill.dat$no.Par.info) # these were already removed
table(fill.dat$dontknow.Par.info) # these still have to go
table(fill.dat$no.Emp.info) # these as well
table(fill.dat$actype=='Nothing copied') # and these

table(fill.dat$dontknow.Par.info)/length(fill.dat$dontknow.Par.info)
# 3% of remaining person-years
fill.dat <- fill.dat[fill.dat$dontknow.Par.info==0,]

table(fill.dat$no.Emp.info)/length(fill.dat$no.Emp.info)
# 0.4% of remaining person-years
fill.dat <- fill.dat[fill.dat$no.Emp.info==0,]

table(fill.dat$actype=='Nothing copied')/length(fill.dat$actype)
# 1.4% of remaining person-yaers
fill.dat <- fill.dat[fill.dat$actype!='Nothing copied',]

# It is possible that some of the removed rows were not final rows
# but intermediate rows. The rows that came afterwards are
# therefore also not useful, because we want to take the history
# of a person into account
# so let's check that with the lag function

fill.dat$lagyear <- lag1.func(fill.dat$year, fill.dat$bcsid)

# when no intermediate rows were removed, lagyear should be
# current year minus 1
fill.dat$remove2 <- ifelse(fill.dat$lagyear==fill.dat$year-1 |
         is.na(fill.dat$lagyear),0,1)
table(fill.dat$remove2)
# only 2 rows (and the ones that follow them)
# let's have a look at those
View(fill.dat[which(fill.dat$bcsid %in% fill.dat$bcsid[fill.dat$remove2==1]),])

# I remove all those higher rows as well
for(id in fill.dat$bcsid[fill.dat$remove2==1]) {
  
  yearsremove <- fill.dat$year[fill.dat$bcsid==id & fill.dat$remove2==1]
  
  fill.dat$remove2[fill.dat$bcsid==id & fill.dat$year >= yearsremove] <- 1
    
}

table(fill.dat$remove2) # 4 rows in total, as it should be

fill.dat <- fill.dat[fill.dat$remove2==0,]

# some other data handling
fill.dat$no.Emp.info <- NULL
fill.dat$dontknow.Par.info <- NULL
fill.dat$par.not6n7n8 <- NULL
fill.dat$par.not7n8 <- NULL
fill.dat$par.not8 <- NULL
fill.dat$removerow <- NULL
fill.dat$lagyear <- NULL
fill.dat$remove2 <- NULL

# make useable time-varying covariates

# partner status
table(fill.dat$partype)
fill.dat$partner.single <- ifelse(fill.dat$partype=='Single',1,0)
fill.dat$partner.cohab <- ifelse(fill.dat$partype=='Cohabitation',1,0)
fill.dat$partner.married <- ifelse(fill.dat$partype=='Marriage',1,0)

# activity type
fill.dat$job.full <- ifelse(fill.dat$actype=='F/t paid employee (30+ hrs)' |
                              fill.dat$actype=='F/t self-employed',
                            1,0)
fill.dat$job.part <- ifelse(fill.dat$actype=='Employed, but unpaid' |
                              fill.dat$actype=='Employed, not known if FT/PT' |
                              fill.dat$actype=='P/t paid employee (lt 30 hrs)' |
                              fill.dat$actype=='P/t self-employed' |
                              fill.dat$actype=='Self-employed, not known if FT/PT' |
                              fill.dat$actype=='Maternity leave', # only 93 rows
                            1,0)
fill.dat$job.less <- ifelse(fill.dat$actype=='Unemployed seeking work' |
                              fill.dat$actype=='Looking after home/family' |
                              fill.dat$actype=='Temporarily sick/disabled' |
                              fill.dat$actype=='Travelling/Extended holiday' |
                              fill.dat$actype=='Voluntary work' |
                              fill.dat$actype=='Wholly retired', # since these guys are aged 30 or something!
                            1,0)         # which means they can actually return to work
fill.dat$job.edu <- ifelse(fill.dat$actype=='F/t education' |
                             fill.dat$actype=='Government training scheme' |
                             fill.dat$actype=='Part-time education',
                           1,0)
fill.dat$job.perdis <- ifelse(fill.dat$actype=='Permanently sick/disabled',
                              1,0)
fill.dat$job.dontknow <- ifelse(fill.dat$actype=="Don't know/ Not enough info." |
                                  fill.dat$actype=='Other',
                                1,0)

table(fill.dat$job.dontknow) # 2711 person-years with unknown job
table(fill.dat$job.dontknow)/length(fill.dat$job.dontknow)
# 1 % of all person-years left

# let's remove these and then check the lagged years again
fill.dat <- fill.dat[fill.dat$job.dontknow==0,]

fill.dat$lagyear <- lag1.func(fill.dat$year, fill.dat$bcsid)
fill.dat$remove <- ifelse(fill.dat$lagyear==fill.dat$year-1 |
                             is.na(fill.dat$lagyear),0,1)
table(fill.dat$remove) # 342 rows
table(fill.dat$remove)/length(fill.dat$remove) # 0.1% of all person-years

# mark those rows
for(id in fill.dat$bcsid[fill.dat$remove==1]) {
  
  yearsremove <- fill.dat$year[fill.dat$bcsid==id & fill.dat$remove==1]
  yearsremove <- min(yearsremove) # in case there are multiple options
  # everything after the smallest year has to be removed
  
  fill.dat$remove[fill.dat$bcsid==id & fill.dat$year >= yearsremove] <- 1
  
}
table(fill.dat$remove) # 3225 rows
table(fill.dat$remove)/length(fill.dat$remove) # 1.2% of remaining PY

fill.dat <- fill.dat[fill.dat$remove==0,]

fill.dat$remove <- NULL
fill.dat$job.dontknow <- NULL

# that concludes the removing missings bit
# (except that some time-constant also have missing info)
# overall, removing due to missingness has been quite a low
# percentage of total observations so far

# Also make a 'total eduction' variable that counts number of years
# in full-time education (from age 16 onwards)

edu.id <- unique(fill.dat$bcsid[fill.dat$job.edu==1])

fill.dat$totaledu <- 0
for(id in edu.id) {
  
  fill.dat$totaledu[fill.dat$bcsid==id] <- cumsum(fill.dat$job.edu[fill.dat$bcsid==id])
  
  print(id)
}

# to see the distribution of education, let's see it at age 38
# so we don't count people multiple times
table(fill.dat$totaledu[fill.dat$age==38])

# this still needs to be separated by sex
write.csv(fill.dat,'filldat_almostfinal.csv')

fert.bothsex.dat <- read.csv('filldat_almostfinal.csv')

# let's exclude males and check the data
table(fert.bothsex.dat$sex)
fert.dat <- fert.bothsex.dat[fert.bothsex.dat$sex=='Female',]

# what info is useful here?
names(fert.dat)

fert.dat$X <- NULL # is just row info
table(fert.dat$sex) # can now be removed
fert.dat$sex <- NA
table(fert.dat$year)
table(fert.dat$age)
table(fert.dat$pliv) # number of live births per pregnancy
# add stochastically to model
table(fert.dat$birth)
table(fert.dat$totalbirth)
table(fert.dat$partype)
table(fert.dat$actype) # check how we categorized this in data handling
table(fert.dat$J91SEG) # 1 is dont know, 2 is 'not applicable', see employment data
# looks useful actually
table(fert.dat$J8NSSEC) # can be removed
fert.dat$J8NSSEC <- NULL
table(fert.dat$J91SC) # better verion of J91SEG
# see employment file for coding
table(fert.dat$agemother)
table(fert.dat$parity70) # number of siblings at birth
table(fert.dat$agefather) 
table(fert.dat$foreignborn) # vast majority is Britain

length(unique(fert.dat$bcsid)) # 5971

# some of the other variables have missingness as well
# but before we check their missingness
# let's only select individuals for whom we have observations in 1986
id1986 <- unique(fert.dat$bcsid[fert.dat$year==1986])
fert.dat <- fert.dat[which(fert.dat$bcsid %in% id1986),]

length(unique(fert.dat$bcsid)) # 5467

# who has no baseline info in 1986?
# which can be checked by not having siblingsD info
fert.dat2 <- fert.dat[!is.na(fert.dat$siblingsD),]
length(unique(fert.dat2$bcsid)) # n = 4107
# parity70 might not be a bad var to use for multiple imputation
# of the missing sibling info

fert.dat <- fert.dat2
rm(fert.dat2)
length(unique(fert.dat$bcsid)) 

# the ones with missing info
table(fert.dat$income80)/length(fert.dat$income80) # 18% missing
table(fert.dat$region86)/length(fert.dat$region86) # 1% missing
table(fert.dat$income86)/length(fert.dat$income86) # 32% missing
table(fert.dat$FatherSocialClass86)/length(fert.dat$FatherSocialClass86) # 35% missing
table(fert.dat$MotherSocialClass86)/length(fert.dat$MotherSocialClass86) # 34% missing
table(fert.dat$FatherEmployStat86)/length(fert.dat$FatherEmployStat86) # 45% missing
table(fert.dat$MotherEmployStat86)/length(fert.dat$MotherEmployStat86) # 56% missing
table(fert.dat$siblingsize)/length(fert.dat$siblingsize) # 11% missing

# let's have a closer look at this missing information
# lets look by individuals
View(fert.dat[fert.dat$income80=='',])

fert.id.remove <- unique(fert.dat$bcsid[fert.dat$income80=='' | is.na(fert.dat$agemother)])
fert.id <- unique(fert.dat$bcsid)
fert.id.keep <- setdiff(fert.id,fert.id.remove)
fert.dat <- fert.dat[which(fert.dat$bcsid %in% fert.id.keep),]
length(unique(fert.dat$bcsid)) # 3264

# looks like individuals who have missing on one of the above
# variables, tend to have missingness on the other ones as well
# though not always

# based on these findings, let's drop father and mother social class
# and father and mother employment info
# with the rest being optional
fert.dat$FatherSocialClass86 <- NULL
fert.dat$MotherSocialClass86 <- NULL
fert.dat$FatherEmployStat86 <- NULL
fert.dat$MotherEmployStat86 <- NULL
fert.dat$twin <- NULL
fert.dat$scm86 <- NULL
fert.dat$income86 <- NULL
fert.dat$rural86 <- NULL
fert.dat$famdisruption86 <- NULL
fert.dat$c5k45 <- NULL
fert.dat$foreignborn <- NULL
fert.dat$agefather <- NULL
fert.dat$J91SEG <- NULL
fert.dat$siblingsA <- NULL
fert.dat$siblingsC <- NULL

length(unique(fert.dat$bcsid)) 

# recategorize
# age
# agemother
# region86
# income80
# J91SC

# age and agemother into
# 15-22, 23-29, 30+
fert.dat$age1522 <- ifelse(fert.dat$age < 23,1,0)
fert.dat$age2329 <- ifelse(fert.dat$age > 22 & fert.dat$age < 30,1,0)
fert.dat$age30pl <- ifelse(fert.dat$age > 30,1,0)

fert.dat$agemoth1522 <- ifelse(fert.dat$agemother < 23,1,0)
fert.dat$agemoth2329 <- ifelse(fert.dat$agemother > 22 & fert.dat$agemother < 30,1,0)
fert.dat$agemoth30pl <- ifelse(fert.dat$agemother > 30,1,0)

table(fert.dat$region86)
# region86
fert.dat <- fert.dat[fert.dat$region86 != '-1. Unknown',]
fert.dat$region.north <- ifelse(fert.dat$region86=='1. North East' |
                                  fert.dat$region86=='2. North West' |
                                  fert.dat$region86=='3. Yorkshire and Humberside',
                                1,0)
fert.dat$region.midl <- ifelse(fert.dat$region86=='4. East Midlands' |
                                  fert.dat$region86=='5. West Midlands' |
                                  fert.dat$region86=='6. East of England',
                                1,0)
fert.dat$region.lond <- ifelse(fert.dat$region86=='7. London',
                               1,0)
fert.dat$region.south <- ifelse(fert.dat$region86=='8. South East' |
                                 fert.dat$region86=='9. South West',
                               1,0)
fert.dat$region.wales <- ifelse(fert.dat$region86=='10. Wales',
                                1,0)
fert.dat$region.scot <- ifelse(fert.dat$region86=='11. Scotland',
                                1,0)

# Income80
length(unique(fert.dat$bcsid)) # 3225 unique women left.
fert.dat$inc80.low <- ifelse(fert.dat$income80=='under 35pw' |
                               fert.dat$income80=='50-99pw' 
                             ,1,0)
fert.dat$inc80.med <- ifelse(fert.dat$income80=='100-149pw'
                             ,1,0)
fert.dat$inc80.high <- ifelse(fert.dat$income80=='150-199' |
                                fert.dat$income80=='200-249pw' |
                                fert.dat$income80=='250 or more'
                              ,1,0)

# J91SC
table(fert.dat$J91SC)
fert.dat$jobses.high <- ifelse(fert.dat$J91SC=='(I) Professional' |
                                 fert.dat$J91SC=='(II) Managerial-technical' |
                                 fert.dat$J91SC=='3.1',
                               1,0)

fert.dat$jobses.low <- ifelse(fert.dat$J91SC!='(I) Professional' &
                                 fert.dat$J91SC!='(II) Managerial-technical' &
                                 fert.dat$J91SC!='3.1' &
                                fert.dat$J91SC!='Not applicable',
                               1,0)

fert.dat$jobses.nojob <- ifelse(fert.dat$J91SC=='Not applicable',
                              1,0)

# multiple births
table(fert.dat$pliv,fert.dat$age)
100*table(fert.dat$pliv,fert.dat$age)[3,]/table(fert.dat$pliv,fert.dat$age)[2,]
# not a super clear relation between age and multiple births

# let's get lagged versions of the time varying vars
fert.dat$l.birth  <- lag1.func(fert.dat$birth ,fert.dat$bcsid)
fert.dat$l.totalbirth  <- lag1.func(fert.dat$totalbirth,fert.dat$bcsid)
fert.dat$l.totaledu  <- lag1.func(fert.dat$totaledu,fert.dat$bcsid)
fert.dat$l.partner.single  <- lag1.func(fert.dat$partner.single,fert.dat$bcsid)
fert.dat$l.partner.cohab  <- lag1.func(fert.dat$partner.cohab,fert.dat$bcsid)
fert.dat$l.partner.married  <- lag1.func(fert.dat$partner.married,fert.dat$bcsid)
fert.dat$l.job.full  <- lag1.func(fert.dat$job.full,fert.dat$bcsid)
fert.dat$l.job.part  <- lag1.func(fert.dat$job.part,fert.dat$bcsid)
fert.dat$l.job.edu  <- lag1.func(fert.dat$job.edu,fert.dat$bcsid)
fert.dat$l.job.perdis  <- lag1.func(fert.dat$job.perdis,fert.dat$bcsid)
fert.dat$l.job.less  <- lag1.func(fert.dat$job.less,fert.dat$bcsid)
fert.dat$l.jobses.high  <- lag1.func(fert.dat$jobses.high,fert.dat$bcsid)
fert.dat$l.jobses.low  <- lag1.func(fert.dat$jobses.low,fert.dat$bcsid)
fert.dat$l.jobses.nojob  <- lag1.func(fert.dat$jobses.nojob,fert.dat$bcsid)

# let's also have different age categories

# age and agemother into
# 15-22, 23-29, 30+
fert.dat$age1519 <- ifelse(fert.dat$age <= 19,1,0)
fert.dat$age2023 <- ifelse(fert.dat$age >= 20 & fert.dat$age <= 23,1,0)
fert.dat$age2427 <- ifelse(fert.dat$age >= 24 & fert.dat$age <= 27,1,0)
fert.dat$age2831 <- ifelse(fert.dat$age >= 28 & fert.dat$age <= 31,1,0)
fert.dat$age3235 <- ifelse(fert.dat$age >= 32 & fert.dat$age <= 35,1,0)
fert.dat$age36pl <- ifelse(fert.dat$age >= 36,1,0)

write.csv(fert.dat,'fertdat.csv')
