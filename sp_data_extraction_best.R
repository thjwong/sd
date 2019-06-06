## Colorblind-friendly color palette
#cbPalette <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#999999")
#pie(rep(1,length(cbPalette)), col=cbPalette)

#records[records=='\\N'] <- NA
rec20150220 <- read.table(file="../sp/records_20150220_30001.csv", header=T, sep=",", na.strings=c("NA", "-", "?", "\\N"))

rec20150222 <- read.table(file="../sp/records_20150222_30000-20150221-d2x3s264rand.csv", header=T, sep=",", na.strings=c("NA", "-", "?", "\\N"))

rec20150226 <- read.table(file="../sp/records_20150226_30000-20150221-d2x3s264rand.csv", header=T, sep=",", na.strings=c("NA", "-", "?", "\\N"))

rec20150301 <- read.table(file="../sp/records_20150301_30000-20150221-d2x3s264rand.csv", header=T, sep=",", na.strings=c("NA", "-", "?", "\\N"))

records <- rec20150301

#democheck <-  c(502751298, ... )
#completed <- subset(records, sessionid %in% democheck)

## check all attempted the first trial / round in training
firstroundcount <- subset(records, stage == 0 & trial == 1)$sessionid
attempted <- subset(records, sessionid %in% firstroundcount)
## remove those who did 20 rounds of test rounds
whodid20rounds <- subset(records, stage == 1 & trial == 20)$sessionid
attempted <- attempted[!(attempted$sessionid %in% whodid20rounds),]

## make sure the last round / trial of the 2nd stage was completed
## and then defined a completed study by such
lastroundcheck <- subset(records, stage == 1 & trial == 10)$sessionid
completed <- subset(records, sessionid %in% lastroundcheck)

## 2015.11.10
## Found out there are duplication problems in the records for sessionid 1195349427,
## i.e. almost every record of trials in stage 1 & 0 are recorded 2 times.
## The list is selected based on duplicating in the fields of stage & trial & duration,
## as verified by manual checking.
## The row numbers selected to be removed are the second occurrences of these records.
## ***CAUTION*** The row numbers are 'row names' generated from the creation of data.frame 'records',
## actual total number of rows of 'completed' does not exceed 9000. To reset the 'row names' or index,
## one can use [rownames(df) <- NULL] but I didn't do it in this case before looking for these rows.
## Have to remove these rows (remember the COMMA at the end!)
completed.original <- completed
## doesn't work because these are not indices, but 'row names' inherited from data.frame 'records';
## the total number of rows of 'completed' does not exceed 9000
#completed <- completed[-c(9303,9304,9306,9308,9310,9312,9314,9316,9318,9320,9322,9324,9325,9327,9329,9331,9333,9335,9337,9339,9341,9343,9345,9347,9349,9351,9353,9355,9357,9359,9361,9363,9365,9367,9369,9371,9373,9375,9377),, drop=FALSE]
duplist <- c(9303,9304,9306,9308,9310,9312,9314,9316,9318,9320,9322,9324,9325,9327,9329,9331,9333,9335,9337,9339,9341,9343,9345,9347,9349,9351,9353,9355,9357,9359,9361,9363,9365,9367,9369,9371,9373,9375,9377)
duplist <- c(1098,1148,1165,duplist) ## Sessionid[38] repeated the first 3 rounds in stage 1
## select those rows not contained in duplist
completed <- completed[!(rownames(completed) %in% duplist),]
completed.cleaned.original <- completed

## MTurk records exported from task websites of related MTurk HITs
## one number in Batch_1830893_batch_results.csv was '{}' and had to be corrected to numeric form
## e.g. to 0
MTurk20150303 <- read.csv("Batch_1830893_batch_results.csv")
MTurk20150219 <- read.csv("Batch_1828723_batch_results.csv")
MTscode <- MTurk20150303$Answer.surveycode
MTscode2 <- MTurk20150219$Answer.surveycode
MTscodes <- c(MTscode,MTscode2)
## include only the subjects within these MTurk records
completed <- subset(completed, sessionid %in% MTscodes)

## *OR*
## this will include the trial that has an erroneous MTurk surveycode "{}"
## so that number of subjects in uniform condition will change from 53 to 54
completed <- completed.cleaned.original
completed <- subset(completed, trialtotal == 10)

require("ggplot2")
require("reshape2")
require("gridExtra")
require("plyr")

### define names used in graphs here
bgoal <- "best-choice goal"
egoal <- "maximizing goal"
dist11 <- "uniform"
dist14 <- "+ve skew"
dist41 <- "-ve skew"

### for attempted trials / subjects
#bar <- subset(attempted,  stage == 0 & trial == 1, select = c(sessionid,valuepayoff,dist))
### for completed trials / subjects
bar <- subset(completed,  stage == 1 & trial == 10, select = c(sessionid,valuepayoff,dist))
## only select the non-duplicated
bar <- bar[!duplicated(bar$sessionid) , ]
bar$dist[bar$dist==0] <- dist11;
#bar$dist[bar$dist==1] <- "positive";
bar$dist[bar$dist==1] <- dist14;
#bar$dist[bar$dist==2] <- "negative";
bar$dist[bar$dist==2] <- dist41;
#bar$valuepayoff[bar$valuepayoff==0] <- "best only goal";
bar$valuepayoff[bar$valuepayoff==0] <- bgoal;
#bar$valuepayoff[bar$valuepayoff==1] <- "expectation goal";
bar$valuepayoff[bar$valuepayoff==1] <- egoal;

#counts <- table(bar$valuepayoff, bar$dist)
#barplot(counts, main="Participants allocation",
#  xlab="Conditions (distributions)", ylab="Number of participants", col=c("olivedrab","chocolate"),
#  legend = rownames(counts), beside=TRUE)

counts <- as.data.frame(table(bar$valuepayoff, bar$dist))
#counts <- with(counts, counts[rev(order(Var2)),])
#counts$Var2a <- rev(order(counts$Var2))
## specify factor level orders of Var2
counts$Var2 <- ordered(counts$Var2, levels = c("uniform", "+ve skew", "-ve skew"))
#pdf("sp_results150310_participants_summary.pdf")
#pdf("sp_results151020_participants_summary.pdf")
pdf("sp_results151110_participants_summary.pdf")
ggplot(counts,aes(x=Var2,y=Freq,fill=Var1)) +
    geom_bar(stat="identity", position="dodge") +
    xlab("Distributions") +
    ylab("Number of participants") +
    guides(fill=guide_legend(title="Payoff structure")) +
    geom_text(aes(label=Freq,x=Var2,y=Freq),position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

### participants summary for just the best-only goal
bar <- subset(completed, valuepayoff==0 & stage == 1 & trial == 10, select = c(sessionid,dist))
bar$dist[bar$dist==0] <- dist11;
#bar$dist[bar$dist==1] <- "positive";
bar$dist[bar$dist==1] <- dist14;
#bar$dist[bar$dist==2] <- "negative";
bar$dist[bar$dist==2] <- dist41;

counts <- as.data.frame(table(bar$dist))
counts$Var1 <- ordered(counts$Var1, levels = c("-ve skew", "uniform", "+ve skew"))
#pdf("sp_results151209_participants_summary_p1_samefishbowl.pdf")
#pdf("sp_results160114_participants_summary_p1_samefishbowl.pdf")
#pdf("sp_results160125_participants_summary_p1_samefishbowl.pdf")
pdf("sp_results_participants_summary_p1_samefishbowl.pdf")
ggplot(counts,aes(x=Var1,y=Freq)) +
    geom_bar(
        stat="identity",
        position="dodge",
        fill=c("#56B4E9","#009E73","#CC79A7"),
        color=c("#56B4E9","#009E73","#CC79A7")) +
    xlab("Distributions") +
    ylab("Number of participants") +
    #geom_text(aes(label=Freq,x=Var1,y=Freq),position=position_dodge(width=0.9), vjust=-0.25)
    geom_text(
        aes(label=Freq,x=Var1,y=Freq,ymax=Freq+1),
        position=position_dodge(width=0.9),
        vjust=-0.25) +
    theme(legend.position="none") # This removes all legends
dev.off()

### extract sessionid and other parameters of the study from the very last trial
Sessionid <- subset(completed, stage == 1 & trial == 10)$sessionid
Valuepayoff <- subset(completed, stage == 1 & trial == 10)$valuepayoff
Dist <- subset(completed, stage == 1 & trial == 10)$dist

###### score and bonus
### extract the cumulative score and bonuses from the last trial of test phase
Score <- subset(completed, stage == 1 & trial == 10)$score
Bonus <- subset(completed, stage == 1 & trial == 10)$bonus

##############################################################
###### Validation of data by checking subject behaviors ######
##############################################################
### Start to prepare Effort Score Noncandidate data.frame further below
##EffortScoreNoncand <- data.frame(cbind(Sessionid, Valuepayoff, Dist, mean_effort_stage0, mean_effort_stage1, Score, Bonus, Noncandidate_stage0, Noncandidate_stage1, Noncandidate_total, Chosen1stitem_stage0, Chosen1stitem_stage1, Chosen1stitem_total))

#### mean_effort
mean_effort_stage0 <- rep(NA, length(Sessionid))
mean_effort_stage1 <- rep(NA, length(Sessionid))
mean_dtime_stage0 <- rep(NA, length(Sessionid))
mean_dtime_stage1 <- rep(NA, length(Sessionid))
mean_chosen_stage0 <- rep(NA, length(Sessionid))
mean_chosen_stage1 <- rep(NA, length(Sessionid))
for (i in 1:length(Sessionid)) {
    mean_effort_stage0[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 0)$position)
    mean_dtime_stage0[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 0)$duration)
    mean_chosen_stage0[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 0)$chosen)
    mean_effort_stage1[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 1)$position)
    mean_dtime_stage1[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 1)$duration)
    mean_chosen_stage1[i] <- mean(subset(completed, sessionid %in% Sessionid[i] & stage == 1)$chosen)
    gc()
}

#### See how many times did they choose a max-ever-shown number
#### i.e. which was the max ever shown in a trial (legitimate 'candidate')
Noncandidate <- rep(0, length(completed$rid))
## determine per trial
## select number shown s00 to s99 and find out the max
for (i in 1:length(Noncandidate)) {gc();
    if (completed[i,]$chosen == max(as.numeric(completed[i,which(names(completed)=="s00"):(which(names(completed)=="s00")+99)]),na.rm=TRUE)) {
        Noncandidate[i] <- 0
    }
    else {
        ### lastpossibleitem == NA, when it was not reached
        lastpossibleitem <- completed[i,(which(names(completed)=="s00")+(completed[i,]$ntotal-1))];
        ### When the last item of the sequence (==NA) was not reached
        ### & When the chosen item was not the max item shown
        ### Then Noncandidate is committed
        if (is.na(lastpossibleitem))
            Noncandidate[i] <- 1
    }
}
gc();
Noncandidate_stage0 <- rep(0, length(Sessionid))
Noncandidate_stage1 <- rep(0, length(Sessionid))
Noncandidate_total <- rep(0, length(Sessionid))
## count total per participant
for (i in 1:length(Sessionid)) {gc();print(i);
    if (i == 1) k <- 1 ## start k counter at the beginning of Sessionid
    cnt <- 0 ## restart cnt counter for every Sessionid
    ifelse ( k <= 333, k <- 1, k <- (k-333) ) ## rewind 333 on the array in case Sessionid overlaps the previous one
    for (j in k:length(Noncandidate)) {
        if(Sessionid[i] == completed[j,]$sessionid) {
            if(completed[j,]$stage == 0) {
                Noncandidate_stage0[i] <- Noncandidate_stage0[i] + Noncandidate[j]
            }
            else { cnt <- cnt + 1
                Noncandidate_stage1[i] <- Noncandidate_stage1[i] + Noncandidate[j]
            }
            Noncandidate_total[i] <- Noncandidate_total[i] + Noncandidate[j]
            ### last trial: break out the inner for-loop
            ## On subset(completed, stage==1 & trial==20, select=c(happen,sessionid,stage,trial)),
            ## there were 19 subjects who had 20 trials in stage 1,
            ## we are not making use of those 19 extra 10 trials here as
            ## 19 is a smaller number and it will complicate the whole procedure.
            if (completed[j,]$stage == 1 & completed[j,]$trial == completed[j,]$trialtotal) {
                k <- j ## record where the j marker has stopped
                print(paste("count: ",cnt," location: ", k));
                if (cnt != 10) print("Anomaly!")
                break
            }
        }
    }
}
## To calculate number of repetitions of stage 0 (training)
## for every subject completed stage 1 (inside 'completed')
library(plyr)
stage0_rep <- ddply(subset(completed, stage==0 & trial==10, select=c(sessionid,trialtotal)),.(sessionid),nrow)
names(stage0_rep) <- c("sessionid","repcount")
gc();
### convert from count to percentage (out of trialtotal)
Noncandidate_stage0 <- Noncandidate_stage0/(stage0_rep$repcount*10)*100
#names(Noncandidate_stage0) <- "PercentNonCandidate"
#names(Noncandidate_stage0) <- NULL
Noncandidate_stage1 <- Noncandidate_stage1/subset(completed, stage==1 & trial==10, select=trialtotal)*100
#names(Noncandidate_stage1) <- "PercentNoncandidate"
## reset / remove freaking rownames inherited long ago
#rownames(Noncandidate_stage1) <- seq(length=nrow(Noncandidate_stage1))
rownames(Noncandidate_stage1) <- NULL
Noncandidate_stage1 <- Noncandidate_stage1[,1]
Noncandidate_total <- Noncandidate_total/((stage0_rep$repcount*10) + subset(completed, stage==1 & trial==10, select=trialtotal))*100
#names(Noncandidate_total) <- "PercentNoncandidate"
rownames(Noncandidate_total) <- NULL
Noncandidate_total <- Noncandidate_total[,1]

#### Choose 1st
#### See how many time did they choose the first item in a round
Chosen1stitem <- rep(0, length(completed$rid))
## determine per trial
for (i in 1:length(Chosen1stitem)) {gc();
    if (completed[i,]$chosen == as.numeric(completed[i,which(names(completed)=="s00")]))
        Chosen1stitem[i] <- 1
}
gc()
Chosen1stitem_stage0 <- rep(0, length(Sessionid))
Chosen1stitem_stage1 <- rep(0, length(Sessionid))
Chosen1stitem_total <- rep(0, length(Sessionid))
## count total per participant
for (i in 1:length(Sessionid)) {gc();print(i);
    if (i == 1) k <- 1 ## start k counter at the beginning of Sessionid
    cnt <- 0 ## restart cnt counter for every Sessionid
    ifelse ( k <= 333, k <- 1, k <- (k-333) ) ## rewind 333 on the array in case Sessionid overlaps the previous one
    for (j in k:length(Chosen1stitem)) {
        if(Sessionid[i] == completed[j,]$sessionid) {
            if(completed[j,]$stage == 0) {
                Chosen1stitem_stage0[i] <- Chosen1stitem_stage0[i] + Chosen1stitem[j]
            }
            else { cnt <- cnt + 1
                Chosen1stitem_stage1[i] <- Chosen1stitem_stage1[i] + Chosen1stitem[j]
            }
            Chosen1stitem_total[i] <- Chosen1stitem_total[i] + Chosen1stitem[j]
            ### last trial: break out the inner for loop
            if (completed[j,]$stage == 1 & completed[j,]$trial == completed[j,]$trialtotal) {
                k <- j ## record where the j marker has stopped
                print(paste("count: ",cnt," location: ", k));
                if (cnt != 10) print("Anomaly!")
                break
            }
        }
    }
}
## convert from count to percentage (out of trialtotal)
Chosen1stitem_stage0 <- Chosen1stitem_stage0/(stage0_rep$repcount*10)*100
#names(Chosen1stitem_stage0) <- "PercentChosen1stitem"
#names(Chosen1stitem_stage0) <- NULL
Chosen1stitem_stage1 <- Chosen1stitem_stage1/subset(completed, stage==1 & trial==10, select=trialtotal)*100
#names(Chosen1stitem_stage1) <- "PercentChosen1stitem"
#rownames(Chosen1stitem_stage1) <- NULL
Chosen1stitem_stage1 <- Chosen1stitem_stage1[,1]
Chosen1stitem_total <- Chosen1stitem_total/((stage0_rep$repcount*10) + subset(completed, stage==1 & trial==10, select=trialtotal))*100
#names(Chosen1stitem_total) <- "PercentChosen1stitem"
Chosen1stitem_total <- Chosen1stitem_total[,1]

#### Combining data results
EffortScoreNoncand <- data.frame(cbind(Sessionid, Valuepayoff, Dist, mean_effort_stage0, mean_effort_stage1, Score, Bonus, Noncandidate_stage0, Noncandidate_stage1, Noncandidate_total, Chosen1stitem_stage0, Chosen1stitem_stage1, Chosen1stitem_total))

##names(EffortScoreNoncand) <- c("Sessionid","Payoff","Distribution","Effort stage 0","Effort stage 1", "Score", "Bonus", "Commit Non-candidate stage 0", "Commit Non-candidate stage 1", "Commit Non-candidate total")

data <- EffortScoreNoncand;

#data$Dist[data$Dist==0] <- "uniform";
data$Dist[data$Dist==0] <- dist11;
#data$Dist[data$Dist==1] <- "positive skew";
data$Dist[data$Dist==1] <- dist14;
#data$Dist[data$Dist==2] <- "negative skew";
data$Dist[data$Dist==2] <- dist41;

#data$Valuepayoff[data$Valuepayoff==0] <- "best only goal";
data$Valuepayoff[data$Valuepayoff==0] <- bgoal;
#data$Valuepayoff[data$Valuepayoff==1] <- "gain by value goal";
data$Valuepayoff[data$Valuepayoff==1] <- egoal;

## select using subset only rounds with expectation max goal
datae <- subset(data, Valuepayoff==egoal)
datae$Valuepayoff <- NULL

## select using subset only rounds with best-only goal
datab <- subset(data, Valuepayoff==bgoal)
datab$Valuepayoff <- NULL

## use data.frame to combine, cbind returns a matrix and all values must be of the same data type.
dataemelt <- data.frame(as.factor(c(rep(0,length(datae$Chosen1stitem_stage0)),rep(1,length(datae$Chosen1stitem_stage1)))),c(datae$Sessionid,datae$Sessionid),c(datae$Dist,datae$Dist),as.numeric(c(datae$mean_effort_stage0,datae$mean_effort_stage1)),as.numeric(c(datae$Chosen1stitem_stage0,datae$Chosen1stitem_stage1)),as.numeric(c(datae$Noncandidate_stage0,datae$Noncandidate_stage1)))

names(dataemelt) <- c("Stage","Sessionid","Dist","mean_effort","Chosen1stitem","Noncandidate")

## use data.frame to combine, cbind returns a matrix and all values must be of the same data type.
databmelt <- data.frame(as.factor(c(rep(0,length(datab$Chosen1stitem_stage0)),rep(1,length(datab$Chosen1stitem_stage1)))),c(datab$Sessionid,datab$Sessionid),c(datab$Dist,datab$Dist),as.numeric(c(datab$mean_effort_stage0,datab$mean_effort_stage1)),as.numeric(c(datab$Chosen1stitem_stage0,datab$Chosen1stitem_stage1)),as.numeric(c(datab$Noncandidate_stage0,datab$Noncandidate_stage1)))

names(databmelt) <- c("Stage","Sessionid","Dist","mean_effort","Chosen1stitem","Noncandidate")

###### Summarizes data.
###### http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
###### Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
######   data: a data frame.
######   measurevar: the name of a column that contains the variable to be summariezed
######   groupvars: a vector containing names of columns that contain grouping variables
######   na.rm: a boolean that indicates whether to ignore NA's
######   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    # Calculate standard error of the mean (SE or SEM)
    datac$sem <- datac$sd / sqrt(datac$N)

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, df=datac$N-1)
    datac$ci <- datac$sem * ciMult

    return(datac)
}

require(ggplot2)

#pdf("sp_results150310_Effort0.pdf")
#pdf("sp_results151021_Effort0.pdf")
pdf("sp_results151023_Decisionpoint0.pdf")
dfs <- summarySE(data, measurevar="mean_effort_stage0", groupvars=c("Valuepayoff", "Dist"))
## Error bars represent standard error of the mean
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=mean_effort_stage0, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") + coord_flip() +
    geom_errorbar(aes(ymin=mean_effort_stage0-sem, ymax=mean_effort_stage0+sem),
                  width=.2,                    ## Width of the error bars
                  position=position_dodge(.9)) +
    xlab("Distribution") +
    #ylab("Effort in training (N=25)") +
    ylab("Decision point in training (N=25)") +
    scale_fill_discrete(name  ="Pay-off structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 25)
dev.off()

#require("scales")

pdf("sp_results150303_Effort0_bp.pdf")
hline <- data.frame(yint = 8, lt = '1/e point')
#stopFKBayes <- data.frame(stops=c(10,12,10),dist=c("uniform","positive skew","negative skew"))
stopFKBayes <- data.frame(stops=c(10,12,10),dist=c(dist11,"+ve skew","-ve skew"))
#stopMaxExp <- data.frame(stops=c(9,10,9),dist=c("uniform","positive skew","negative skew"))
stopMaxExp <- data.frame(stops=c(9,10,9),dist=c(dist11,"+ve skew","-ve skew"))
stopModels <- data.frame(rbind(stopFKBayes,stopMaxExp))
stopModels$type <- c("FKB","FKB","FKB","MaxExp","MaxExp","MaxExp")
ggplot(data,aes(x=Dist, y=mean_effort_stage0, fill=Valuepayoff)) +
    geom_boxplot(horizontal=TRUE) +
    #stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
    xlab("Distribution") +
    ylab("Effort in training out of N (25)") +
    scale_fill_discrete(name  ="Pay-off structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 25) +
    geom_hline(data = hline,
               aes(yintercept = yint, linetype = lt),
               color = "blue4", size=1, alpha = 0.99, show_guide = TRUE) +
    #geom_line(data=stopModels, aes(x=dist,y=stops,group=type,color="##99##"),show_guide=FALSE)+
    #stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, shape = 10, size = 3, colour = "blue")
    #stat_summary(fun.y = c(11,11,11,11,11,11), shape = 10, size = 3, colour = "blue")
    scale_linetype_manual(name = 'Legend',values = 1,guide = "legend")
dev.off()

pdf("sp_results150303_Effort1_bp.pdf")
hline <- data.frame(yint = 8, lt = '1/e point')
#stopFKBayes <- data.frame(stops=c(10,12,10),dist=c("uniform","positive skew","negative skew"))
stopFKBayes <- data.frame(stops=c(10,12,10),dist=c(dist11,"+ve skew","-ve skew"))
#stopMaxExp <- data.frame(stops=c(9,10,9),dist=c("uniform","positive skew","negative skew"))
stopMaxExp <- data.frame(stops=c(9,10,9),dist=c(dist11,"+ve skew","-ve skew"))
stopModels <- data.frame(rbind(stopFKBayes,stopMaxExp))
stopModels$type <- c("FKB","FKB","FKB","MaxExp","MaxExp","MaxExp")
ggplot(data,aes(x=Dist, y=mean_effort_stage1, fill=Valuepayoff)) +
    geom_boxplot(horizontal=TRUE) +
    xlab("Distribution") +
    ylab("Effort in experiment out of N (25)") +
    scale_fill_discrete(name  ="Pay-off structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 25) +
    geom_hline(data = hline,
               aes(yintercept = yint, linetype = lt),
               color = "blue4", size=1, alpha = 0.99, show_guide = TRUE) +
    #geom_line(data=stopModels, aes(x=dist,y=stops,group=type,color="##99##"),show_guide=FALSE)+
    #stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, shape = 10, size = 3, colour = "blue")
    #stat_summary(fun.y = c(11,11,11,11,11,11), shape = 10, size = 3, colour = "blue")
    scale_linetype_manual(name = 'Legend',values = 1,guide = "legend")
dev.off()

#pdf("sp_results150310_Effort1.pdf")
#pdf("sp_results151021_Effort1.pdf")
pdf("sp_results151023_Decisionpoint1.pdf")
dfs <- summarySE(data, measurevar="mean_effort_stage1", groupvars=c("Valuepayoff", "Dist"))
## Error bars represent standard error of the mean
#dfs$Dist <- ordered(dfs$Dist, levels = c(dist11, dist14, dist41))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=mean_effort_stage1, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") + coord_flip() +
    geom_errorbar(aes(ymin=mean_effort_stage1-sem, ymax=mean_effort_stage1+sem),
                  width=.2,                    ## Width of the error bars
                  position=position_dodge(.9)) +
    xlab("Distribution") +
    #ylab("Effort in targets (N=25)") +
    ylab("Decision point in targets (N=25)") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 25)
dev.off()

#pdf("sp_results150310_Score_best.pdf")
#pdf("sp_results151021_Score_best.pdf")
pdf("sp_results_Score_best.pdf")
#data_subset <- subset(data,Valuepayoff=="best only goal")
data_subset <- subset(data,Valuepayoff==bgoal)
data_subset$Score <- data_subset$Score / 10
dfs <- summarySE(data_subset, measurevar="Score", groupvars=c("Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
#ggplot(dfs, aes(x=Dist, y=Score, fill="best only goal")) +
ggplot(dfs, aes(x=Dist, y=Score, fill=bgoal)) +
    geom_bar(position=position_dodge(), fill="olivedrab", stat="identity") +
    geom_errorbar(aes(ymin=Score-sem, ymax=Score+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("% Success with the best-choice goal") +
    xlab("Distribution") +
    #ylab("Score with best only goal")
    ylab("% Success")
dev.off()

#pdf("sp_results150310_Score_expect.pdf")
#pdf("sp_results151021_Score_expect.pdf")
#pdf("sp_results160125_Score_expect.pdf")
pdf("sp_results_Score_expect.pdf")
min.mean.sd.max <- function(x) {
    r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}
data_subset <- subset(data,Valuepayoff==egoal)
data_subset$Valuepayoff <- NULL
dfs <- summarySE(data_subset, measurevar="Score", groupvars=c("Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("-ve skew", "uniform", "+ve skew"))
ggplot(data_subset, aes(x=Dist, y=Score, color=Dist)) +
    theme_set(theme_bw(12)) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(group = Dist), position = 'dodge') +
    geom_jitter(position=position_jitter(0.2)) +
    scale_x_discrete(limits=c("-ve skew", "uniform", "+ve skew")) +
    scale_color_manual(breaks=c("-ve skew", "uniform", "+ve skew"),
                       values=c("#CC79A7", "#56B4E9", "#009E73"),
                       name="Distribution") +
    #ggtitle("Score with the expected-value maximization goal") +
    ggtitle("Score: final sum of all test rounds") +
    xlab("Distribution") +
    ylab("Score total") +
    theme(legend.position="none")
dev.off()



#pdf("sp_results150310_NCcheck1.pdf")
pdf("sp_results151021_NCcheck1.pdf")
dfs <- summarySE(data, measurevar="Noncandidate_stage0", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Noncandidate_stage0, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Noncandidate_stage0-sem, ymax=Noncandidate_stage0+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing a non-candidate in training") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results150310_NCcheck2.pdf")
pdf("sp_results151021_NCcheck2.pdf")
dfs <- summarySE(data, measurevar="Noncandidate_stage1", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Noncandidate_stage1, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Noncandidate_stage1-sem, ymax=Noncandidate_stage1+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing a non-candidate in targets") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results150310_NCcheck3.pdf")
pdf("sp_results151021_NCcheck3.pdf")
dfs <- summarySE(data, measurevar="Noncandidate_total", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Noncandidate_total, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Noncandidate_total-sem, ymax=Noncandidate_total+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing a non-candidate in total") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results160122_NCcheck_expectmax.pdf")
#pdf("sp_results_NCcheck_bestchoice.pdf")
png("sp_results_NCcheck_bestchoice.png",
    width=7, height=7, units='in', res=600)
dfs <- summarySE(databmelt, measurevar="Noncandidate", groupvars=c("Stage", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("-ve skew", "uniform", "+ve skew"))
## results from sp_StimulationPlots.R
######### Determine how often the optimal model choose a non-candidate #########
dfsoptimal <- data.frame(Stage=as.factor(c(0,1,0,1,0,1)),Dist=c("-ve skew","-ve skew","uniform","uniform","+ve skew","+ve skew"),Noncandidate=c(6.84,6.84,9.42,9.42,15.9,15.9))
dfsoptimal$Dist <- ordered(dfsoptimal$Dist, levels = c("-ve skew", "uniform", "+ve skew"))
ggplot(dfs, aes(x=Dist, y=Noncandidate, fill=Stage)) +
    theme_bw(18) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    #theme(axis.line = element_line(color = 'black')) +
    theme(axis.line.x = element_line(color = 'black')) +
    theme(axis.line.y = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    theme(legend.position="bottom") +
    geom_bar(position=position_dodge(), stat="identity") +
    #geom_crossbar(data=dfsoptimal,aes(x=Dist,y=Noncandidate,ymin=Noncandidate,ymax=Noncandidate),color="skyblue",fatten=2.5,show.legend=FALSE) +
    geom_errorbar(aes(ymin=Noncandidate-sem, ymax=Noncandidate+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    #ggtitle("Percentage of choosing a card that was not the largest card ever appeared") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_manual(values=c("#999999","#E69F00"),
                      name  ="Stage",
                      breaks=c(0,1),
                      labels=c("Training", "Test")) +
    ylim(0, 100)
dev.off()

#pdf("sp_results160125_Decisionpoint_expectmax.pdf")
pdf("sp_results_Decisionpoint_bestchoice.pdf")
min.mean.sd.max <- function(x) {
    r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}
ggplot(databmelt, aes(x=Dist, y=mean_effort, color=Stage, shape=Stage)) +
    theme_set(theme_bw(12)) +
    coord_flip() +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(group = Stage), position = 'dodge') +
    geom_jitter(position=position_jitter(0.3), size=2) +
    scale_x_discrete(limits=c("-ve skew", "uniform", "+ve skew")) +
    ## scale_color_manual(breaks=c("-ve skew", "uniform", "+ve skew"),
    ##                    values=c("#CC79A7", "#56B4E9", "#009E73"),
    ##                    name="Distribution") +
    scale_shape_manual(values = c(2, 15), guide=FALSE) +
    scale_color_manual(
        values=c("#E69F00","#0072B2"),
        name="Stage",
        breaks=c(0,1),
        labels=c("Training","Test")
    ) +
    #ggtitle("Score with the expected-value maximization goal") +
    ggtitle("Average decision points of individuals") +
    xlab("Distribution") +
    ylab("Average decision point (N=25)")
dev.off()

#pdf("sp_results150310_CFcheck1.pdf")
pdf("sp_results151021_CFcheck1.pdf")
#pdf("sp_results_CFcheck1.pdf")
dfs <- summarySE(data, measurevar="Chosen1stitem_stage0", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Chosen1stitem_stage0, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Chosen1stitem_stage0-sem, ymax=Chosen1stitem_stage0+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing the first item in training") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results150310_CFcheck2.pdf")
pdf("sp_results151021_CFcheck2.pdf")
dfs <- summarySE(data, measurevar="Chosen1stitem_stage1", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Chosen1stitem_stage1, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Chosen1stitem_stage1-sem, ymax=Chosen1stitem_stage1+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing the first item in targets") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results150310_CFcheck3.pdf")
pdf("sp_results151021_CFcheck3.pdf")
dfs <- summarySE(data, measurevar="Chosen1stitem_total", groupvars=c("Valuepayoff", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("uniform", "+ve skew", "-ve skew"))
ggplot(dfs, aes(x=Dist, y=Chosen1stitem_total, fill=Valuepayoff)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Chosen1stitem_total-sem, ymax=Chosen1stitem_total+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ggtitle("Percentage of choosing the first item in total") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_discrete(name  ="Payoff structure",
                          #breaks=c("best only goal", "gain by value goal"),
                          breaks=c(bgoal, egoal),
                          #labels=c("best only goal", "gain by value goal")) +
                          labels=c(bgoal, egoal)) +
    ylim(0, 100)
dev.off()

#pdf("sp_results160122_CFcheck_expectmax.pdf")
#pdf("sp_results_CFcheck_bestchoice.pdf")
png("sp_results_CFcheck_bestchoice.png",
    width=7, height=7, units='in', res=600)
dfs <- summarySE(databmelt, measurevar="Chosen1stitem", groupvars=c("Stage", "Dist"))
dfs$Dist <- ordered(dfs$Dist, levels = c("-ve skew", "uniform", "+ve skew"))
dfsoptimal <- data.frame(Stage=as.factor(c(0,1,0,1,0,1)),Dist=c("-ve skew","-ve skew","uniform","uniform","+ve skew","+ve skew"),Chosen1stitem=c(3.2,3.2,3.2,3.2,3.2,3.2))
dfsoptimal$Dist <- ordered(dfsoptimal$Dist, levels = c("-ve skew", "uniform", "+ve skew"))
ggplot(dfs, aes(x=Dist, y=Chosen1stitem, fill=Stage)) +
    theme_bw(18) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    #theme(axis.line = element_line(color = 'black')) +
    theme(axis.line.x = element_line(color = 'black')) +
    theme(axis.line.y = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    theme(legend.position="bottom") +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_crossbar(data=dfsoptimal,aes(x=Dist,y=Chosen1stitem,ymin=Chosen1stitem,ymax=Chosen1stitem),color="skyblue",fatten=2.5,show.legend=FALSE) +
    geom_errorbar(aes(ymin=Chosen1stitem-sem, ymax=Chosen1stitem+sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    #ggtitle("Percentage of choosing the first item in training") +
    xlab("Distribution") +
    ylab("% Committing") +
    scale_fill_manual(values=c("#999999","#E69F00"),
                      name  ="Stage",
                      breaks=c(0,1),
                      labels=c("Training", "Test")) +
    ylim(0, 100)
dev.off()
## Below is another way to present dfs of the above graph,
## showing more the sd and variations of each condition.
# http://www.sthda.com/english/wiki/ggplot2-essentials
# http://www.r-bloggers.com/boxplot-with-mean-and-standard-deviation-in-ggplot2-plus-jitter/
# http://stackoverflow.com/questions/16138206/using-gpplot2-to-replicate-sciplot-using-multiple-factor-variables
min.mean.sd.max <- function(x) {
    r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}

p <- ggplot(dataemelt, aes(x=Dist, y=Chosen1stitem, color=Stage, shape=Stage)) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(group = Stage), position = 'dodge') +
    geom_jitter(position=position_jitter(0.2)) +
    scale_x_discrete(limits=c("uniform", "+ve skew", "-ve skew")) +
    scale_shape_manual(values = c(17, 15)) +
    scale_color_manual(values=c("#E69F00","#0072B2"))



###### Decision points frequency by distributions
require(ggplot2)
require(gridExtra)
## completedandcleaned is computed from below by removing the outliers
df <- subset(completedandcleaned, valuepayoff==1 & dist==2 & stage==0, select=c(stage,position))
#asp <- data.frame(seq(0.5,24.5,1),aspb41*length(df$position))
asp <- data.frame(seq(0.5,24.5,1),aspb41*length(df$position))
names(asp) <- c("position","frequency")
p1a <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#56B4E9",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position, y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")
df <- subset(completedandcleaned, valuepayoff==1 & dist==2 & stage==1, select=c(stage,position))
asp <- data.frame(seq(0.5,24.5,1),aspb41*length(df$position))
names(asp) <- c("position","frequency")
p1b <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#56B4E9",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position,y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")
df <- subset(completedandcleaned, valuepayoff==1 & dist==0 & stage==0, select=c(stage,position))
asp <- data.frame(seq(0.5,24.5,1),aspb11*length(df$position))
names(asp) <- c("position","frequency")
p2a <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#009E73",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position,y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")
df <- subset(completedandcleaned, valuepayoff==1 & dist==0 & stage==1, select=c(stage,position))
asp <- data.frame(seq(0.5,24.5,1),aspb11*length(df$position))
names(asp) <- c("position","frequency")
p2b <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#009E73",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position,y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")
df <- subset(completedandcleaned, valuepayoff==1 & dist==1 & stage==0, select=c(stage,position))
asp <- data.frame(seq(0.5,24.5,1),aspb14*length(df$position))
names(asp) <- c("position","frequency")
p3a <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#CC79A7",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position,y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")
df <- subset(completedandcleaned, valuepayoff==1 & dist==1 & stage==1, select=c(stage,position))
asp <- data.frame(seq(0.5,24.5,1),aspb14*length(df$position))
names(asp) <- c("position","frequency")
p3b <- ggplot() +
    theme_bw(24) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_histogram(data=df,
                   aes(x=position),
                   breaks=seq(0,25,1),
                   fill="#CC79A7",
                   col="grey50",
                   alpha=0.5, position="identity") +
    geom_area(data=asp,
              aes(x=position,y=frequency),
              alpha=0.4,
              fill="grey50") +
    xlab("Choice position") +
    ylab("Frequency")

grid.arrange(p1a, p2a, p3a,
             p1b, p2b, p3b,
             ncol=3)

#pdf(onefile=FALSE,
pdf("DecisionPointFreqSkewnegStage0.pdf")
p1a
dev.off()
pdf("DecisionPointFreqSkewnegStage1.pdf")
p1b
dev.off()
pdf("DecisionPointFreqUniformStage0.pdf")
p2a
dev.off()
pdf("DecisionPointFreqUniformStage1.pdf")
p2b
dev.off()
pdf("DecisionPointFreqSkewposStage0.pdf")
p3a
dev.off()
pdf("DecisionPointFreqSkewposStage1.pdf")
p3b
dev.off()


###### effort
###### reaction time / decision time / duration
###### chosen card value
### calculate mean effort, mean duration, mean selected card value per individual (stage 0 & stage 1)
### calculate mean per 3x2 conditions

## pulled from sp_StimulationPlots.R when N <- 25
N <- 25
##from Jonathan: infoForJohn copy.txt
cspb11 <- c(0.068677,0.135,0.19896,0.26057,0.3198,0.37667,0.43117,0.48329,0.53303,0.58038,0.62534,0.6679,0.70805,0.74579,0.7811,0.81397,0.8444,0.87236,0.89784,0.92082,0.94127,0.95916,0.97448,0.98724,1)
cspb14 <- c(0.046105,0.091782,0.13702,0.18181,0.22613,0.26998,0.31333,0.35618,0.3985,0.44028,0.4815,0.52213,0.56215,0.60154,0.64027,0.6783,0.71561,0.75215,0.78789,0.82278,0.8568,0.88993,0.92226,0.95434,1)
cspb41 <- c(0.072606,0.14253,0.20977,0.27432,0.33618,0.39534,0.45181,0.50559,0.55666,0.60502,0.65068,0.69362,0.73384,0.77133,0.80609,0.8381,0.86736,0.89386,0.91757,0.93848,0.95657,0.97179,0.98411,0.99344,1)
cspn01 <- c(0.044955,0.08967,0.13413,0.17833,0.22226,0.2659,0.30924,0.35225,0.39493,0.43725,0.4792,0.52073,0.56183,0.60247,0.64259,0.68217,0.72114,0.75943,0.79698,0.83366,0.86937,0.90391,0.93706,0.96853,1)
fkcumulstopproblist <- list(cspb11,cspb14,cspb41,cspn01)

## for Secretary Problem (Jonathan's for Cayley's problem)
cspSb14 <- sapply(1:25, cumStopProbSecBeta, a=1, b=3.7, N=25)
cspSb11 <- sapply(1:25, cumStopProbSecBeta, a=1, b=1, N=25)
cspSb41 <- sapply(1:25, cumStopProbSecBeta, a=3.7, b=1, N=25)

## count the cumulative values closest to 0.5 from start and I have determined the following stopping positions
## cspSb11[15] == cspSb11[15] == cspSb11[15] and are just > 0.5, at 0.52368351
StopProb50Percentn25 <- c(15,15,15)
## and the expected value of card at that point is the inversed CDF value of 0.52368351
(BetaNormalize(qbeta(0.52368351, a, b), a, b) + 4.0128)*1200

## import files from Jonathan
fkb11 <- read.csv(file="jnbetaNormalizedOneOne_sampleCost0.csv",header=F)
fkb14 <- read.csv(file="jnbetaNormalizedSkewPos1_sampleCost0.csv",header=F)
fkb41 <- read.csv(file="jnbetaNormalizedSkewNeg1_sampleCost0.csv",header=F)
fkn01 <- read.csv(file="jnnorm_sampleCost0.csv",header=F)
fkcondexplist <- list(rev(fkb11[1:N,2]),rev(fkb14[1:N,2]),rev(fkb41[1:N,2]),rev(fkn01[1:N,2]))
ExpectedQuality50PercentStopProbn25 <- c(fkcondexplist[[1]][StopProb50Percentn25[1]],fkcondexplist[[2]][StopProb50Percentn25[2]],fkcondexplist[[3]][StopProb50Percentn25[3]],fkcondexplist[[4]][StopProb50Percentn25[4]])

dumtable <- list(read.csv("tableuni.csv"), read.csv("tablepos.csv"), read.csv("tableneg.csv"))

######################################################
######
######################################################

## an empty data frame with the same variables to store cleaned data
completedandcleaned <- completed[which(is.na(completed$rid)), ]

## clean by removing those outlier rounds with extreme durations (+/- 3*stddev) for each condition and stage
for(payoff in 0:1) {
    for(distribution in 0:2) {
        #fuu0 <- subset(completed, stage == 0 & valuepayoff == payoff & dist == distribution, select=c(sessionid,position,duration,chosen))
        cc0 <- subset(completed, stage == 0 & valuepayoff == payoff & dist == distribution)
        #fuu0totalmean <- mean(fuu0$duration)
        cc0totalmean <- mean(cc0$duration)
        #fuu0totalsd <- sd(fuu0$duration)
        cc0totalsd <- sd(cc0$duration)
        #fuu0clean <- fuu0[fuu0$duration > (fuu0totalmean - 3*fuu0totalsd) & fuu0$duration < (fuu0totalmean + 3*fuu0totalsd),]
        cc0clean <- cc0[cc0$duration > (cc0totalmean - 3*cc0totalsd) & cc0$duration < (cc0totalmean + 3*cc0totalsd),]
        completedandcleaned <- rbind(completedandcleaned, cc0clean)

        #fuu1 <- subset(completed, stage == 1 & valuepayoff == payoff & dist == distribution, select=c(sessionid,position,duration,chosen))
        cc1 <- subset(completed, stage == 1 & valuepayoff == payoff & dist == distribution)
        #fuu1totalmean <- mean(fuu1$duration)
        cc1totalmean <- mean(cc1$duration)
        #fuu1totalsd <- sd(fuu1$duration)
        cc1totalsd <- sd(cc1$duration)
        #fuu1clean <- fuu1[fuu1$duration > (fuu1totalmean - 3*fuu1totalsd) & fuu1$duration < (fuu1totalmean + 3*fuu1totalsd),]
        cc1clean <- cc1[cc1$duration > (cc1totalmean - 3*cc1totalsd) & cc1$duration < (cc1totalmean + 3*cc1totalsd),]
        completedandcleaned <- rbind(completedandcleaned, cc1clean)
    }
}

## create the 3 x condition size x 2 arrays to store the means
distribution <- 3
payoff <- 2

## note the length of sessionid array should actually be just about 1/(3*2) of total Sessionid
mean_effort_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff))
mean_effort_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff))
mean_dtime_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff))
mean_dtime_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff))
mean_chosen_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff))
mean_chosen_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff))
sd_chosen_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff))
sd_chosen_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff))

payoff <- 1
percent_correct_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff))
percent_correct_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff))

percent_correct_stage0_arr <- array(NA, c(distribution, length(Sessionid)))
percent_correct_stage1_arr <- array(NA, c(distribution, length(Sessionid)))

#for(payoff in 1:1) {
for(payoff in 0:1) {
    for(distribution in 0:2) {
        ## get individual means (per payoff per stage) ## [0] points to nothing, so have to +1
        for (i in 1:length(Sessionid)) {
            mean_effort_stage0_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 0 & valuepayoff == payoff & dist == distribution)$position)
            mean_dtime_stage0_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 0 & valuepayoff == payoff & dist == distribution)$duration)
            mean_chosen_stage0_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 0 & valuepayoff == payoff & dist == distribution)$chosen);
            sd_chosen_stage0_arr[(distribution+1),i,(payoff+1)] <- sd(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 0 & valuepayoff == payoff & dist == distribution)$chosen);
            mean_effort_stage1_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 1 & valuepayoff == payoff & dist == distribution)$position)
            mean_dtime_stage1_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 1 & valuepayoff == payoff & dist == distribution)$duration)
            mean_chosen_stage1_arr[(distribution+1),i,(payoff+1)] <- mean(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 1 & valuepayoff == payoff & dist == distribution)$chosen);gc();
            sd_chosen_stage1_arr[(distribution+1),i,(payoff+1)] <- sd(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 1 & valuepayoff == payoff & dist == distribution)$chosen);gc();
        }
    }
}

# These are of different lengths!
Sessionid <- subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 0)$sessionid
Sessionid <- subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 1)$sessionid
Sessionid <- subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 2)$sessionid
Sessionid <- subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 0)$sessionid
Sessionid <- subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 1)$sessionid
Sessionid <- subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 2)$sessionid

# Find the max length among 3 distributions (use only those completed stage 1 trial 10)
Sessionidmaxlength <- max(
#length(subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 0)$sessionid),
#length(subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 1)$sessionid),
#length(subset(completedandcleaned, stage == 0 & trial == 10 & valuepayoff == 0 & dist == 2)$sessionid),
length(subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 0)$sessionid),
length(subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 1)$sessionid),
length(subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == 2)$sessionid)
)

distribution <- 3
payoff <- 1
percent_correct_stage0_arr <- array(NA, c(distribution, Sessionidmaxlength))
percent_correct_stage1_arr <- array(NA, c(distribution, Sessionidmaxlength))

# Do it Stage by Stage manually

Stage <- 0
for(distribution in 0:2) {
    #Sessionid <- subset(completedandcleaned, stage == Stage & trial == 10 & valuepayoff == 0 & dist == distribution)$sessionid
    # Use only those completed stage 1 trial 10
    Sessionid <- subset(completedandcleaned, stage == 1 & trial == 10 & valuepayoff == 0 & dist == distribution)$sessionid
    #print(distribution)
    #print(length(Sessionid))
    for (i in 1:Sessionidmaxlength) {
        if (i <= length(Sessionid))
            percent_correct_stage0_arr[(distribution+1),i] <- sum(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == Stage & trial == 10 & valuepayoff == 0 & dist == distribution)$score)/length(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == Stage & valuepayoff == 0 & dist == distribution)$score)
        else
            break
    }
}

Stage <- 1
for(distribution in 0:2) {
    Sessionid <- subset(completedandcleaned, stage == Stage & trial == 10 & valuepayoff == 0 & dist == distribution)$sessionid
    #print(distribution)
    #print(length(Sessionid))
    for (i in 1:Sessionidmaxlength) {
        if (i <= length(Sessionid))
            percent_correct_stage1_arr[(distribution+1),i] <- sum(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == Stage & trial == 10 & valuepayoff == 0 & dist == distribution)$score)/length(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == Stage & valuepayoff == 0 & dist == distribution)$score)
        else
            break
    }
}

require('reshape2')

# mean scores per round for training and test
tmp0 <- subset(melt(t(percent_correct_stage0_arr)), !is.na(value))
names(tmp0) <- c("Stage","Dist","Score")
tmp0$Stage <- 0
tmp0$Dist <- tmp0$Dist-1
tmp1 <- subset(melt(t(percent_correct_stage1_arr)), !is.na(value))
names(tmp1) <- c("Stage","Dist","Score")
tmp1$Stage <- 1
tmp1$Dist <- tmp1$Dist-1
data <- rbind(tmp0,tmp1)
## May need to do these:
data$Dist[data$Dist==2] <- "Negative skew"
data$Dist[data$Dist==0] <- "Uniform"
data$Dist[data$Dist==1] <- "Positive skew"
data$Stage[data$Stage==0] <- "Training"
data$Stage[data$Stage==1] <- "Test"
data$Dist <- ordered(data$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
data$Stage <- ordered(data$Stage, levels = c("Training", "Test"))
#data[, 'Stage'] <- as.factor(data[, 'Stage'])
#data[, 'Dist'] <- as.factor(data[, 'Dist'])
### http://www.acupsy.com/blog/boxplot-with-overlaid-scatterplots-in-ggplot2-a-hack-for-use-with-a-fill-variable/
## recode x columns as numeric
data$Dist_n[data$Dist == "Positive skew"] <- 1
data$Dist_n[data$Dist == "Uniform"] <- 2
data$Dist_n[data$Dist == "Negative skew"] <- 3
## create a new column with adjustment values called scat_adj
data$scat_adj[data$Stage == "Training"] <- -0.20
data$scat_adj[data$Stage == "Test"] <- 0.20
# get the results from  > tapply(data$Stage, data$Dist, summary)
datatextlabel <- data.frame(Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"), Stage=c("Training","Test","Training","Test","Training","Test"), Score=c("N=74","N=74","N=57","N=57","N=52","N=52"))
datatextlabel$Dist <- ordered(datatextlabel$Dist, levels = c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"))
datatextlabel$Stage <- ordered(datatextlabel$Stage, levels = c("Training","Test","Training","Test","Training","Test"))
## ## average scores for optimal models from line ca. 1170
## ## "./IndividualMeanScoreVsEffort-","payoff",payoff,"stage_means.pdf"
## ## * Capitalization of factors matter! *
## P(win) of one choice-game in known distribution
## Gilbert & Mosteller, 1966, part 3 1st paragraph
## Gilbert & Mosteller, 1966, Table 8
dfsoptimal <- data.frame(Stage=as.factor(c("Training","Test","Training","Test","Training","Test")),Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"),Score=c((0.59*100),(0.59*100),(0.59*100),(0.59*100),(0.59*100),(0.59*100)))
dfsoptimal$Dist <- ordered(dfsoptimal$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
Prandom <- (1/25 * 100)
dfsrandom <- data.frame(Stage=as.factor(c("Training","Test","Training","Test","Training","Test")),Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"),Score=c(Prandom,Prandom,Prandom,Prandom,Prandom,Prandom))
dfsrandom$Dist <- ordered(dfsrandom$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
## normal color grouping should be by Stage, can't remove the line in the scale of shapes

## test to see if the % success values of subjects are equal to the theoretical random probability 4%
wilcox.test(data$Score[data$Stage == "Training" & data$Dist == "Negative skew"], mu = 4, alternative = "two.sided")$p.value
wilcox.test(data$Score[data$Stage == "Training" & data$Dist == "Uniform"], mu = 4, alternative = "two.sided")$p.value
wilcox.test(data$Score[data$Stage == "Training" & data$Dist == "Positive skew"], mu = 4, alternative = "two.sided")$p.value
wilcox.test(data$Score[data$Stage == "Test" & data$Dist == "Negative skew"], mu = 4, alternative = "two.sided")$p.value
wilcox.test(data$Score[data$Stage == "Test" & data$Dist == "Uniform"], mu = 4, alternative = "two.sided")$p.value
wilcox.test(data$Score[data$Stage == "Test" & data$Dist == "Positive skew"], mu = 4, alternative = "two.sided")$p.value

p <- ggplot(data, aes(x=Dist, y=Score, color=Dist, shape=Stage)) +
    theme_bw(12) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line separately
##http://stackoverflow.com/questions/35833307/ggplot2-axis-not-showing-after-using-themeaxis-line-element-line
    theme(axis.line.x = element_line(color="black", size = .5),
      axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.position='none') +
    geom_boxplot(fill="#FFFFFF", outlier.shape=NA) +
    scale_color_manual(values=c("#CC79A7", "#009E73", "#56B4E9"),
                       name="Distribution") +
    scale_shape_manual(values=c(17,15)) +
    scale_x_discrete(labels=c("\nPositive skew\n(N=54)", "\nUniform\n(N=52)", "\nNegative skew\n(N=49)")) +
    geom_text(data=datatextlabel,aes(label=Stage, x=Dist, y=0), vjust=3.3, color="black",
            position = position_dodge(0.9), size=4.5)+
    geom_jitter(aes(x=(Dist_n + scat_adj), color=Dist, shape=Stage), size=2.4, alpha=0.36, position=position_jitter(width=0.1,height=0)) +
    geom_errorbar(data=dfsoptimal,aes(x=Dist,y=Score,ymin=Score,ymax=Score,group=Dist),color="grey30",size=2.4,show.legend=FALSE, linetype=3) +
    geom_errorbar(data=dfsrandom,aes(x=Dist,y=Score,ymin=Score,ymax=Score,group=Dist),color="grey60",size=1.2,show.legend=FALSE, linetype=2) +
    ylim(0,100) +
    ggtitle("% Success in selecting\nthe highest card") +
    xlab("") +
    ylab("% Success")
## Disable clipping in ggplot2 and allow plotting outside of canvas
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
require(grid)
#pdf("sp_results_average_PSuccess_best.pdf")
png("sp_results_average_PSuccess_best.png",
    width=7, height=7, units='in', res=600)
grid.draw(gt)
dev.off()

## create the 3 x condition size x 2 arrays to store the means
distribution <- 3
payoff <- 2
# For figures in CumulativeDensityAverageRoundEffortReached * (below)
N <- 25
effort_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff, N))
effort_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff, N))
effortprop_stage0_arr <- array(NA, c(distribution, length(Sessionid), payoff, N))
effortprop_stage1_arr <- array(NA, c(distribution, length(Sessionid), payoff, N))

for(payoff in 0:1) {
    for(distribution in 0:2) {
        for (i in 1:length(Sessionid)) {
            effort_stage0_arr[(distribution+1),i,(payoff+1),] <- as.vector(table(factor(as.vector(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 0 & valuepayoff == payoff & dist == distribution, select=position)$position),levels=0:24)))
            effort_stage1_arr[(distribution+1),i,(payoff+1),] <- as.vector(table(factor(as.vector(subset(completedandcleaned, sessionid %in% Sessionid[i] & stage == 1 & valuepayoff == payoff & dist == distribution, select=position)$position),levels=0:24)))
        }
    }
}


require('reshape2')

# mean scores per round for training and test
tmp0 <- subset(melt(t(mean_chosen_stage0_arr[,,1])), !is.na(value))
names(tmp0) <- c("Stage","Dist","Score")
tmp0$Stage <- 0
tmp0$Dist <- tmp0$Dist-1
tmp1 <- subset(melt(t(mean_chosen_stage1_arr[,,1])), !is.na(value))
names(tmp1) <- c("Stage","Dist","Score")
tmp1$Stage <- 1
tmp1$Dist <- tmp1$Dist-1
data <- rbind(tmp0,tmp1)
## May need to do these:
data$Dist[data$Dist==2] <- "Negative skew"
data$Dist[data$Dist==0] <- "Uniform"
data$Dist[data$Dist==1] <- "Positive skew"
data$Stage[data$Stage==0] <- "Training"
data$Stage[data$Stage==1] <- "Test"
data$Dist <- ordered(data$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
data$Stage <- ordered(data$Stage, levels = c("Training", "Test"))
#data[, 'Stage'] <- as.factor(data[, 'Stage'])
#data[, 'Dist'] <- as.factor(data[, 'Dist'])
### http://www.acupsy.com/blog/boxplot-with-overlaid-scatterplots-in-ggplot2-a-hack-for-use-with-a-fill-variable/
## recode x columns as numeric
data$Dist_n[data$Dist == "Positive skew"] <- 1
data$Dist_n[data$Dist == "Uniform"] <- 2
data$Dist_n[data$Dist == "Negative skew"] <- 3
## create a new column with adjustment values called scat_adj
data$scat_adj[data$Stage == "Training"] <- -0.20
data$scat_adj[data$Stage == "Test"] <- 0.20
# get the results from  > tapply(data$Stage, data$Dist, summary)
datatextlabel <- data.frame(Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"), Stage=c("Training","Test","Training","Test","Training","Test"), Score=c("N=74","N=74","N=57","N=57","N=52","N=52"))
datatextlabel$Dist <- ordered(datatextlabel$Dist, levels = c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"))
datatextlabel$Stage <- ordered(datatextlabel$Stage, levels = c("Training","Test","Training","Test","Training","Test"))
## average scores for optimal models from line ca. 1170
## "./IndividualMeanScoreVsEffort-","payoff",payoff,"stage_means.pdf"
## * Capitalization of factors matter! *
dfsoptimal <- data.frame(Stage=as.factor(c("Training","Test","Training","Test","Training","Test")),Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"),Score=c(7633,7633,6692,6692,6187,6187))
dfsoptimal$Dist <- ordered(dfsoptimal$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
dfsrandom <- data.frame(Stage=as.factor(c("Training","Test","Training","Test","Training","Test")),Dist=c("Positive skew","Positive skew","Uniform","Uniform","Negative skew","Negative skew"),Score=c(4800,4800,4800,4800,4800,4800))
dfsrandom$Dist <- ordered(dfsrandom$Dist, levels = c("Positive skew", "Uniform", "Negative skew"))
## normal color grouping should be by Stage, can't remove the line in the scale of shapes
#p <- ggplot(data, aes(x=Dist, y=Score, color=Stage)) + geom_boxplot(fill="#FFFFFF")
p <- ggplot(data, aes(x=Dist, y=Score, color=Dist, shape=Stage)) +
    theme_bw(12) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
##http://stackoverflow.com/questions/35833307/ggplot2-axis-not-showing-after-using-themeaxis-line-element-line
    #theme(axis.line = element_line(color = 'black')) +
    theme(axis.line.x = element_line(color="black", size = .5),
      axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.position='none') +
    #theme(plot.margin = unit(c(1,1,4,2), "cm")) +
    #theme(plot.margin = unit(c(2,0.5,1,1), "in")) +
    #geom_crossbar(data=dfsoptimal,aes(x=Dist,y=Score,ymin=Score-500,ymax=Score,group=Dist),color="grey30",fatten=2.4,show.legend=FALSE, linetype=3) +
    geom_errorbar(data=dfsoptimal,aes(x=Dist,y=Score,ymin=Score,ymax=Score,group=Dist),color="grey30",size=2.4,show.legend=FALSE, linetype=3) +
    geom_errorbar(data=dfsrandom,aes(x=Dist,y=Score,ymin=Score,ymax=Score,group=Dist),color="grey60",size=1.2,show.legend=FALSE, linetype=2) +
    #geom_boxplot(fill="#FFFFFF") +
    geom_boxplot(fill="#FFFFFF", outlier.shape=NA) +
    scale_color_manual(values=c("#CC79A7", "#009E73", "#56B4E9"),
                       name="Distribution") +
    scale_shape_manual(values=c(17,15)) +
    #scale_x_discrete(labels=c("\nPositive skew\n(N=74)", "\nUniform\n(N=57)", "\nNegative skew\n(N=52)")) +
    scale_x_discrete(labels=c("\nPositive skew\n(N=71)", "\nUniform\n(N=54)", "\nNegative skew\n(N=48)")) +
    geom_text(data=datatextlabel,aes(label=Stage, x=Dist, y=4500), vjust=3.3, color="black",
            position = position_dodge(0.9), size=4.5)+
    #geom_text(data=datatextlabel,aes(label=Score, x=Dist, y=4500), color="black",
            #position = position_dodge(0.9), size=4.5)+
    geom_jitter(aes(x=(Dist_n + scat_adj), color=Dist, shape=Stage), size=2.4, alpha=0.36, position=position_jitter(width=0.1,height=0)) +
    ylim(4500,8190) +
    ggtitle("Average chosen values") +
    xlab("") +
    ylab("Card value")
## Disable clipping in ggplot2 and allow plotting outside of canvas
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
require(grid)
pdf("sp_results_average_Chosen_best.pdf")
grid.draw(gt)
dev.off()


payoff <- 0

### Individual 'efficiency' as reflected by mean score achieved by mean effort
## Training
#pdf(file=paste("./IndividualMeanScoreVsEffort-","payoff",payoff,"distri",distribution,"stage0.pdf",sep=""))
#pdf(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage0.pdf",sep=""))
png(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage0.png",sep=""),
    width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.8,4.1,1.7))
#plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     xlim=c(1, 25),
     #ylim=c(4500, 10000),
     #ylim=c(4500, 9000),
     #ylim=c(5000, 8500),
     ylim=c(5250, 8250),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     ## xlab="Individual mean search length, training",
     ## ylab="Individual mean value of selected card")
     xlab="Mean search length (training)",
     ylab="Mean value: chosen card")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=5, col="#56B4E9")
## These two lines should be roughly the same, but there is not n = 0.52368351 in the 1 to 25 sequence, there will be a slight difference
## (BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7) + 4.0128)*1200
## (xNormalized(qbeta(0.52368351, 1, 3.7), 1, 3.7) + 4.0128)*1200
## mean(as.matrix(dumtable[[2+1]])) + sd(as.matrix(dumtable[[2+1]]))*(BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7))), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 6095.587, cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=1, col="#009E73")
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 1, 1), 1, 1))), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 6519.619, cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=0, col="#CC79A7")
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 3.7, 1), 3.7, 1))), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 7322.402, cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=0, col="#CC79A7")
dev.off()

### Individual 'efficiency' as reflected by mean percentile achieved by mean effort
## Training
png(file=paste("./IndividualMeanPercentileVsEffort-","payoff",payoff,"stage0.png",sep=""),
    width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.8,4.1,1.7))
#plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     xlim=c(1, 25),
     ylim=c(0.4, 1),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     ## xlab="Individual mean search length, training",
     ## ylab="Individual mean value of selected card")
     xlab="Mean search length (training)",
     ylab="Mean percentile: chosen cards")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[3]]),mean_chosen_stage0_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=5, col="#56B4E9")
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[3]]),6095.587), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[3]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[1]]),mean_chosen_stage0_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=1, col="#009E73")
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[1]]),6519.619), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[1]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[2]]),mean_chosen_stage0_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=0, col="#CC79A7")
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[2]]),7322.402), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[2]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=0, col="#CC79A7")
dev.off()

## Training Z-score tranformed
pdf(file=paste("./IndividualMeanScoreVsEffort-","payoff",payoff,"stage0Z.pdf",sep=""))
plot(5,5, type='n',
     xlim=c(0, 25),
     ylim=c(-2.5, 2.5),
     xlab="Individual mean effort per round",
     ylab="Individual Z-score transformed mean score per round")
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
points(((mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=5, col="#56B4E9")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=9, col="#56B4E9")
#points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=6, lwd=2, pch=5, col="#56B4E9")
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=5, col="#56B4E9")
payoff <- 1
distribution <- 0 ## tableuni in sp_learn.php
points(((mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=1, col="#009E73")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=10, col="#009E73")
#points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=6, lwd=2, pch=1, col="#009E73")
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=1, col="#009E73")
payoff <- 1
distribution <- 1 ## tablepos in sp_learn.php
points(((mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=0, col="#CC79A7")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=12, col="#CC79A7")
#points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=6, lwd=2, pch=0, col="#CC79A7")
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=0, col="#CC79A7")
#legendpars <- c(expression(paste("Individual Z-score transformed means at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual Z-score transformed means at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual Z-score transformed eans at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")))
#legend(x="topright",legendpars,pt.cex=c(1,2,2,1,2,2,1,2,2),pt.lwd=c(1,2,2,1,2,2,1,2,2),pch=c(5,5,9,1,1,10,0,0,12),col=c("#56B4E9","#56B4E9","#56B4E9","#009E73","#009E73","#009E73","#CC79A7","#CC79A7","#CC79A7"))
dev.off()

payoff <- 0

## Test
#pdf(file=paste("./IndividualMeanScoreVsEffort-","payoff",payoff,"distri",distribution,"stage1.pdf",sep=""))
#pdf(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage1.pdf",sep=""))
png(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage1.png",sep=""),
    width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.8,4.1,1.7))
#plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     xlim=c(1, 25),
     #ylim=c(4500, 10000),
     #ylim=c(4500, 9000),
     #ylim=c(5000, 8500),
     ylim=c(5250, 8250),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     ## xlab="Individual mean search length, test",
     ## ylab="Individual mean value of selected card")
     xlab="Mean search length (test)",
     ylab="Mean value: chosen cards")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
## These two lines should be roughly the same, but there is not n = 0.52368351 in the 1 to 25 sequence, there will be a slight difference
## (BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7) + 4.0128)*1200
## (xNormalized(qbeta(0.52368351, 1, 3.7), 1, 3.7) + 4.0128)*1200
## mean(as.matrix(dumtable[[2+1]])) + sd(as.matrix(dumtable[[2+1]]))*(BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 1, 3.7), 1, 3.7))), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 6095.587, cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
distribution <- 0 ## tableuni in sp_learn.php
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 1, 1), 1, 1))), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E7E", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 6519.619, cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
distribution <- 1 ## tablepos in sp_learn.php
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*(BetaNormalize(qbeta(0.52368351, 3.7, 1), 3.7, 1))), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 7322.402, cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
dev.off()

## Test
png(file=paste("./IndividualMeanPercentileVsEffort-","payoff",payoff,"stage1.png",sep=""),
    width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.8,4.1,1.7))
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     xlim=c(1, 25),
     ylim=c(0.4, 1),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     ## xlab="Individual mean search length, test",
     ## ylab="Individual mean value of selected card")
     xlab="Mean search length (test)",
     ylab="Mean percentile: chosen cards")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[3]]),mean_chosen_stage1_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[3]]),6095.587), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[3]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
distribution <- 0 ## tableuni in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[1]]),mean_chosen_stage1_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[1]]),6519.619), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[1]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
distribution <- 1 ## tablepos in sp_learn.php
points(ecdf_fun(as.matrix(dumtable[[2]]),mean_chosen_stage1_arr[(distribution+1),,(payoff+1)]) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[2]]),7322.402), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[2]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
dev.off()

## Test Z-score tranformed
pdf(file=paste("./IndividualMeanScoreVsEffort-","payoff",payoff,"stage1Z.pdf",sep=""))
plot(5,5, type='n',
     xlim=c(0, 25),
     ylim=c(-2.5, 2.5),
     xlab="Individual mean effort per round",
     ylab="Individual Z-score transformed mean score per round")
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
points(((mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=5, col="#56B4E9")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=9, col="#56B4E9")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=5, col="#56B4E9")
payoff <- 1
distribution <- 0 ## tableuni in sp_learn.php
points(((mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=1, col="#009E73")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=10, col="#009E73")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=1, col="#009E73")
payoff <- 1
distribution <- 1 ## tablepos in sp_learn.php
points(((mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] - mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]))/sd(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])) ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=0, col="#CC79A7")
points(StopProb50Percentn25[(distribution+1)], ExpectedQuality50PercentStopProbn25[(distribution+1)], cex=2, lwd=2, pch=12, col="#CC79A7")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), 0, cex=6, lwd=2, pch=0, col="#CC79A7")
#legendpars <- c(expression(paste("Individual Z-score transformed means at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual Z-score transformed means at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual Z-score transformed eans at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")))
#legend(x="topright",legendpars,pt.cex=c(1,2,2,1,2,2,1,2,2),pt.lwd=c(1,2,2,1,2,2,1,2,2),pch=c(5,5,9,1,1,10,0,0,12),col=c("#56B4E9","#56B4E9","#56B4E9","#009E73","#009E73","#009E73","#CC79A7","#CC79A7","#CC79A7"))
dev.off()

pdf(file=paste("./IndividualMeanScoreVsEffort-","payoff",payoff,"stage_all.pdf",sep=""))
par(mar=c(5.1,5.6,4.1,2.1))
plot(5,5, type='n',cex.lab=2,cex.axis=1.5,
     xlim=c(0, 25),
     #ylim=c(4500, 10000),
     ylim=c(5000, 8500),
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     xlab="Individual mean effort per round",
     ylab="Individual mean score per round")
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=5, col=adjustcolor("#56B4E9", offset=c(0.2,0.2,0.2,0)))
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=5, col="#56B4E9")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=2, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
payoff <- 1
distribution <- 0 ## tableuni in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=1, col=adjustcolor("#009E73", offset=c(0.2,0.2,0.2,0)))
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=1, col="#009E73")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=2, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.2,-.2,-.2,0)))
payoff <- 1
distribution <- 1 ## tablepos in sp_learn.php
points(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage0_arr[(distribution+1),,(payoff+1)], cex=.75, pch=0, col=adjustcolor("#CC79A7", offset=c(0.2,0.2,0.2,0)))
points(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)] ~ mean_effort_stage1_arr[(distribution+1),,(payoff+1)], cex=.75, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=0, col="#CC79A7")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=4.5, lwd=2, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=2, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.2,-.2,-.2,0)))
dev.off()

payoff <- 0

#pdf(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage_means.pdf",sep=""))
png(file=paste("./IndividualMeanValueVsEffort-","payoff",payoff,"stage_means.png",sep=""),
     width=7, height=7, units='in', res=600)
#par(mar=c(5.1,5.6,4.1,2.1))
par(mar=c(5.1,4.9,4.1,1.7))
#plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     xlim=c(1, 25),
     #ylim=c(4500, 10000),
     #ylim=c(5000, 8500),
     ylim=c(5250, 8250),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     xlab="Aggregate mean search length",
     ylab="Aggregate mean value: chosen card")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=5, col="#56B4E9")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
## For the moment computed by simulations...
points(14.3, 6095.587, cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
distribution <- 0 ## tableuni in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=1, col="#009E73")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.2,-.2,-.2,0)))
## For the moment computed by simulations...
points(14.3, 6519.619, cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
distribution <- 1 ## tablepos in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=0, col="#CC79A7")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]), cex=3.6, lwd=2, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
#points(StopProb50Percentn25[(distribution+1)], (mean(as.matrix(dumtable[[(distribution+1)]]))+sd(as.matrix(dumtable[[(distribution+1)]]))*ExpectedQuality50PercentStopProbn25[(distribution+1)]), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.2,-.2,-.2,0)))
## For the moment computed by simulations...
points(14.3, 7322.402, cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
dev.off()

png(file=paste("./IndividualMeanPercentileVsEffort-","payoff",payoff,"stage_means.png",sep=""),
     width=7, height=7, units='in', res=600)
#par(mar=c(5.1,5.6,4.1,2.1))
par(mar=c(5.1,4.9,4.1,1.7), xpd=TRUE)
#plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
plot(5,5, type='n',cex.lab=2.4,cex.axis=2.4,
     #xlim=c(1, 25),
     xlim=c(10, 15),
     #ylim=c(0.4, 1),
     ylim=c(0.7, 1),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Performance by score over effort",
     #xlab="Aggregate search length",
     xlab="Mean search length",
     #ylab="Aggregate mean percentile")
     ylab="Mean percentile")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[3]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=5, col="#56B4E9")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[3]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=23, col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[3]]),6095.587), cex=3.3, lwd=2, pch=9, col=adjustcolor("#56B4E9", offset=c(-.3,-.3,-.3,0)))
distribution <- 0 ## tableuni in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[1]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=1, col="#009E73")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[1]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=21, col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[1]]),6519.619), cex=3.3, lwd=2, pch=10, col=adjustcolor("#009E73", offset=c(-.3,-.3,-.3,0)))
distribution <- 1 ## tablepos in sp_learn.php
points(mean(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[2]]),mean(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=0, col="#CC79A7")
points(mean(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]), ecdf_fun(as.matrix(dumtable[[2]]),mean(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])])), cex=3.6, lwd=2, pch=22, col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
## For the moment computed by simulations...
points(14.3, ecdf_fun(as.matrix(dumtable[[2]]),7322.402), cex=3.3, lwd=2, pch=12, col=adjustcolor("#CC79A7", offset=c(-.3,-.3,-.3,0)))
legendpars <- c(
    expression(paste("Positive skew - Training")),expression(paste("Positive skew - Test")),expression(paste("Positive skew - Optimal strategy")),
    expression(paste("Uniform distribution - Training")),expression(paste("Uniform distribution - Test")),expression(paste("Uniform distribution - Optimal strategy")),
    expression(paste("Negative skew - Training")),expression(paste("Negative skew - Test")),expression(paste("Negative skew - Optimal strategy"))
)
legend("bottomright",inset=.05,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(2,2,2,2,2,2,2,3,2),pt.lwd=c(2,2,2,2,2,2,2,2,2),pch=c(0,15,12,1,16,10,5,18,9),col=c("#CC79A7","#CC79A7","#CC79A7","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9"),cex=1.0, bty="1", title="Mean of individual mean performance")
dev.off()

#pdf(file=paste("./IndividualMeanValueVsEffort-legend_all.pdf"))
png(file=paste("./IndividualMeanPercentileVsEffort-legend_all.png"),
    width=7, height=7, units='in', res=600)
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", cex.=2.4, axes=FALSE, xlab="", ylab="")
legendpars <- c(
    expression(paste("Mean percentiles (training, +ve skew)")),expression(paste("Mean percentiles in (test, +ve skew)")),expression(paste("Aggregate mean (training, +ve skew)")),expression(paste("Aggregate mean (test, +ve skew)")),expression(paste("Mean with optimal strategy (+ve skew)")),
    expression(paste("Mean percentiles (training, uniform)")),expression(paste("Mean percentiles (test, uniform)")),expression(paste("Aggregate mean (training, uniform)")),expression(paste("Aggregate mean (test, uniform)")),expression(paste("Mean with optimal strategy (uniform)")),
    expression(paste("Mean percentiles (training, -ve skew)")),expression(paste("Mean percentiles (test, -ve skew)")),expression(paste("Aggregate mean (training, -ve skew)")),expression(paste("Aggregate mean (test, -ve skew)")),expression(paste("Mean with optimal strategy (-ve skew)"))
)
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,1,3,3,3,1,1,3,3,3,1,1,3,4,3),pt.lwd=c(1,1,2,2,2,1,1,2,2,2,1,1,2,2,2),pch=c(0,15,0,15,12,1,16,1,16,10,5,18,5,18,9),col=c("#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9"),cex=2.0, bty="n", title="")
dev.off()

### nonparametric tests
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
SLnegtrain<- mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]
SLnegtest <- mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]
distribution <- 0 ## tableuni in sp_learn.php
SLunitrain <- mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]
SLunitest <- mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]
distribution <- 1 ## tablepos in sp_learn.php
SLpostrain <- mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])]
SLpostest <- mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])]

wilcox.test(SLnegtrain, mu = 14.3, alternative = "two.sided")$p.value
wilcox.test(SLnegtest, mu = 14.3, alternative = "two.sided")$p.value
wilcox.test(SLunitrain, mu = 14.3, alternative = "two.sided")$p.value
wilcox.test(SLunitest, mu = 14.3, alternative = "two.sided")$p.value
wilcox.test(SLpostrain, mu = 14.3, alternative = "two.sided")$p.value
wilcox.test(SLpostest, mu = 14.3, alternative = "two.sided")$p.value

kdata <- list(g1=SLnegtest,g2=SLunitest,g3=SLpostest)
kruskal.test(kdata)

payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
CVnegtrain <- mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]
CVnegtest <- mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]
distribution <- 0 ## tableuni in sp_learn.php
CVunitrain <- mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]
CVunitest <- mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]
distribution <- 1 ## tablepos in sp_learn.php
CVpostrain <- mean_chosen_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage0_arr[(distribution+1),,(payoff+1)])]
CVpostest <- mean_chosen_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_chosen_stage1_arr[(distribution+1),,(payoff+1)])]

wilcox.test(CVnegtrain,CVnegtest,paired=TRUE)
wilcox.test(CVunitrain,CVunitest,paired=TRUE)
wilcox.test(CVpostrain,CVpostest,paired=TRUE)

pdf(file=paste("./IndividualMeanScoreVsEffort-legend_pos.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(
    expression(paste("Individual means in training rounds")),expression(paste("Individual means in test rounds")),expression(paste("Aggregate mean in training rounds")),expression(paste("Aggregate mean in test rounds")),expression(paste("Mean with optimal strategy"))
)
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,1,3,3,3),pt.lwd=c(1,1,2,2,2),pch=c(0,15,0,15,12),col=c("#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7"),cex=1.5, bty="n", title="Positive skew distribution")
dev.off()

pdf(file=paste("./IndividualMeanScoreVsEffort-legend_uni.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(
    expression(paste("Individual means in training rounds")),expression(paste("Individual means in test rounds")),expression(paste("Aggregate mean in training rounds")),expression(paste("Aggregate mean in test rounds")),expression(paste("Mean with optimal strategy"))
)
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,1,3,3,3),pt.lwd=c(1,1,2,2,2),pch=c(1,16,1,16,10),col=c("#009E73","#009E73","#009E73","#009E73","#009E73"),cex=1.5, bty="n", title="Uniform distribution")
dev.off()

pdf(file=paste("./IndividualMeanScoreVsEffort-legend_neg.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(
    expression(paste("Individual means in training rounds")),expression(paste("Individual means in test rounds")),expression(paste("Aggregate mean in training rounds")),expression(paste("Aggregate mean in test rounds")),expression(paste("Mean with optimal strategy"))
)
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,1,3,4,3),pt.lwd=c(1,1,2,2,2),pch=c(5,18,5,18,9),col=c("#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9"),cex=1.5, bty="n", title="Negative skew distribution")
dev.off()


pdf(file=paste("./IndividualMeanScoreVsEffort-legend.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(expression(paste("Individual means at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual means at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual means at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")))
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,2,2,1,2,2,1,2,2),pt.lwd=c(1,2,2,1,2,2,1,2,2),pch=c(5,5,9,1,1,10,0,0,12),col=c("#56B4E9","#56B4E9","#56B4E9","#009E73","#009E73","#009E73","#CC79A7","#CC79A7","#CC79A7"),cex=1.5)
dev.off()

pdf(file=paste("./IndividualMeanScoreVsEffort-legend-old.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(expression(paste("Individual means/Z-score transformed means at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual means/Z-score transformed means at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")),expression(paste("Individual means/Z-score transformed means at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Mean of individual means")),expression(paste("Theoretical mean with optimal strategy")))
legend(1,1,xjust=0.5,yjust=0.5,legendpars,pt.cex=c(1,2,2,1,2,2,1,2,2),pt.lwd=c(1,2,2,1,2,2,1,2,2),pch=c(5,5,9,1,1,10,0,0,12),col=c("#56B4E9","#56B4E9","#56B4E9","#009E73","#009E73","#009E73","#CC79A7","#CC79A7","#CC79A7"))
dev.off()


### Percentage of rounds reaching certain position on average VS Cumulative stopping probability
## using ggplot2
require("reshape2")
## Training
payoff <- 0
distribution <- 2 ###### tableneg in sp_learn.php
effortpropmean_stage0_b41 <- rep(0, 25)
# empty matrix with 25 columns
effortpropmean_stage0_b41_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage0_b41 <- 1
effortreaching_stage0_b41 <- NULL
effortreaching_stage0_b41_upper <- NULL
effortreaching_stage0_b41_lower <- NULL
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    # count only when
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0_b41 <- effortpropmean_stage0_b41 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage0_b41_M <- rbind(effortpropmean_stage0_b41_M, effortprop_stage0_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0_b41 <- effortpropmean_stage0_b41 / validcasecount
# cumulative procedure
for(i in 1:25) {
    #effortreaching_stage0_b41 <- c(effortreaching_stage0_b41, (1-sum(effortpropmean_stage0[1:i])))
    #effortreaching_stage0_b41 <- c(effortreaching_stage0_b41, sum(effortpropmean_stage0[1:i]))
    effortreaching_stage0_b41 <- c(effortreaching_stage0_b41, sum(apply(effortpropmean_stage0_b41_M, 2, mean)[1:i]))
    effortreaching_stage0_b41_upper <- c(effortreaching_stage0_b41_upper, sum((apply(effortpropmean_stage0_b41_M, 2, mean) + apply(effortpropmean_stage0_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage0_b41_lower <- c(effortreaching_stage0_b41_lower, sum((apply(effortpropmean_stage0_b41_M, 2, mean) - apply(effortpropmean_stage0_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
distribution <- 0 ###### tableuni in sp_learn.php
effortpropmean_stage0_b11 <- rep(0, 25)
effortpropmean_stage0_b11_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage0_b11 <- 1
effortreaching_stage0_b11 <- NULL
effortreaching_stage0_b11_upper <- NULL
effortreaching_stage0_b11_lower <- NULL
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0_b11 <- effortpropmean_stage0_b11 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage0_b11_M <- rbind(effortpropmean_stage0_b11_M, effortprop_stage0_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0_b11 <- effortpropmean_stage0_b11 / validcasecount
for(i in 1:25) {
    #effortreaching_stage0_b11 <- c(effortreaching_stage0_b11, (1-sum(effortpropmean_stage0[1:i])))
    effortreaching_stage0_b11 <- c(effortreaching_stage0_b11, sum(apply(effortpropmean_stage0_b11_M, 2, mean)[1:i]))
    effortreaching_stage0_b11_upper <- c(effortreaching_stage0_b11_upper, sum((apply(effortpropmean_stage0_b11_M, 2, mean) + apply(effortpropmean_stage0_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage0_b11_lower <- c(effortreaching_stage0_b11_lower, sum((apply(effortpropmean_stage0_b11_M, 2, mean) - apply(effortpropmean_stage0_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
distribution <- 1 ###### tablepos in sp_learn.php
effortpropmean_stage0_b14 <- rep(0, 25)
effortpropmean_stage0_b14_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage0_b14 <- 1
effortreaching_stage0_b14 <- NULL
effortreaching_stage0_b14_upper <- NULL
effortreaching_stage0_b14_lower <- NULL
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0_b14 <- effortpropmean_stage0_b14 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage0_b14_M <- rbind(effortpropmean_stage0_b14_M, effortprop_stage0_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0_b14 <- effortpropmean_stage0_b14 / validcasecount
for(i in 1:25) {
    #effortreaching_stage0_b14 <- c(effortreaching_stage0_b14, (1-sum(effortpropmean_stage0[1:i])))
    effortreaching_stage0_b14 <- c(effortreaching_stage0_b14, sum(apply(effortpropmean_stage0_b14_M, 2, mean)[1:i]))
    effortreaching_stage0_b14_upper <- c(effortreaching_stage0_b14_upper, sum((apply(effortpropmean_stage0_b14_M, 2, mean) + apply(effortpropmean_stage0_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage0_b14_lower <- c(effortreaching_stage0_b14_lower, sum((apply(effortpropmean_stage0_b14_M, 2, mean) - apply(effortpropmean_stage0_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
effortpropmean_stage0_er41 <- data.frame(x=1:length(effortreaching_stage0_b41), y=effortreaching_stage0_b41*100)
effortpropmean_stage0_om41 <- data.frame(x=1:length(cspSb41[1:25]*100), y=cspSb41[1:25]*100)
effortpropmean_stage0_com41 <- data.frame(x=1:length(cspb41[1:25]*100), y=cspb41[1:25]*100)
effortpropmean_stage0_er11 <- data.frame(x=1:length(effortreaching_stage0_b11), y=effortreaching_stage0_b11*100)
effortpropmean_stage0_om11 <- data.frame(x=1:length(cspSb11[1:25]*100), y=cspSb11[1:25]*100)
effortpropmean_stage0_com11 <- data.frame(x=1:length(cspb11[1:25]*100), y=cspb11[1:25]*100)
effortpropmean_stage0_er14 <- data.frame(x=1:length(effortreaching_stage0_b14), y=effortreaching_stage0_b14*100)
effortpropmean_stage0_om14 <- data.frame(x=1:length(cspSb14[1:25]*100), y=cspSb14[1:25]*100)
effortpropmean_stage0_com14 <- data.frame(x=1:length(cspb14[1:25]*100), y=cspb14[1:25]*100)
effortpropmean_stage0_zz <- melt(list(er41=effortpropmean_stage0_er41,om41=effortpropmean_stage0_om41,com41=effortpropmean_stage0_com41,er11=effortpropmean_stage0_er11,om11=effortpropmean_stage0_om11,com11=effortpropmean_stage0_com11,er14=effortpropmean_stage0_er14,om14=effortpropmean_stage0_om14,com14=effortpropmean_stage0_com14), id.vars="x")
effortpropmean_stage0_zz$L1 <- factor(effortpropmean_stage0_zz$L1, levels = c("er41", "om41", "com41", "er11", "om11", "com11", "er14", "om14", "com14"), ordered=TRUE)

#pdf(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage0.pdf",sep=""))
png(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage0.png",sep=""),
     width=7, height=7, units='in', res=600)
ggplot(data = effortpropmean_stage0_zz, aes(x=x,y=value, color=L1, shape=L1, linetype=L1)) +
    theme_bw(12) +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    theme(axis.line = element_line(color = 'black')) +
    ## draws x and y axis line separately for a bug
    #theme(axis.line.x = element_line(color="black", size = .5),
    #  axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er41")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er41")), aes(color = L1, shape = L1), size=4) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om41")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er11")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er11")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om11")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er14")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er14")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om14")), aes(color = L1, linetype = L1), lwd=0.75) +
    xlab("Position in training rounds") +
    ylab("Cumulative selection probability") +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    theme(legend.position="none") +
    scale_linetype_manual(name = "",
                          breaks=c("er41","om41","er11","om11","er14","om14"),
                          labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                          values=c("er41"=1, "om41"=1,
                                   "er11"=1, "om11"=1,
                                   "er14"=1, "om14"=1)
                          ) +
    scale_shape_manual(name = "",
                       breaks=c("er41","om41","er11","om11","er14","om14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"=5, "om41"=18,
                                "er11"=1, "om11"=19,
                                "er14"=0, "om14"=15)
                       ) +
    scale_color_manual(name = "",
                       breaks=c("er41","om41","er11","om11","er14","om14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"="#56B4E9", "om41"="#000000",
                                "er11"="#009E73", "om11"="#000000",
                                "er14"="#CC79A7", "om14"="#000000")
                       )
dev.off()

## Added optimal lines of expectation goals for comparison
png(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage0com.png",sep=""),
     width=7, height=7, units='in', res=600)
ggplot(data = effortpropmean_stage0_zz, aes(x=x,y=value, color=L1, shape=L1, linetype=L1)) +
    theme_bw(12) +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line separately for a bug
    theme(axis.line = element_line(color = 'black')) +
    #theme(axis.line.x = element_line(color="black", size = .5),
    #  axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er41")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er41")), aes(color = L1, shape = L1), size=4) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om41")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("com41")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er11")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er11")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om11")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("com11")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("er14")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage0_zz, L1 %in% c("er14")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("om14")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage0_zz, L1 %in% c("com14")), aes(color = L1, linetype = L1), lwd=0.75) +
    xlab("Position in training rounds") +
    ylab("Cumulative selection probability") +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    theme(legend.position="none") +
    scale_linetype_manual(name = "",
                          breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                          labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                          values=c("er41"=1, "om41"=1, "com41"=3,
                                   "er11"=1, "om11"=1, "com11"=4,
                                   "er14"=1, "om14"=1, "com14"=2)
                          ) +
    scale_shape_manual(name = "",
                       breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"=5, "om41"=18, "com41"=18,
                                "er11"=1, "om11"=19, "com11"=19,
                                "er14"=0, "om14"=15, "com14"=15)
                       ) +
    scale_color_manual(name = "",
                       breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"="#56B4E9", "om41"="#000000", "com41"="#56B4E9",
                                "er11"="#009E73", "om11"="#000000", "com11"="#009E73",
                                "er14"="#CC79A7", "om14"="#000000", "com14"="#CC79A7")
                       )
dev.off()

## Test
require("reshape2")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
effortpropmean_stage1_b41 <- rep(0, 25)
# empty matrix with 25 columns
effortpropmean_stage1_b41_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage1_b41 <- 1
effortreaching_stage1_b41 <- NULL
effortreaching_stage1_b41_upper <- NULL
effortreaching_stage1_b41_lower <- NULL
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    # count only when
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1_b41 <- effortpropmean_stage1_b41 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage1_b41_M <- rbind(effortpropmean_stage1_b41_M, effortprop_stage1_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1_b41 <- effortpropmean_stage1_b41 / validcasecount
# cumulative procedure
for(i in 1:25) {
    #effortreaching_stage1_b41 <- c(effortreaching_stage1_b41, (1-sum(effortpropmean_stage1[1:i])))
    #effortreaching_stage1_b41 <- c(effortreaching_stage1_b41, sum(effortpropmean_stage1[1:i]))
    effortreaching_stage1_b41 <- c(effortreaching_stage1_b41, sum(apply(effortpropmean_stage1_b41_M, 2, mean)[1:i]))
    effortreaching_stage1_b41_upper <- c(effortreaching_stage1_b41_upper, sum((apply(effortpropmean_stage1_b41_M, 2, mean) + apply(effortpropmean_stage1_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage1_b41_lower <- c(effortreaching_stage1_b41_lower, sum((apply(effortpropmean_stage1_b41_M, 2, mean) - apply(effortpropmean_stage1_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
distribution <- 0 ## tableuni in sp_learn.php
effortpropmean_stage1_b11 <- rep(0, 25)
effortpropmean_stage1_b11_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage1_b11 <- 1
effortreaching_stage1_b11 <- NULL
effortreaching_stage1_b11_upper <- NULL
effortreaching_stage1_b11_lower <- NULL
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1_b11 <- effortpropmean_stage1_b11 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage1_b11_M <- rbind(effortpropmean_stage1_b11_M, effortprop_stage1_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1_b11 <- effortpropmean_stage1_b11 / validcasecount
for(i in 1:25) {
    #effortreaching_stage1_b11 <- c(effortreaching_stage1_b11, (1-sum(effortpropmean_stage1[1:i])))
    effortreaching_stage1_b11 <- c(effortreaching_stage1_b11, sum(apply(effortpropmean_stage1_b11_M, 2, mean)[1:i]))
    effortreaching_stage1_b11_upper <- c(effortreaching_stage1_b11_upper, sum((apply(effortpropmean_stage1_b11_M, 2, mean) + apply(effortpropmean_stage1_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage1_b11_lower <- c(effortreaching_stage1_b11_lower, sum((apply(effortpropmean_stage1_b11_M, 2, mean) - apply(effortpropmean_stage1_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
distribution <- 1 ## tablepos in sp_learn.php
effortpropmean_stage1_b14 <- rep(0, 25)
effortpropmean_stage1_b14_M <- matrix(, nrow = 0, ncol = 25)
validcasecount <- 0
#effortreaching_stage1_b14 <- 1
effortreaching_stage1_b14 <- NULL
effortreaching_stage1_b14_upper <- NULL
effortreaching_stage1_b14_lower <- NULL
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1_b14 <- effortpropmean_stage1_b14 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        effortpropmean_stage1_b14_M <- rbind(effortpropmean_stage1_b14_M, effortprop_stage1_arr[(distribution+1),i,(payoff+1),])
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1_b14 <- effortpropmean_stage1_b14 / validcasecount
for(i in 1:25) {
    #effortreaching_stage1_b14 <- c(effortreaching_stage1_b14, (1-sum(effortpropmean_stage1[1:i])))
    effortreaching_stage1_b14 <- c(effortreaching_stage1_b14, sum(apply(effortpropmean_stage1_b14_M, 2, mean)[1:i]))
    effortreaching_stage1_b14_upper <- c(effortreaching_stage1_b14_upper, sum((apply(effortpropmean_stage1_b14_M, 2, mean) + apply(effortpropmean_stage1_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
    effortreaching_stage1_b14_lower <- c(effortreaching_stage1_b14_lower, sum((apply(effortpropmean_stage1_b14_M, 2, mean) - apply(effortpropmean_stage1_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))[1:i]))
}
effortpropmean_stage1_er41 <- data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100)
effortpropmean_stage1_om41 <- data.frame(x=1:length(cspSb41[1:25]*100), y=cspSb41[1:25]*100)
effortpropmean_stage1_com41 <- data.frame(x=1:length(cspb41[1:25]*100), y=cspb41[1:25]*100)
effortpropmean_stage1_er11 <- data.frame(x=1:length(effortreaching_stage1_b11), y=effortreaching_stage1_b11*100)
effortpropmean_stage1_om11 <- data.frame(x=1:length(cspSb11[1:25]*100), y=cspSb11[1:25]*100)
effortpropmean_stage1_com11 <- data.frame(x=1:length(cspb11[1:25]*100), y=cspb11[1:25]*100)
effortpropmean_stage1_er14 <- data.frame(x=1:length(effortreaching_stage1_b14), y=effortreaching_stage1_b14*100)
effortpropmean_stage1_om14 <- data.frame(x=1:length(cspSb14[1:25]*100), y=cspSb14[1:25]*100)
effortpropmean_stage1_com14 <- data.frame(x=1:length(cspb14[1:25]*100), y=cspb14[1:25]*100)
effortpropmean_stage1_zz <- melt(list(er41=effortpropmean_stage1_er41,om41=effortpropmean_stage1_om41,com41=effortpropmean_stage1_com41,er11=effortpropmean_stage1_er11,om11=effortpropmean_stage1_om11,com11=effortpropmean_stage1_com11,er14=effortpropmean_stage1_er14,om14=effortpropmean_stage1_om14,com14=effortpropmean_stage1_com14), id.vars="x")
effortpropmean_stage1_zz$L1 <- factor(effortpropmean_stage1_zz$L1, levels = c("er41", "om41", "com41", "er11", "om11", "com11", "er14", "om14", "com14"), ordered=TRUE)

#pdf(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage1.pdf",sep=""))
png(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage1.png",sep=""),
     width=7, height=7, units='in', res=600)
ggplot(data = effortpropmean_stage1_zz, aes(x=x,y=value, color=L1, shape=L1, linetype=L1)) +
    theme_bw(12) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    theme(axis.line = element_line(color = 'black')) +
    ## draws x and y axis line separately for a bug
    #theme(axis.line.x = element_line(color="black", size = .5),
    #  axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    #geom_line(data = data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100), aes(color="skew41"), lwd=1, color="#56B4E9") +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er41")), aes(color = L1, linetype = L1), lwd=0.6) +
    #geom_ribbon(data = data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100), aes(ymin=level-1, ymax=level+1, x=x, fill = "grey30"), alpha = 0.3) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er41")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#56B4E9", alpha.f=0.5)) +
    #geom_line(data = data.frame(x=1:length(c(1,(1-cspb41[1:24]))*100), y=c(1,(1-cspb41[1:24]))*100), aes(x=x,y=y,color="skew41"), linetype=3, color="#56B4E9") +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om41")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om41")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er11")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er11")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#009E73", alpha.f=0.5)) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om11")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om11")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er14")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er14")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#CC79A7", alpha.f=0.5)) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om14")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om14")), aes(color = L1, shape = L1), size=3) +
    xlab("Position in test rounds") +
    #ylab("% rounds an item was selected by a particular position") +
    #ylab("% rounds an item was already selected by a position") +
    ylab("Cumulative selection probability") +
    #xlim(1,25) +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    #theme(legend.justification = c(0.45, 0.5), legend.position = c(0.6, 0.18), legend.text=element_text(size=12)) +
    theme(legend.position="none") +
    scale_linetype_manual(name = "",
                          breaks=c("er41","om41","er11","om11","er14","om14"),
                          labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                          values=c("er41"=1, "om41"=1,
                                   "er11"=1, "om11"=1,
                                   "er14"=1, "om14"=1)
                          ) +
    scale_shape_manual(name = "",
                       breaks=c("er41","om41","er11","om11","er14","om14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"=23, "om41"=18,
                                "er11"=21, "om11"=19,
                                "er14"=22, "om14"=15)
                       ) +
    scale_color_manual(name = "",
                       breaks=c("er41","om41","er11","om11","er14","om14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"="#56B4E9", "om41"="#000000",
                                "er11"="#009E73", "om11"="#000000",
                                "er14"="#CC79A7", "om14"="#000000")
                       )
dev.off()

## Added optimal lines of expectation goals for comparison
png(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-","payoff",payoff,"stage1com.png",sep=""),
     width=7, height=7, units='in', res=600)
ggplot(data = effortpropmean_stage1_zz, aes(x=x,y=value, color=L1, shape=L1, linetype=L1)) +
    theme_bw(12) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line separately for a bug
    theme(axis.line = element_line(color = 'black')) +
    #theme(axis.line.x = element_line(color="black", size = .5),
    #  axis.line.y = element_line(color="black", size = .5)) +
    theme(plot.title = element_text(size = rel(2), angle=0, face='bold')) +
    theme(axis.text.x = element_text(size = rel(1.7), angle=0)) +
    theme(axis.text.y = element_text(size = rel(1.7), angle=0)) +
    theme(axis.title.y = element_text(size = rel(1.8), angle=90)) +
    theme(axis.title.x = element_text(size = rel(1.8), angle=0)) +
    theme(legend.text = element_text(size = rel(1.4), angle=0)) +
    #geom_line(data = data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100), aes(color="skew41"), lwd=1, color="#56B4E9") +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er41")), aes(color = L1, linetype = L1), lwd=0.6) +
    #geom_ribbon(data = data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100), aes(ymin=level-1, ymax=level+1, x=x, fill = "grey30"), alpha = 0.3) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er41")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#56B4E9", alpha.f=0.5)) +
    #geom_line(data = data.frame(x=1:length(c(1,(1-cspb41[1:24]))*100), y=c(1,(1-cspb41[1:24]))*100), aes(x=x,y=y,color="skew41"), linetype=3, color="#56B4E9") +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om41")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("com41")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om41")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er11")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er11")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#009E73", alpha.f=0.5)) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om11")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("com11")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om11")), aes(color = L1, shape = L1), size=3) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("er14")), aes(color = L1, linetype = L1), lwd=0.6) +
    geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("er14")), aes(color = L1, shape = L1), size=3, fill=adjustcolor("#CC79A7", alpha.f=0.5)) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("om14")), aes(color = L1, linetype = L1), lwd=0.75) +
    geom_line(data = subset(effortpropmean_stage1_zz, L1 %in% c("com14")), aes(color = L1, linetype = L1), lwd=0.75) +
    #geom_point(data = subset(effortpropmean_stage1_zz, L1 %in% c("om14")), aes(color = L1, shape = L1), size=3) +
    xlab("Position in test rounds") +
    #ylab("% rounds an item was selected by a particular position") +
    #ylab("% rounds an item was already selected by a position") +
    ylab("Cumulative selection probability") +
    #xlim(1,25) +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    #theme(legend.justification = c(0.45, 0.5), legend.position = c(0.6, 0.18), legend.text=element_text(size=12)) +
    theme(legend.position="none") +
    scale_linetype_manual(name = "",
                          breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                          labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                          values=c("er41"=1, "om41"=1, "com41"=3,
                                   "er11"=1, "om11"=1, "com11"=4,
                                   "er14"=1, "om14"=1, "com14"=2)
                          ) +
    scale_shape_manual(name = "",
                       breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"=23, "om41"=-1, "com41"=-1,
                                "er11"=21, "om11"=-1, "com11"=-1,
                                "er14"=22, "om14"=-1, "com14"=-1)
                       ) +
    scale_color_manual(name = "",
                       breaks=c("er41","om41","com41","er11","om11","com11","er14","om14","com14"),
                       labels = c(expression(paste("effort of subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=3.7,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=1")),expression(paste("effort of subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical effort with optimal strategy at ",alpha,"=1,",beta,"=3.7"))),
                       values=c("er41"="#56B4E9", "om41"="#000000", "com41"="#56B4E9",
                                "er11"="#009E73", "om11"="#000000", "com11"="#009E73",
                                "er14"="#CC79A7", "om14"="#000000", "com14"="#CC79A7")
                       )
dev.off()


pdf(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-legend.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
#legendpars <- c(expression(paste("effort of subjects in positive skew, training")),expression(paste("effort of subjects in positive skew, test")),expression(paste("theoretical effort with optimal strategy, positive skew")),expression(paste("effort of subjects in uniform, training")),expression(paste("effort of subjects in uniform, test")),expression(paste("theoretical effort with optimal strategy, uniform")),expression(paste("effort of subjects in negative skew, training")),expression(paste("effort of subjects in negative skew, test")),expression(paste("theoretical effort with optimal strategy, negative skew")))
legendpars <- c(expression(paste("Optimal strategy outcome\nfor all 3 distributions")),expression(paste("Positive skew, training")),expression(paste("Positive skew, test")),expression(paste("Uniform, training")),expression(paste("Uniform, test")),expression(paste("Negative skew, training")),expression(paste("Negative skew, test")))
legend(1,1,xjust=0.5,yjust=0.5, legendpars, lty=c(1,1,1,1,1,1,1), lwd=3, pt.cex=3, pch=c(26,0,15,1,19,5,18),col=c("#000000","#CC79A7","#CC79A7","#009E73","#009E73","#56B4E9","#56B4E9"),cex=1.4, bty="n")
dev.off()

pdf(file=paste("./CumulativeDensityAverageRoundItemAlreadySelected-legendcom.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
#legendpars <- c(expression(paste("effort of subjects in positive skew, training")),expression(paste("effort of subjects in positive skew, test")),expression(paste("theoretical effort with optimal strategy, positive skew")),expression(paste("effort of subjects in uniform, training")),expression(paste("effort of subjects in uniform, test")),expression(paste("theoretical effort with optimal strategy, uniform")),expression(paste("effort of subjects in negative skew, training")),expression(paste("effort of subjects in negative skew, test")),expression(paste("theoretical effort with optimal strategy, negative skew")))
legendpars <- c(expression(paste("Optimal strategy for all 3 distributions")),expression(paste("Positive skew, training")),expression(paste("Positive skew, test")),expression(paste("Positive skew, value-based strategy")),expression(paste("Uniform, training")),expression(paste("Uniform, test")),expression(paste("Uniform, value-based strategy")),expression(paste("Negative skew, training")),expression(paste("Negative skew, test")),expression(paste("Negative skew, value-based strategy")))
legend(1,1,xjust=0.5,yjust=0.5, legendpars, lty=c(1,1,1,2,1,1,4,1,1,3), lwd=3, pt.cex=3, pch=c(26,0,15,26,1,19,26,5,18,26),col=c("#000000","#CC79A7","#CC79A7","#CC79A7","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9"),cex=1.4, bty="n")
dev.off()


### Bootstrap resampling
### e.g. to see variations by sampling 50 from 51 and repeat it many times (1 M)
### and then check how varied those means obtained using 50-samples from the real mean from 51
dim(effortpropmean_stage0_b14_M)
dim(effortpropmean_stage0_b11_M)
dim(effortpropmean_stage0_b41_M)
dim(effortpropmean_stage1_b14_M)
dim(effortpropmean_stage1_b41_M)
dim(effortpropmean_stage1_b11_M)

pdf2cdf <- function(s) {
    cdf <- NULL
    for(i in 1:length(s))
        cdf <- c(cdf, sum(s[1:i]))
    return(cdf)
}

bootstrapResample <- function(mat, x=0, repn=1000) {
    randseed <- as.numeric(Sys.Date())
    #randseed <- as.numeric(Sys.time())
    print(randseed)
    set.seed(randseed)
    N <- ncol(mat)
    BSRpdf <- matrix(, nrow = repn, ncol = N)
    BSRcdf <- matrix(, nrow = repn, ncol = N)
    for(j in 1:repn) {
        ## from each position (1 to 25), sample with replacement a sample of [sample size - x] and repeat repn times
        BSRpdf[j,] <- apply(mat, 2, function(m) {mean(sample(m, (length(m) - x), replace=TRUE))})
        j <- j + 1;
    }
    ## convert the resampled PDFs to CDFs
    BSRcdf <- apply(BSRpdf, 1, pdf2cdf)
    ## find the standard errors at each position (1 to 25) among these resampled CDFs
    BSRsem <- apply(t(BSRcdf), 2, function(m) {sd(m)/sqrt(length(m))})
    return(BSRsem)
}

## usage:
##    mat = matrix of stopping proportions of 25 positions * number of subjects
##     i.e. for example, 25 columns * 51 rows (each representing a subject)
##    0 = minus 0 sample(s) (with replacement) from the sample
##    1000 = repetition
## > bootstrapResample(mat,0,1000)
## output = 25 sem (standard error of the mean) among the 100 resampled data points at each position


### (PDF) Percentage of rounds selected at a certain position
# for demostration to Jonathan and Lael about PDF & CDF & variability
## Training
epm41 <- data.frame(x=1:length(apply(effortpropmean_stage0_b41_M, 2, mean)), y=apply(effortpropmean_stage0_b41_M, 2, mean))
# function for the standard error of the mean: sem <- sd(x)/sqrt(length(x))
esem41 <- data.frame(x=1:length(apply(effortpropmean_stage0_b41_M, 2, mean)), y=apply(effortpropmean_stage0_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))
epm11 <- data.frame(x=1:length(apply(effortpropmean_stage0_b11_M, 2, mean)), y=apply(effortpropmean_stage0_b11_M, 2, mean))
esem11 <- data.frame(x=1:length(apply(effortpropmean_stage0_b11_M, 2, mean)), y=apply(effortpropmean_stage0_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))
epm14 <- data.frame(x=1:length(apply(effortpropmean_stage0_b14_M, 2, mean)), y=apply(effortpropmean_stage0_b14_M, 2, mean))
esem14 <- data.frame(x=1:length(apply(effortpropmean_stage0_b14_M, 2, mean)), y=apply(effortpropmean_stage0_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))
zpm <- melt(list(epm41=epm41,epm11=epm11,epm14=epm14), id.vars="x")
zpm$L1 <- factor(zpm$L1, levels = c("epm41","epm11","epm14"), ordered=TRUE)
zpm$sem <- c(esem41$y,esem11$y,esem14$y)
# For convert to percentages later
zpm$value <- zpm$value*100
zpm$sem <- zpm$sem*100

pdf(file=paste("./ProbabilityDensityProportionRoundEffort-","payoff",payoff,"stage0.pdf",sep=""))
ggplot(data = zpm, aes(x=x,y=value, color=L1, shape=L1, fill=L1)) +
    theme_bw(18) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    #geom_line(data = data.frame(x=1:length(effortreaching_stage0_b41), y=effortreaching_stage0_b41*100), aes(color="skew41"), lwd=1, color="#56B4E9") +
    geom_line(data = subset(zpm, L1 %in% c("epm41")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm41")), aes(color = L1, shape = L1), size=4) +
    # calculate 95% confidence interval with SEM  = c(mean(x)-2*sem,mean(x)+2*sem)
    geom_ribbon(data = subset(zpm, L1 %in% c("epm41")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zpm, L1 %in% c("epm11")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm11")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zpm, L1 %in% c("epm11")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zpm, L1 %in% c("epm14")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm14")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zpm, L1 %in% c("epm14")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    xlab("Position") +
    ylab("% rounds an item was selected at a particular position") +
    #xlim(1,25) +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    theme(legend.justification = c(0.45, 0.5), legend.position = c(0.66, 0.9), legend.title=element_text(size=15), legend.text=element_text(size=12)) +
    scale_shape_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"=18,
                                "epm11"=19,
                                "epm14"=15)
                       ) +
    scale_color_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"="#56B4E9",
                                "epm11"="#009E73",
                                "epm14"="#CC79A7")
                       ) +
    scale_fill_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"="#56B4E9",
                                "epm11"="#009E73",
                                "epm14"="#CC79A7")
                       )
dev.off()

## Test
epm41 <- data.frame(x=1:length(apply(effortpropmean_stage1_b41_M, 2, mean)), y=apply(effortpropmean_stage1_b41_M, 2, mean))
# function for the standard error of the mean: sem <- sd(x)/sqrt(length(x))
esem41 <- data.frame(x=1:length(apply(effortpropmean_stage1_b41_M, 2, mean)), y=apply(effortpropmean_stage1_b41_M, 2, function(x) {sd(x)/sqrt(length(x))}))
epm11 <- data.frame(x=1:length(apply(effortpropmean_stage1_b11_M, 2, mean)), y=apply(effortpropmean_stage1_b11_M, 2, mean))
esem11 <- data.frame(x=1:length(apply(effortpropmean_stage1_b11_M, 2, mean)), y=apply(effortpropmean_stage1_b11_M, 2, function(x) {sd(x)/sqrt(length(x))}))
epm14 <- data.frame(x=1:length(apply(effortpropmean_stage1_b14_M, 2, mean)), y=apply(effortpropmean_stage1_b14_M, 2, mean))
esem14 <- data.frame(x=1:length(apply(effortpropmean_stage1_b14_M, 2, mean)), y=apply(effortpropmean_stage1_b14_M, 2, function(x) {sd(x)/sqrt(length(x))}))
zpm <- melt(list(epm41=epm41,epm11=epm11,epm14=epm14), id.vars="x")
zpm$L1 <- factor(zpm$L1, levels = c("epm41","epm11","epm14"), ordered=TRUE)
zpm$sem <- c(esem41$y,esem11$y,esem14$y)
# For convert to percentages later
zpm$value <- zpm$value*100
zpm$sem <- zpm$sem*100

pdf(file=paste("./ProbabilityDensityProportionRoundEffort-","payoff",payoff,"stage1.pdf",sep=""))
ggplot(data = zpm, aes(x=x,y=value, color=L1, shape=L1, fill=L1)) +
    theme_bw(18) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    #geom_line(data = data.frame(x=1:length(effortreaching_stage1_b41), y=effortreaching_stage1_b41*100), aes(color="skew41"), lwd=1, color="#56B4E9") +
    geom_line(data = subset(zpm, L1 %in% c("epm41")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm41")), aes(color = L1, shape = L1), size=4) +
    # calculate 95% confidence interval with SEM  = c(mean(x)-2*sem,mean(x)+2*sem)
    geom_ribbon(data = subset(zpm, L1 %in% c("epm41")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zpm, L1 %in% c("epm11")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm11")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zpm, L1 %in% c("epm11")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zpm, L1 %in% c("epm14")), aes(color = L1, shape = L1), lwd=1) +
    geom_point(data = subset(zpm, L1 %in% c("epm14")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zpm, L1 %in% c("epm14")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    xlab("Position") +
    ylab("% rounds an item was selected at a particular position") +
    #xlim(1,25) +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    theme(legend.justification = c(0.45, 0.5), legend.position = c(0.66, 0.9), legend.title=element_text(size=15), legend.text=element_text(size=12)) +
    scale_shape_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"=18,
                                "epm11"=19,
                                "epm14"=15)
                       ) +
    scale_color_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"="#56B4E9",
                                "epm11"="#009E73",
                                "epm14"="#CC79A7")
                       ) +
    scale_fill_manual(name = "Shading in 95% CI",
                       breaks=c("epm41","epm11","epm14"),
                       labels = c(expression(paste("Rounds at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Rounds at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)"))),
                       values=c("epm41"="#56B4E9",
                                "epm11"="#009E73",
                                "epm14"="#CC79A7")
                       )
dev.off()


### (CDF) Cumulative percentage of rounds selected at a certain position
# for demostration to Jonathan and Lael about PDF & CDF & variability
## Training
ecm41 <- data.frame(x=1:length(apply(effortpropmean_stage0_b41_M, 2, mean)), y=pdf2cdf(apply(effortpropmean_stage0_b41_M, 2, mean)))
# function for the standard error of the mean: sem <- sd(x)/sqrt(length(x))
ecsem41 <- data.frame(x=1:length(apply(effortpropmean_stage0_b41_M, 2, mean)), y=bootstrapResample(effortpropmean_stage0_b41_M, 0, 1000))
ecm11 <- data.frame(x=1:length(apply(effortpropmean_stage0_b11_M, 2, mean)), y=pdf2cdf(apply(effortpropmean_stage0_b11_M, 2, mean)))
ecsem11 <- data.frame(x=1:length(apply(effortpropmean_stage0_b11_M, 2, mean)), y=bootstrapResample(effortpropmean_stage0_b11_M, 0, 1000))
ecm14 <- data.frame(x=1:length(apply(effortpropmean_stage0_b14_M, 2, mean)), y=pdf2cdf(apply(effortpropmean_stage0_b14_M, 2, mean)))
ecsem14 <- data.frame(x=1:length(apply(effortpropmean_stage0_b14_M, 2, mean)), y=bootstrapResample(effortpropmean_stage0_b14_M, 0, 1000))
zcm <- melt(list(ecm41=ecm41,ecm11=ecm11,ecm14=ecm14), id.vars="x")
zcm$L1 <- factor(zcm$L1, levels = c("ecm41","ecm11","ecm14"), ordered=TRUE)
zcm$sem <- c(ecsem41$y,ecsem11$y,ecsem14$y)
# For convert to percentages later
zcm$value <- zcm$value*100
zcm$sem <- zcm$sem*100

pdf(file=paste("./CumulativeDensityProportionRoundEffort-","payoff",payoff,"stage0.pdf",sep=""))
ggplot(data = zcm, aes(x=x,y=value, color=L1, shape=L1, fill=L1)) +
    theme_bw(18) +
    #eliminates background, gridlines, and chart border
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black')) +
    #geom_line(data = data.frame(x=1:length(effortreaching_stage0_b41), y=effortreaching_stage0_b41*100), aes(color="skew41"), lwd=1, color="#56B4E9") +
    geom_line(data = subset(zcm, L1 %in% c("ecm41")), aes(color = L1, shape = L1), lwd=0.5) +
    geom_point(data = subset(zcm, L1 %in% c("ecm41")), aes(color = L1, shape = L1), size=4) +
    # calculate 95% confidence interval with SEM  = c(mean(x)-2*sem,mean(x)+2*sem)
    geom_ribbon(data = subset(zcm, L1 %in% c("ecm41")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zcm, L1 %in% c("ecm11")), aes(color = L1, shape = L1), lwd=0.5) +
    geom_point(data = subset(zcm, L1 %in% c("ecm11")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zcm, L1 %in% c("ecm11")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    geom_line(data = subset(zcm, L1 %in% c("ecm14")), aes(color = L1, shape = L1), lwd=0.5) +
    geom_point(data = subset(zcm, L1 %in% c("ecm14")), aes(color = L1, shape = L1), size=3) +
    geom_ribbon(data = subset(zcm, L1 %in% c("ecm14")), aes(ymin=value-(2*sem), ymax=value+(2*sem), x=x, fill=L1), alpha = 0.3) +
    xlab("Position") +
    ylab("Cumulative % selection at / before position") +
    #xlim(1,25) +
    scale_x_continuous(breaks = c(5,10,15,20,25)) +
    theme(legend.justification = c(0.45, 0.5), legend.position = c(0.66, 0.15), legend.title=element_text(size=15), legend.text=element_text(size=12)) +
    scale_shape_manual(name = "Shading in 95% CI",
                       breaks=c("ecm41","ecm11","ecm14"),
                       labels = c(expression(paste("Rounds at negative skew")),expression(paste("Rounds at uniform")),expression(paste("Rounds at positive skew"))),
                       values=c("ecm41"=18,
                                "ecm11"=19,
                                "ecm14"=15)
                       ) +
    scale_color_manual(name = "Shading in 95% CI",
                       breaks=c("ecm41","ecm11","ecm14"),
                       labels = c(expression(paste("Rounds at negative skew")),expression(paste("Rounds at uniform")),expression(paste("Rounds at positive skew"))),
                       values=c("ecm41"="#56B4E9",
                                "ecm11"="#009E73",
                                "ecm14"="#CC79A7")
                       ) +
    scale_fill_manual(name = "Shading in 95% CI",
                       breaks=c("ecm41","ecm11","ecm14"),
                       labels = c(expression(paste("Rounds at negative skew")),expression(paste("Rounds at uniform")),expression(paste("Rounds at positive skew"))),
                       values=c("ecm41"="#56B4E9",
                                "ecm11"="#009E73",
                                "ecm14"="#CC79A7")
                       )
dev.off()



### Percentage of rounds reaching certain position on average VS Cumulative stopping probability
## Training
pdf(file=paste("./CumulativeDensityAverageRoundEffortReached-","payoff",payoff,"stage0.pdf",sep=""))
par(mar=c(5.1,5.6,4.1,2.1))
plot(5,5, type='n',cex.lab=2,cex.axis=1.5,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     ylim=c(0, 100),
     # axes=FALSE,
     bty="l",
     # ann=FALSE,
     #main="Percentage of participants on average\nreaching a position before choosing a card",
     xlab="Position",
     ylab="% rounds reaching the position")
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
effortpropmean_stage0 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage0 <- 1
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0 <- effortpropmean_stage0 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0 <- effortpropmean_stage0 / validcasecount
for(i in 1:24)
    effortreaching_stage0 <- c(effortreaching_stage0, (1-sum(effortpropmean_stage0[1:i])))
lines(effortreaching_stage0*100, lty=1, lwd=2, col="#56B4E9")
points(effortreaching_stage0*100, pch=18, lwd=2, col="#56B4E9")
points(effortreaching_stage0*100, pch=23, lwd=1, col="#56B4E9")
lines(c(1,(1-cspb41[1:24]))*100, lty=3, lwd=1.5, col="#56B4E9")
points(c(1,(1-cspb41[1:24]))*100, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
effortpropmean_stage0 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage0 <- 1
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0 <- effortpropmean_stage0 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0 <- effortpropmean_stage0 / validcasecount
for(i in 1:24)
    effortreaching_stage0 <- c(effortreaching_stage0, (1-sum(effortpropmean_stage0[1:i])))
lines(effortreaching_stage0*100, lty=1, lwd=2, col="#009E73")
points(effortreaching_stage0*100, pch=19, col="#009E73")
lines(c(1,(1-cspb11[1:24]))*100, lty=3, lwd=1.5, col="#009E73")
points(c(1,(1-cspb11[1:24]))*100, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
effortpropmean_stage0 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage0 <- 1
for(i in 1:nrow(effort_stage0_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage0_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage0_arr[(distribution+1),i,(payoff+1),] <- effort_stage0_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage0_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage0 <- effortpropmean_stage0 + effortprop_stage0_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage0 <- effortpropmean_stage0 / validcasecount
for(i in 1:24)
    effortreaching_stage0 <- c(effortreaching_stage0, (1-sum(effortpropmean_stage0[1:i])))
lines(effortreaching_stage0*100, lty=1, lwd=2, col="#CC79A7")
points(effortreaching_stage0*100, pch=15, col="#CC79A7")
lines(c(1,(1-cspb14[1:24]))*100, lty=3, lwd=1.5, col="#CC79A7")
points(c(1,(1-cspb14[1:24]))*100, pch=0, col="#CC79A7")
legendpars <- c(expression(paste("subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical density with optimal strategy")))
legend(x="topright", legendpars, lty=c(1,3,1,3,1,3), lwd=1.5, pch=c(18,5,19,1,15,0),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"))
dev.off()

## Test
pdf(file=paste("./CumulativeDensityAverageRoundEffortReached-","payoff",payoff,"stage1.pdf",sep=""))
par(mar=c(5.1,5.6,4.1,2.1))
plot(5,5, type='n',cex.lab=2,cex.axis=1.5,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     ylim=c(0, 100),
     bty="l",
     # axes=FALSE,
     # ann=FALSE,
     #main="Percentage of participants on average\nreaching a position before choosing a card",
     xlab="Position",
     ylab="% rounds reaching the position")
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
effortpropmean_stage1 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage1 <- 1
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1 <- effortpropmean_stage1 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1 <- effortpropmean_stage1 / validcasecount
for(i in 1:24)
    effortreaching_stage1 <- c(effortreaching_stage1, (1-sum(effortpropmean_stage1[1:i])))
lines(effortreaching_stage1*100, lty=1, lwd=2, col="#56B4E9")
points(effortreaching_stage1*100, pch=18, lwd=2, col="#56B4E9")
points(effortreaching_stage1*100, pch=23, lwd=1, col="#56B4E9")
lines(c(1,(1-cspb41[1:24]))*100, lty=3, lwd=1.5, col="#56B4E9")
points(c(1,(1-cspb41[1:24]))*100, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
effortpropmean_stage1 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage1 <- 1
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1 <- effortpropmean_stage1 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1 <- effortpropmean_stage1 / validcasecount
for(i in 1:24)
    effortreaching_stage1 <- c(effortreaching_stage1, (1-sum(effortpropmean_stage1[1:i])))
lines(effortreaching_stage1*100, lty=1, lwd=2, col="#009E73")
points(effortreaching_stage1*100, pch=19, col="#009E73")
lines(c(1,(1-cspb11[1:24]))*100, lty=3, lwd=1.5, col="#009E73")
points(c(1,(1-cspb11[1:24]))*100, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
effortpropmean_stage1 <- rep(0, 25)
validcasecount <- 0
effortreaching_stage1 <- 1
for(i in 1:nrow(effort_stage1_arr[(distribution+1),,(payoff+1),])) {
    if(sum(effort_stage1_arr[(distribution+1),i,(payoff+1),]) >= 10) {
        effortprop_stage1_arr[(distribution+1),i,(payoff+1),] <- effort_stage1_arr[(distribution+1),i,(payoff+1),]/sum(effort_stage1_arr[(distribution+1),i,(payoff+1),])
        effortpropmean_stage1 <- effortpropmean_stage1 + effortprop_stage1_arr[(distribution+1),i,(payoff+1),]
        validcasecount <- validcasecount + 1
    }
}
effortpropmean_stage1 <- effortpropmean_stage1 / validcasecount
for(i in 1:24)
    effortreaching_stage1 <- c(effortreaching_stage1, (1-sum(effortpropmean_stage1[1:i])))
lines(effortreaching_stage1*100, lty=1, lwd=2, col="#CC79A7")
points(effortreaching_stage1*100, pch=15, col="#CC79A7")
lines(c(1,(1-cspb14[1:24]))*100, lty=3, lwd=1.5, col="#CC79A7")
points(c(1,(1-cspb14[1:24]))*100, pch=0, col="#CC79A7")
legendpars <- c(expression(paste("subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical density with optimal strategy")))
legend(x="topright", legendpars, lty=c(1,3,1,3,1,3), lwd=1.5, pch=c(18,5,19,1,15,0),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"))
dev.off()

payoff <- 0

### Percentage of subjects reaching certain position on average VS Cumulative stopping probability
#pdf(file=paste("./CumulativeDensityDMEffortSpent-","payoff",payoff,"distri",distribution,"stage0.pdf",sep=""))
## Training
pdf(file=paste("./CumulativeDensityDMEffortSpent-","payoff",payoff,"stage0.pdf",sep=""))
par(mar=c(5.1,5.6,4.1,2.1))
plot(5,5, type='n',cex.lab=2,cex.axis=1.5,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     ylim=c(0, 100),
     # axes=FALSE,
     # ann=FALSE,
     #main="Percentage of participants on average\nreaching a position before choosing a card",
     xlab="Position",
     ylab="% subjects reaching the position")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
CumulativeDensityDMEffortSpent0 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent0[i] <- length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage0_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent0*100, lty=1, lwd=2, col="#56B4E9")
points(CumulativeDensityDMEffortSpent0*100, pch=18, lwd=2, col="#56B4E9")
points(CumulativeDensityDMEffortSpent0*100, pch=23, lwd=1, col="#56B4E9")
#lines(c(1,(1-cspb41[1:24]))*100, lty=3, lwd=1.5, col="#56B4E9")
#points(c(1,(1-cspb41[1:24]))*100, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
CumulativeDensityDMEffortSpent0 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent0[i] <- length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage0_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent0*100, lty=1, lwd=2, col="#009E73")
points(CumulativeDensityDMEffortSpent0*100, pch=19, col="#009E73")
#lines(c(1,(1-cspb11[1:24]))*100, lty=3, lwd=1.5, col="#009E73")
#points(c(1,(1-cspb11[1:24]))*100, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
CumulativeDensityDMEffortSpent0 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent0[i] <- length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage0_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage0_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage0_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent0*100, lty=1, lwd=2, col="#CC79A7")
points(CumulativeDensityDMEffortSpent0*100, pch=15, col="#CC79A7")
#lines(c(1,(1-cspb14[1:24]))*100, lty=3, lwd=1.5, col="#CC79A7")
#points(c(1,(1-cspb14[1:24]))*100, pch=0, col="#CC79A7")
legendpars <- c(expression(paste("subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical density with optimal strategy")))
legend(x="topright", legendpars, lty=c(1,3,1,3,1,3), lwd=1.5, pch=c(18,5,19,1,15,0),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"))
dev.off()

payoff <- 0

## Test
pdf(file=paste("./CumulativeDensityDMEffortSpent-","payoff",payoff,"stage1.pdf",sep=""))
par(mar=c(5.1,5.6,4.1,2.1))
plot(5,5, type='n',cex.lab=2,cex.axis=1.5,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     ylim=c(0, 100),
     # axes=FALSE,
     # ann=FALSE,
     #main="Percentage of participants on average\nreaching a position before choosing a card",
     xlab="Position",
     ylab="% of subjects reaching the position")
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
CumulativeDensityDMEffortSpent1 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent1[i] <- length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage1_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent1*100, lty=1, lwd=2, col="#56B4E9")
points(CumulativeDensityDMEffortSpent1*100, pch=18, lwd=2, col="#56B4E9")
points(CumulativeDensityDMEffortSpent1*100, pch=23, lwd=1, col="#56B4E9")
#lines(c(1,(1-cspb41[1:24]))*100, lty=3, lwd=1.5, col="#56B4E9")
#points(c(1,(1-cspb41[1:24]))*100, pch=5, col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
CumulativeDensityDMEffortSpent1 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent1[i] <- length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage1_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent1*100, lty=1, lwd=2, col="#009E73")
points(CumulativeDensityDMEffortSpent1*100, pch=19, col="#009E73")
#lines(c(1,(1-cspb11[1:24]))*100, lty=3, lwd=1.5, col="#009E73")
#points(c(1,(1-cspb11[1:24]))*100, pch=1, col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
CumulativeDensityDMEffortSpent1 <- rep(0, (N-1));
for(i in 1:N) {
    CumulativeDensityDMEffortSpent1[i] <- length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)]) & mean_effort_stage1_arr[(distribution+1),,(payoff+1)] >= (i-1)])/length(mean_effort_stage1_arr[(distribution+1),,(payoff+1)][is.finite(mean_effort_stage1_arr[(distribution+1),,(payoff+1)])])
}
lines(CumulativeDensityDMEffortSpent1*100, lty=1, lwd=2, col="#CC79A7")
points(CumulativeDensityDMEffortSpent1*100, pch=15, col="#CC79A7")
#lines(c(1,(1-cspb14[1:24]))*100, lty=3, lwd=1.5, col="#CC79A7")
#points(c(1,(1-cspb14[1:24]))*100, pch=0, col="#CC79A7")
legendpars <- c(expression(paste("subjects at ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("theoretical density with optimal strategy")),expression(paste("subjects at ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("theoretical density with optimal strategy")))
legend(x="topright", legendpars, lty=c(1,3,1,3,1,3), lwd=1.5, pch=c(18,5,19,1,15,0),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"))
dev.off()

# Run from the lines above fullknowthresholdsn25.pdf (in sp_StimulationPlots.R) and then here
fkcardthresholds41 <- (mean(as.matrix(read.csv(file="tableneg.csv",header = TRUE))) + (fkthresholdslist[[1]]*sd(as.matrix(read.csv(file="tableneg.csv",header = TRUE)))))
fkcardthresholds11 <- (mean(as.matrix(read.csv(file="tableuni.csv",header = TRUE))) + (fkthresholdslist[[2]]*sd(as.matrix(read.csv(file="tableuni.csv",header = TRUE)))))
fkcardthresholds14 <- (mean(as.matrix(read.csv(file="tablepos.csv",header = TRUE))) + (fkthresholdslist[[3]]*sd(as.matrix(read.csv(file="tablepos.csv",header = TRUE)))))

# Run from the commented lines above fullknowcondexpectationn25.pdf (in sp_StimulationPlots.R) and then here
fkcardcondexpects41 <- (mean(as.matrix(read.csv(file="tableneg.csv",header = TRUE))) + (fkcondexplist[[1]]*sd(as.matrix(read.csv(file="tableneg.csv",header = TRUE)))))
fkcardcondexpects11 <- (mean(as.matrix(read.csv(file="tableuni.csv",header = TRUE))) + (fkcondexplist[[2]]*sd(as.matrix(read.csv(file="tableuni.csv",header = TRUE)))))
fkcardcondexpects14 <- (mean(as.matrix(read.csv(file="tablepos.csv",header = TRUE))) + (fkcondexplist[[3]]*sd(as.matrix(read.csv(file="tablepos.csv",header = TRUE)))))

pdf(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage0Z.pdf",sep=""))
plot(5,5, type='n',
     xlim=c(0, 25),
     ylim=c(-2, 1),
     #ylim=c(4000,8000),
     # axes=FALSE,
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position in training rounds",
     #ylab="Quality relative to mean"
     ylab="Z-score of Quality relative to mean"
     )
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage0_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage0_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage0_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage0_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage0_arr","mean_effort_stage1_arr","mean_dtime_stage0_arr","mean_dtime_stage1_arr","mean_chosen_stage0_arr","mean_chosen_stage1_arr","sd_chosen_stage0_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc0mean <- rep(NA, 25)
#for(i in 0:24)
#    cc0mean[i+1] <- mean((subset(cc0clean, position==i))$chosen)
#plot(cc0mean,type='l')
#lines(smooth.spline(cc0mean,spar=.6),col='red',lty=4)
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    #print(i);print((cc0clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr))
    cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- (cc0clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$sd_chosen_stage0_arr + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    #cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    cc0adjust[cc0clean[i,]$position,]$counter <- 1 + cc0adjust[cc0clean[i,]$position,]$counter
}
lines(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#56B4E9")
points(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=5,col="#56B4E9")
lines(smooth.spline(cc0adjust$chosenadjustedsum/cc0adjust$counter,spar=.6),lwd=2,lty=3,col="#56B4E9")
payoff <- 1
distribution <- 0 ## tableuni in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage0_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage0_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage0_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage0_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage0_arr","mean_effort_stage1_arr","mean_dtime_stage0_arr","mean_dtime_stage1_arr","mean_chosen_stage0_arr","mean_chosen_stage1_arr","sd_chosen_stage0_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc0mean <- rep(NA, 25)
#for(i in 0:24)
#    cc0mean[i+1] <- mean((subset(cc0clean, position==i))$chosen)
#plot(cc0mean,type='l')
#lines(smooth.spline(cc0mean,spar=.6),col='red',lty=4)
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    #print(i);print((cc0clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr))
    cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- (cc0clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$sd_chosen_stage0_arr + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    #cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    cc0adjust[cc0clean[i,]$position,]$counter <- 1 + cc0adjust[cc0clean[i,]$position,]$counter
}
lines(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#009E73")
points(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=1,col="#009E73")
lines(smooth.spline(cc0adjust$chosenadjustedsum/cc0adjust$counter,spar=.6),lwd=2,lty=3,col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage0_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage0_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage0_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage0_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage0_arr","mean_effort_stage1_arr","mean_dtime_stage0_arr","mean_dtime_stage1_arr","mean_chosen_stage0_arr","mean_chosen_stage1_arr","sd_chosen_stage0_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc0mean <- rep(NA, 25)
#for(i in 0:24)
#    cc0mean[i+1] <- mean((subset(cc0clean, position==i))$chosen)
#plot(cc0mean,type='l')
#lines(smooth.spline(cc0mean,spar=.6),col='red',lty=4)
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    #print(i);print((cc0clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr))
    cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- (cc0clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$mean_chosen_stage0_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc0clean[i,]$sessionid))$sd_chosen_stage0_arr + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    #cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[cc0clean[i,]$position,]$chosenadjustedsum
    cc0adjust[cc0clean[i,]$position,]$counter <- 1 + cc0adjust[cc0clean[i,]$position,]$counter
}
lines(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#CC79A7")
points(1:24,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=0,col="#CC79A7")
lines(smooth.spline(cc0adjust$chosenadjustedsum/cc0adjust$counter,spar=.6),lwd=2,lty=3,col="#CC79A7")
dev.off()

payoff <- 0

#pdf(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage0.pdf",sep=""))
png(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage0.png",sep=""),
     width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.5,4.1,1.7))
plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     #ylim=c(-2, 1),
     #ylim=c(4500, 8000),
     ylim=c(4500, 8400),
     #axes=FALSE,
     #bty="n", # this remove the borders but not the axes not to the XY-origin
     bty="l", # this remove the borders but not the axes to the XY origin
     #xaxt="n",
     yaxt="n",
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position in training rounds",
     ylab="Value"
     #ylab="Z-score of Quality relative to mean"
     )
#title(cex.main=2,cex.sub=1.5,cex.lab=1.5,main="Values of chosen cards")
# add back the axis ticks' intervals
#axis(side = 1, at = c(1,5,10,15,20,25))
axis(side = 2, at = c(5000,6000,7000,8000), cex.axis=1.7)
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
lines(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#56B4E9")
points(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=5,col="#56B4E9")
#lines(1:25,rev(indiffcardnegEmpirical),lwd=2,lty=3,col="#56B4E9")
lines(1:25,indiffcardnegEmpirical,lwd=2,lty=3,col="#56B4E9")
#lines(1:25,rev(indiffcardnegCondexp),lwd=2,lty=3,col="blue")
lines(1:25,(fkcondexplist[[1]]*sd(as.matrix(dumtable[[3]])) + mean(as.matrix(dumtable[[3]]))),lwd=2,lty=3,col="blue")
distribution <- 0 ## tableuni in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
lines(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#009E73")
points(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=1,col="#009E73")
#lines(1:25,rev(indiffcarduniEmpirical),lwd=2,lty=4,col="#009E73")
lines(1:25,indiffcarduniEmpirical,lwd=2,lty=4,col="#009E73")
#lines(1:25,rev(indiffcarduniCondexp),lwd=2,lty=4,col="blue")
lines(1:25,(fkcondexplist[[2]]*sd(as.matrix(dumtable[[1]])) + mean(as.matrix(dumtable[[1]]))),lwd=2,lty=4,col="green")
distribution <- 1 ## tablepos in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
lines(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,lty=1,col="#CC79A7")
points(1:25,cc0adjust$chosenadjustedsum/cc0adjust$counter,pch=0,col="#CC79A7")
#lines(1:25,rev(indiffcardposEmpirical),lwd=2,lty=2,col="#CC79A7")
lines(1:25,indiffcardposEmpirical,lwd=2,lty=2,col="#CC79A7")
#lines(1:25,rev(indiffcardposCondexp),lwd=2,lty=2,col="red")
lines(1:25,(fkcondexplist[[3]]*sd(as.matrix(dumtable[[2]])) + mean(as.matrix(dumtable[[2]]))),lwd=2,lty=2,col="red")
dev.off()

png(file=paste("./PercentileAcceptAtPosition-","payoff",payoff,"stage0.png",sep=""),
     width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.5,4.1,1.7))
plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     #ylim=c(-2, 1),
     #ylim=c(4500, 8000),
     ylim=c(0.4, 1),
     #axes=FALSE,
     #bty="n", # this remove the borders but not the axes not to the XY-origin
     bty="l", # this remove the borders but not the axes to the XY origin
     #xaxt="n",
     yaxt="n",
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position in training rounds",
     ylab="Percentile"
     #ylab="Z-score of Quality relative to mean"
     )
#title(cex.main=2,cex.sub=1.5,cex.lab=1.5,main="Values of chosen cards")
# add back the axis ticks' intervals
#axis(side = 1, at = c(1,5,10,15,20,25))
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1), cex.axis=1.7)
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[3]]),cc0adjust$chosenadjustedsum/cc0adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#56B4E9")
points(1:25,chosenPercentile,pch=5,col="#56B4E9")
lines(1:25,indiffcardnegEmpiricalPercent,lwd=2,lty=3,col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[1]]),cc0adjust$chosenadjustedsum/cc0adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#009E73")
points(1:25,chosenPercentile,pch=1,col="#009E73")
lines(1:25,indiffcarduniEmpiricalPercent,lwd=2,lty=4,col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
cc0clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==0, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc0adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc0clean)) {
    cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum <- cc0clean[i,]$chosen + cc0adjust[(cc0clean[i,]$position+1),]$chosenadjustedsum
    cc0adjust[(cc0clean[i,]$position+1),]$counter <- 1 + cc0adjust[(cc0clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[2]]),cc0adjust$chosenadjustedsum/cc0adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#CC79A7")
points(1:25,chosenPercentile,pch=0,col="#CC79A7")
lines(1:25,indiffcardposEmpiricalPercent,lwd=2,lty=2,col="#CC79A7")
dev.off()

pdf(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage1Z.pdf",sep=""))
plot(5,5, type='n',
     xlim=c(0, 25),
     ylim=c(-2, 1),
     #ylim=c(4000, 8000),
     # axes=FALSE,
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position",
     #ylab="Quality relative to mean"
     ylab="Z-score of Quality relative to mean"
     )
payoff <- 1
distribution <- 2 ## tableneg in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage1_arr","mean_effort_stage1_arr","mean_dtime_stage1_arr","mean_dtime_stage1_arr","mean_chosen_stage1_arr","mean_chosen_stage1_arr","sd_chosen_stage1_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc1mean <- rep(NA, 25)
#for(i in 0:24)
#    cc1mean[i+1] <- mean((subset(cc1clean, position==i))$chosen)
#plot(cc1mean,type='l')
#lines(smooth.spline(cc1mean,spar=.6),col='red',lty=4)
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    #print(i);print((cc1clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr))
    cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- (cc1clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$sd_chosen_stage1_arr + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    #cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    cc1adjust[cc1clean[i,]$position,]$counter <- 1 + cc1adjust[cc1clean[i,]$position,]$counter
}
lines(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#56B4E9")
points(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=5,col="#56B4E9")
lines(smooth.spline(cc1adjust$chosenadjustedsum/cc1adjust$counter,spar=.6),lwd=2,lty=3,col="#56B4E9")
payoff <- 1
distribution <- 0 ## tableuni in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage1_arr","mean_effort_stage1_arr","mean_dtime_stage1_arr","mean_dtime_stage1_arr","mean_chosen_stage1_arr","mean_chosen_stage1_arr","sd_chosen_stage1_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc1mean <- rep(NA, 25)
#for(i in 0:24)
#    cc1mean[i+1] <- mean((subset(cc1clean, position==i))$chosen)
#plot(cc1mean,type='l')
#lines(smooth.spline(cc1mean,spar=.6),col='red',lty=4)
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    #print(i);print((cc1clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr))
    cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- (cc1clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$sd_chosen_stage1_arr + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    #cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    cc1adjust[cc1clean[i,]$position,]$counter <- 1 + cc1adjust[cc1clean[i,]$position,]$counter
}
lines(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#009E73")
points(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=1,col="#009E73")
lines(smooth.spline(cc1adjust$chosenadjustedsum/cc1adjust$counter,spar=.6),lwd=2,lty=3,col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
SubjEffortTimeChosen <- data.frame(cbind(Sessionid,mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_effort_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_dtime_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],mean_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)],sd_chosen_stage1_arr[(distribution+1),,(payoff+1)]))
names(SubjEffortTimeChosen) <- c("Sessionid","mean_effort_stage1_arr","mean_effort_stage1_arr","mean_dtime_stage1_arr","mean_dtime_stage1_arr","mean_chosen_stage1_arr","mean_chosen_stage1_arr","sd_chosen_stage1_arr","sd_chosen_stage1_arr")
## to locate duplicate rows:
#which(duplicated(SubjEffortTimeChosen$Sessionid) | duplicated(SubjEffortTimeChosen$Sessionid, fromLast = TRUE))
## mean of chosen items at each position
#cc1mean <- rep(NA, 25)
#for(i in 0:24)
#    cc1mean[i+1] <- mean((subset(cc1clean, position==i))$chosen)
#plot(cc1mean,type='l')
#lines(smooth.spline(cc1mean,spar=.6),col='red',lty=4)
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 24)
counter <- rep(0, 24)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    #print(i);print((cc1clean[i,]$chosen / (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr))
    cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- (cc1clean[i,]$chosen - (subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$mean_chosen_stage1_arr)/(subset(SubjEffortTimeChosen, Sessionid == cc1clean[i,]$sessionid))$sd_chosen_stage1_arr + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    #cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[cc1clean[i,]$position,]$chosenadjustedsum
    cc1adjust[cc1clean[i,]$position,]$counter <- 1 + cc1adjust[cc1clean[i,]$position,]$counter
}
lines(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#CC79A7")
points(1:24,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=0,col="#CC79A7")
lines(smooth.spline(cc1adjust$chosenadjustedsum/cc1adjust$counter,spar=.6),lwd=2,lty=3,col="#CC79A7")
dev.off()

payoff <- 0
#pdf(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage1.pdf",sep=""))
png(file=paste("./QualityAcceptAtPosition-","payoff",payoff,"stage1.png",sep=""),
     width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.5,4.1,1.7))
plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     #ylim=c(-2, 1),
     #ylim=c(4500, 8000),
     ylim=c(4500, 8400),
     # axes=FALSE,
     #bty="n", # this remove the borders but not the axes not to the XY-origin
     bty="l", # this remove the borders but not the axes to the XY origin
     #xaxt="n",
     yaxt="n",
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position in test rounds",
     ylab="Value"
     #ylab="Z-score of Quality relative to mean"
     )
# add back the axis ticks' intervals
#axis(side = 1, at = c(1,5,10,15,20,25))
axis(side = 2, at = c(5000,6000,7000,8000), cex.axis=1.7)
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
lines(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#56B4E9")
points(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=23,col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
#lines(1:25,rev(indiffcardnegEmpirical),lwd=2,lty=3,col="#56B4E9")
lines(1:25,indiffcardnegEmpirical,lwd=2,lty=3,col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
lines(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#009E73")
points(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=21,col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
#lines(1:25,rev(indiffcarduniEmpirical),lwd=2,lty=4,col="#009E73")
lines(1:25,indiffcarduniEmpirical,lwd=2,lty=4,col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
lines(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,lty=1,col="#CC79A7")
points(1:25,cc1adjust$chosenadjustedsum/cc1adjust$counter,pch=22,col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
#lines(1:25,rev(indiffcardposEmpirical),lwd=2,lty=2,col="#CC79A7")
lines(1:25,indiffcardposEmpirical,lwd=2,lty=2,col="#CC79A7")
dev.off()

png(file=paste("./PercentileAcceptAtPosition-","payoff",payoff,"stage1.png",sep=""),
     width=7, height=7, units='in', res=600)
par(mar=c(5.1,4.5,4.1,1.7))
plot(5,5, type='n',cex.lab=1.8,cex.axis=1.7,
     #xlim=c(0, 25),
     xlim=c(1, 25),
     #ylim=c(-2, 1),
     #ylim=c(4500, 8000),
     ylim=c(0.4, 1),
     # axes=FALSE,
     #bty="n", # this remove the borders but not the axes not to the XY-origin
     bty="l", # this remove the borders but not the axes to the XY origin
     #xaxt="n",
     yaxt="n",
     # ann=FALSE,
     #main="Quality accepted by participants at position",
     xlab="Position in test rounds",
     ylab="Percentile"
     #ylab="Z-score of Quality relative to mean"
     )
# add back the axis ticks' intervals
#axis(side = 1, at = c(1,5,10,15,20,25))
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1), cex.axis=1.7)
payoff <- 0
distribution <- 2 ## tableneg in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[3]]),cc1adjust$chosenadjustedsum/cc1adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#56B4E9")
points(1:25,chosenPercentile,pch=23,col="#56B4E9", bg=adjustcolor("#56B4E9", alpha.f=0.5))
lines(1:25,indiffcardnegEmpiricalPercent,lwd=2,lty=3,col="#56B4E9")
distribution <- 0 ## tableuni in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[1]]),cc1adjust$chosenadjustedsum/cc1adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#009E73")
points(1:25,chosenPercentile,pch=21,col="#009E73", bg=adjustcolor("#009E73", alpha.f=0.5))
lines(1:25,indiffcarduniEmpiricalPercent,lwd=2,lty=4,col="#009E73")
distribution <- 1 ## tablepos in sp_learn.php
cc1clean <- subset(completedandcleaned, valuepayoff==payoff & dist==distribution & stage==1, select=c(sessionid,chosen,position))
chosenadjustedsum <- rep(0, 25)
counter <- rep(0, 25)
cc1adjust <- data.frame(cbind(chosenadjustedsum,counter))
for(i in 1:nrow(cc1clean)) {
    cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum <- cc1clean[i,]$chosen + cc1adjust[(cc1clean[i,]$position+1),]$chosenadjustedsum
    cc1adjust[(cc1clean[i,]$position+1),]$counter <- 1 + cc1adjust[(cc1clean[i,]$position+1),]$counter
}
chosenPercentile <- ecdf_fun(as.matrix(dumtable[[2]]),cc1adjust$chosenadjustedsum/cc1adjust$counter)
lines(1:25,chosenPercentile,lty=1,col="#CC79A7")
points(1:25,chosenPercentile,pch=22,col="#CC79A7", bg=adjustcolor("#CC79A7", alpha.f=0.5))
lines(1:25,indiffcardposEmpiricalPercent,lwd=2,lty=2,col="#CC79A7")
dev.off()

#pdf(file=paste("./QualityAcceptAtPosition-legend.pdf"))
png(file=paste("./QualityAcceptAtPosition-legend.png"),
     width=7, height=7, units='in', res=600)
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
#legendpars <- c(expression(paste("Value/Z-Score under ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Full knowledge optimal thresholds")),expression(paste("Value/Z-Score under ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Full knowledge optimal thresholds")),expression(paste("Value/Z-Score under ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Full knowledge optimal thresholds")))
#legend(1,1,xjust=0.5,yjust=0.5,legendpars, lty=c(1,3,1,4,1,2),lwd=1.5, pch=c(5,-1,0,-1,0,-1),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"),cex=1.5)
#legendpars <- c(expression(paste("Scores at positive skew, training")),expression(paste("Scores at positive skew, test")),expression(paste("Values"[exp]," with optimal strategy at positive skew")),expression(paste("Scores at uniform, training")),expression(paste("Scores at uniform, test")),expression(paste("Values"[exp]," with optimal strategy at uniform")),expression(paste("Scores at negative skew, training")),expression(paste("Scores at negative skew, test")),expression(paste("Values"[exp]," with optimal strategy at negative skew")))
legendpars <- c(expression(paste("Positive skew, training")),expression(paste("Positive skew, test")),expression(paste("Positive skew, optimal strategy")),expression(paste("Uniform, training")),expression(paste("Uniform, test")),expression(paste("Uniform, optimal strategy")),expression(paste("Negative skew, training")),expression(paste("Negative skew, test")),expression(paste("Negative skew, optimal strategy")))
#legend(x="bottomleft",inset=c(0,-0.65),legendpars, lty=c(1,3,1,4,1,2), lwd=1.5, pch=c(5,-1,0,-1,0,-1),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"),cex=1.5)
legend(1,1,xjust=0.5,yjust=0.5,legendpars, lty=c(1,1,2,1,1,4,1,1,3), lwd=2, pt.cex=2.4, pch=c(0,15,-1,1,19,-1,5,18,-1),col=c("#CC79A7","#CC79A7","#CC79A7","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9"),cex=1.4, bty="n")
dev.off()

pdf(file=paste("./QualityAcceptAtPosition-legend-old.pdf"))
par(mar=c(0,0,0,0), xpd=TRUE)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legendpars <- c(expression(paste("Value/Z-Score under ",alpha,"=3.7,",beta,"=1","   (-ve skew)")),expression(paste("Smoothed curve (for reference)")),expression(paste("Value/Z-Score under ",alpha,"=1,","   ",beta,"=1","   (uniform)")),expression(paste("Smoothed curve (for reference)")),expression(paste("Value/Z-Score under ",alpha,"=1,","   ",beta,"=3.7","(+ve skew)")),expression(paste("Smoothed curve (for reference)")))
legend(1,1,xjust=0.5,yjust=0.5,legendpars, lty=c(1,3,1,3,1,3),lwd=1.5, pch=c(5,-1,1,-1,0,-1),col=c("#56B4E9","#56B4E9","#009E73","#009E73","#CC79A7","#CC79A7"))
dev.off()

###### payoff
### extract final payoff per individual
##### extract success rate per individual (stage 0 & stage 1)
##### extract mean payoff per individual (stage 0 & stage 1)
### calculate mean total payoff per 3x2 conditions
