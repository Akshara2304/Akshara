#HYPERPARAMETERS
SHORT_CUTOFF <- 0.75
YEAR_START<-2000
YEAR_END<-2018
NORM <- FALSE
#LOAD STANDINGS DATA
# dat_std<-loadStandings(YEAR_START,2019)
# #source("loadData.R")
dat_std=read.csv("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\standings.csv",header=TRUE)
# LOAD MVP DATA
dat_mvp<-read.csv("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\mvp.csv",header=TRUE)
# LOAD TOTAL PLAYER STATS
dat_totals<-read.csv("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\totals.csv",header=TRUE)
# LOAD PERGAME PLAYER STATS
dat_pergame<-read.csv("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\pergame.csv",header=TRUE)
# LOAD advanced STATS
dat_adv<-read.csv("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\advanced.csv",header=TRUE)

# TOTAL STATS MODELS 
# fit shortlist model
predMVPs <- function(param.dat,param.mod,lasso=FALSE,bestlam=0){
  # make predictions
  if (lasso == TRUE) {
    ## lasso case
    predict(param.mod,s=bestlam,newx=param.dat)
  } else {
    ## linear/logistic cases
    pred<-predict(param.mod,newdata=param.dat,type="response")
  }
  # create empty MVPs dataframe
  mvps<-param.dat[0,]
  for (year in levels(as.factor(param.dat$Season))) {
    dat_temp<-param.dat[which(param.dat$Season==year),]
    temp_preds<-pred[which(param.dat$Season==year)]
    mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
    mvps<-data.frame(rbind(as.matrix(mvps), as.matrix(mvp)))
  }
  return(mvps)
}
calcAccuracy <- function(mvps,ranks) {
  if (ranks){
    errs<-mvps[which(mvps$Rank!=1),]
  } else {
    errs<-mvps[which(mvps$MVP==FALSE),]
  }
  print(errs)
  return(1-(dim(errs)[1]/dim(mvps)[1]))
}

accuracyCV <- function(param.dat,param.mod){
  # extract formula from model
  mvps<-param.dat[0,]
  for (year in min(param.dat$Season):max(param.dat$Season)) {
    # leave out one year
    test <- param.dat[which(param.dat$Season == year),]
    train <- param.dat[which(param.dat$Season != year),]
    # train model on remaining
    ## fit model
    if (param.mod$call[1]=="glm()"){
      ## logistic
      cv <- glm(param.mod$call[2],data=train,family = binomial(link = "logit"))
    } else {
      ## linear
      cv <- lm(param.mod$call[2],data=train)
    }
    ## linear pred
    cv.pred<-predict(cv,newdata=test,type="response")
    cv.mvp<-test[which(cv.pred == max(cv.pred)),]
    mvps <- data.frame(rbind(as.matrix(mvps), as.matrix(cv.mvp)))
    # test on leaveout
  }
  return(calcAccuracy(mvps,FALSE))
}
head(dat_totals)
dat_totals$Shortlist<-dat_totals$Share!=0
head(dat_totals)
tot.short.mod <- glm(Shortlist~G+MP+X3P+DRB+AST+
                       BLK+TOV+PF+PTS+Team.Wins,data=dat_totals,family = binomial(link = "logit"))
# grab shortlist
tot.shortlist<-dat_totals[which(predict(tot.short.mod,type="response")>SHORT_CUTOFF),]
## fit linear
tot.lm <- lm(First~G+X3P+DRB+AST+BLK+PF+PTS+Team.Wins,data=tot.shortlist)
# linear pred
# library(Metrics)
# tot.lm.pred<-predict(tot.lm,data=tot.shortlist)
# tot.shortlist$pred=tot.lm.pred
# head(tot.shortlist)
# tot.lm.acc<- mean(abs(min(tot.shortlist$First,tot.shortlist$pred)/max(tot.shortlist$First,tot.shortlist$pred)))
# linear pred
tot.lm.pred<-predMVPs(tot.shortlist,tot.lm)
tot.lm.acc<-calcAccuracy(tot.lm.pred,FALSE)

## fit logit
tot.log <- glm(MVP~G+X3P+DRB+AST+BLK+PF+PTS+Team.Wins,data=tot.shortlist,family = binomial(link = "logit"))
# tot.log.pred<-predict(tot.log,data=tot.shortlist)
# tot.shortlist$pred1=tot.log.pred
head(tot.shortlist)
# tot.log.acc<- mean(abs(min(tot.shortlist$MVP,tot.shortlist$pred1)/max(tot.shortlist$MVP,tot.shortlist$pred1)))
# logit pred
tot.log.pred<-predMVPs(tot.shortlist,tot.log)
tot.log.acc<-calcAccuracy(tot.log.pred,FALSE)

## LASSO 
library(glmnet)
x = model.matrix(First~as.factor(Pos)+Age+G+GS+MP+FG+FGA+FG.+X3P+X3PA+X3P.+X2P+X2PA+X2P.+
                   eFG.+FT+FTA+FT.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,data=tot.shortlist)
y = tot.shortlist$First
tot.cv.out <- cv.glmnet(x,y,alpha=1)
lasso.coef=predict(tot.cv.out,type="coefficients",s=tot.cv.out$lambda.min)

## fit poly
tot.poly <- lm(First~poly(G,3)+poly(X3P,3)+poly(DRB,3)+poly(AST,3)+poly(BLK,3)+poly(PF,3)+
                 poly(PTS,3)+poly(Team.Wins,3),data=tot.shortlist)
# ## poly pred
# tot.poly.pred<-predict(tot.poly,data=tot.shortlist)
# tot.shortlist$pred2=tot.poly.pred
# head(tot.shortlist)
# poly pred
tot.poly.pred<-predMVPs(tot.shortlist,tot.poly)
tot.poly.acc<-calcAccuracy(tot.poly.pred,FALSE)
# tot.poly.acc<- mean(abs(min(tot.shortlist$First,tot.shortlist$pred2)/max(tot.shortlist$First,tot.shortlist$pred2)))
# PERGAME STATS MODELS
## fit shortlist model
dat_pergame$Shortlist<-dat_pergame$Share!=0
head(dat_pergame)
pg.short.mod <- glm(Shortlist~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,
                    data=dat_pergame,family = binomial(link = "logit"))
## grab shortlist
pg.shortlist<-dat_pergame[which(predict(pg.short.mod,type="response")>SHORT_CUTOFF),]

## fit linear
pg.lm <- lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins
            ,data=pg.shortlist)
# ## linear pred
# pg.lm.pred<-predict(pg.lm,data=pg.shortlist)
# linear pred
pg.lm.pred<-predMVPs(pg.shortlist,pg.lm)
pg.lm.acc<-calcAccuracy(pg.lm.pred,FALSE)
# fit logit
pg.log <- glm(MVP~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins
              ,data=pg.shortlist,family = binomial(link = "logit"))
# # logit pred
# pg.log.pred<-predict(pg.log,data=pg.shortlist)
# logit pred
pg.log.pred<-predMVPs(pg.shortlist,pg.log)
pg.log.acc<-calcAccuracy(pg.log.pred,FALSE)
# fit poly
pg.poly <- lm(First~poly(Age,3)+poly(G,3)+poly(GS,3)+poly(MP,3)+poly(FG,3)+poly(FG.,3)+poly(X3P,3)+poly(X3P.,3)+poly(X2P.,3)+
                poly(FT.,3)+poly(ORB,3)+poly(DRB,3)+poly(AST,3)+poly(STL,3)+poly(BLK,3)+poly(TOV,3)+poly(PF,3)+
                poly(PTS,3)+poly(Team.Wins,3),data=pg.shortlist)
## poly pred
pg.poly.pred<-predMVPs(pg.shortlist,pg.poly)
pg.poly.acc<-calcAccuracy(pg.poly.pred,FALSE)
# ADVANCED STATS MODELS
# fit shortlist model
dat_adv$Shortlist<-dat_adv$Share!=0
adv.short.mod <- glm(Shortlist~G+MP+PER+TS.+X3PAr+FTr+ORB.+DRB.+TRB.+AST.+STL.+BLK.+
                       TOV.+USG.+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+Team.Wins,
                     data=dat_adv,family = binomial(link = "logit"))
## grab shortlist
adv.shortlist<-dat_adv[which(predict(adv.short.mod,type="response")>SHORT_CUTOFF),]
# fit linear
adv.lm <- lm(First~G+PER+TS.+X3PAr+FTr+TRB.+AST.+STL.+BLK.+
               TOV.+USG.+WS+BPM+VORP+Team.Wins,
             data=adv.shortlist)
# # linear pred
# adv.lm.pred<-predict(adv.lm,data=adv.shortlist)
# linear pred
adv.lm.pred<-predMVPs(adv.shortlist,adv.lm)
adv.lm.acc<-calcAccuracy(adv.lm.pred,FALSE)
# fit logit
adv.log <- glm(MVP~PER+TS.+X3PAr+FTr+TRB.+AST.+STL.+BLK.+
                 TOV.+USG.+WS+BPM+VORP+Team.Wins
               ,data=adv.shortlist,family = binomial(link = "logit"))
# ## logit pred
# adv.log.pred<-predict(adv.log,data=adv.shortlist)
# logit pred
adv.log.pred<-predMVPs(adv.shortlist,adv.log)
adv.log.acc<-calcAccuracy(adv.log.pred,FALSE)
# LASSO 
#library(glmnet)
x = model.matrix(First~G+MP+PER+TS.+X3PAr+FTr+ORB.+DRB.+TRB.+AST.+STL.+BLK.+
                   TOV.+USG.+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+Team.Wins
                 ,data=adv.shortlist)
y = adv.shortlist$First
adv.cv.out <- cv.glmnet(x,y,alpha=1)
adv.lasso.coef=predict(adv.cv.out,type="coefficients",s=adv.cv.out$lambda.min)
# fit poly
adv.poly <- lm(First~poly(PER,3)+poly(TS.,3)+poly(X3PAr,3)+poly(FTr,3)+poly(TRB.,3)+poly(AST.,3)+poly(STL.,3)+poly(BLK.,3)+
                 poly(TOV.,3)+poly(USG.,3)+poly(WS,3)+poly(BPM,3)+poly(VORP,3)+poly(Team.Wins,3),
               data=adv.shortlist)
# # poly pred
# adv.poly.pred<-predict(adv.poly,data=adv.shortlist)
# poly pred
adv.poly.pred<-predMVPs(adv.shortlist,adv.poly)
adv.poly.acc<-calcAccuracy(adv.poly.pred,FALSE)

# MERGED DATA 
# merge data
dat_merge <- dat_totals
adv_subset <- dat_adv[,c(2,3,12,13,14,15,18,19,20,21,22,23,26,30,31)]
dat_merge <- merge(dat_merge,adv_subset,by=c("Player","Season"))
# grab shortlist
merge.shortlist<-dat_merge[which(predict(tot.short.mod,newdata = dat_merge,type="response")>SHORT_CUTOFF),]
# predict for each model
merge.tot.pred<-predict(tot.log,newdata=merge.shortlist,type="response")
merge.adv.pred<-predict(adv.log,newdata=merge.shortlist,type="response")
merge.pred <- (merge.tot.pred + merge.adv.pred) / 2
# create empty MVPs dataframe
merge.mvps<-merge.shortlist[0,]
for (year in levels(as.factor(merge.shortlist$Season))) {
  dat_temp<-merge.shortlist[which(merge.shortlist$Season==year),]
  temp_preds<-merge.pred[which(merge.shortlist$Season==year)]
  mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
  merge.mvps<-data.frame(rbind(as.matrix(merge.mvps), as.matrix(mvp)))
}
# fit linear
merge.lm <- lm(First~G+PER+TS.+X3PAr+FTr+TRB.+AST.+STL.+BLK.+
                 TOV.+USG.+WS+BPM+VORP+Team.Wins+X3P+DRB+
                 AST+BLK+PF+PTS,
               data=merge.shortlist)
## linear pred
# merge.lm.pred<-predict(merge.lm,data=merge.shortlist)
# linear pred
merge.lm.pred<-predMVPs(merge.shortlist,merge.lm)
merge.lm.acc<-calcAccuracy(merge.lm.pred,FALSE)
# fit logit
merge.log <- glm(MVP~G+PER+TS.+X3PAr+FTr+TRB.+AST.+STL.+BLK.+
                   TOV.+USG.+WS+BPM+VORP+Team.Wins+X3P+DRB+
                   AST+BLK+PF+PTS,
                 data=merge.shortlist,family = binomial(link = "logit"))
# # logit pred
# merge.log.pred<-predict(merge.log,data=merge.shortlist)
# logit pred
merge.log.pred<-predMVPs(merge.shortlist,merge.log)
merge.log.acc<-calcAccuracy(merge.log.pred,FALSE)

# fit poly
merge.poly <- lm(First~poly(G,3)+poly(PER,3)+poly(TS.,3)+poly(X3PAr,3)+poly(FTr,3)+poly(TRB.,3)+
                   poly(AST.,3)+poly(STL.,3)+poly(BLK.,3)+poly(TOV.,3)+poly(USG.,3)+poly(WS,3)+
                   poly(BPM,3)+poly(VORP,3)+poly(Team.Wins,3)+poly(PER,3)+poly(TS.,3)+poly(X3PAr,3)+
                   poly(FTr,3)+poly(TRB.,3)+poly(AST.,3)+poly(STL.,3)+poly(BLK.,3)+poly(TOV.,3)+
                   poly(USG.,3)+poly(WS,3)+poly(BPM,3)+poly(VORP,3),
                 data=merge.shortlist)
## poly pred
# merge.poly.pred<-predict(merge.poly,data=merge.shortlist)
merge.poly.pred<-predMVPs(merge.shortlist,merge.poly)
merge.poly.acc<-calcAccuracy(merge.poly.pred,FALSE)
# logit pred
merge.log.pred<-predMVPs(merge.shortlist,merge.log)
merge.log.acc<-calcAccuracy(merge.log.pred,FALSE)

# CROSS VALIDATION
# total data
tlm.cv.acc<-accuracyCV(tot.shortlist,tot.lm)
tlog.cv.acc<-accuracyCV(tot.shortlist,tot.log)
tpoly.cv.acc<-accuracyCV(tot.shortlist,tot.poly)
# per game data
plm.cv.acc<-accuracyCV(pg.shortlist,pg.lm)
plog.cv.acc<-accuracyCV(pg.shortlist,pg.log)
ppoly.cv.acc<-accuracyCV(pg.shortlist,pg.poly)
# advanced data
alm.cv.acc<-accuracyCV(adv.shortlist,adv.lm)
alog.cv.acc<-accuracyCV(adv.shortlist,adv.log)
apoly.cv.acc<-accuracyCV(adv.shortlist,adv.poly)
# merged data
mlm.cv.acc<-accuracyCV(merge.shortlist,merge.lm)
mlog.cv.acc<-accuracyCV(merge.shortlist,merge.log)
mpoly.cv.acc<-accuracyCV(merge.shortlist,merge.poly)
aggregateCV <- function(param.dat,mod1,mod2){
  mvps<-param.dat[0,]
  for (year in min(param.dat$Season):max(param.dat$Season)){
    # leave out one year
    test <- param.dat[which(param.dat$Season == year),]
    train <- param.dat[which(param.dat$Season != year),]
    # train model on remaining
    ### FIRST MODEL
    if (mod1$call[1]=="glm()"){
      ## logistic
      cv1 <- glm(mod1$call[2],data=train,family = binomial(link = "logit"))
    } else {
      ## linear
      cv1 <- lm(mod1$call[2],data=train)
    }}}
# aggregate model
agglm.cv.acc<-aggregateCV(merge.shortlist,tot.lm,pg.lm)
agglog.cv.acc<-aggregateCV(merge.shortlist,tot.log,pg.log)
aggpoly.cv.acc<-aggregateCV(merge.shortlist,tot.poly,pg.poly)
# SUMMARY PLOTS
accs <- data.frame(Data=c("dat_tot","dat_pergame","dat_adv","dat_merge"),
                   Linear=c(tot.lm.acc,pg.lm.acc,adv.lm.acc,merge.lm.acc),
                   Logistic=c(tot.log.acc,pg.log.acc,adv.log.acc,merge.log.acc),
                   Cubic=c(tot.poly.acc,pg.poly.acc,adv.poly.acc,merge.poly.acc),
                   LOOCV.lm=c(tlm.cv.acc,plm.cv.acc,alm.cv.acc,mlm.cv.acc),
                   LOOCV.log=c(tlog.cv.acc,plog.cv.acc,alog.cv.acc,mlog.cv.acc),
                   LOOCV.poly=c(tpoly.cv.acc,ppoly.cv.acc,apoly.cv.acc,mpoly.cv.acc))
par(mfrow=c(1,1))
#library(RColorBrewer)
library(viridis)
vir=viridis(6)
xx=barplot(c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log,accs$Cubic,accs$LOOCV.poly), 
           names.arg=rep(accs$Data,times=6),
           ylim=c(0,1),
           las=2,
           col=c(rep(vir[1],4),rep(vir[2],4),rep(vir[3],4),rep(vir[4],4),rep(vir[5],4),rep(vir[6],4)),
           ylab="Accuracy", 
           main=paste("Model Accuracies -",YEAR_START,"-",YEAR_END)
)
if(NORM){
  str<-"Normalized -"
} else {
  str<-"Unnormalized -"
}
mtext(paste(str,"Shortlist",SHORT_CUTOFF,"cutoff"))
text(x=xx,
     y=c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log,accs$Cubic,accs$LOOCV.poly),
     label=round(c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log,accs$Cubic,accs$LOOCV.poly),digits=3),
     pos=3
) 
legend("bottomright", 
       c("Linear","Linear LOOCV","Logistic","Logistic LOOCV","Cubic","Cubic LOOCV"), 
       fill=vir, 
       cex=0.8
)


