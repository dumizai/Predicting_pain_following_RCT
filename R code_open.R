##Variable selection – multilevel LASSO:
library(glmmLasso)
lambda <- seq(500,0,by=-5)
AUC_vec<-rep(Inf,length(lambda))
family = binomial(link = 'logit')
Delta.start<-as.matrix(t(rep(0,7+23)))
Q.start<-0.1  

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  glm <- glmmLasso(data$Onewkpain ~ 1+data$Q04P0AGE +as.factor(data$Q04P0FEAR)+as.factor(Q04P0OUTC)+as.factor(Q04P0PAMED)+as.factor(Q04P0NONE)   
                   +as.factor(Q04P0DULL)+as.factor(Q04P0SHRP)+as.factor(Q04P0ACH)+as.factor(Q04P0THROB)+as.factor(Q04P0BURN)
                   +as.factor(Q04P0SHOOT)+as.factor(Q04P0ELECT)
                   +as.factor(Q04P0SPON)+as.factor(Q04P0PROV)+as.factor(Q04P0NOWRS)+as.factor(Q04P0WORSE)
                   +as.factor(Q04P0BITE)+as.factor(Q04P0TEMP)+as.factor(Q04P0STRES)  
                   +as.factor(Q04P0NWPAIN)+as.factor(Q04P0WSTPAIN)+as.factor(Q04P0AVGPAIN)
                   +as.factor(Q04P0NOACDAY)+as.factor(Q04P0NODAC)+as.factor(Q04P0NOREC)+as.factor(Q04P0NOWRK)   
                   +as.factor(Q04P0TOOTHPA)+as.factor(Q04P0BODYPA)+as.factor(Q04P0LFSMK100)+as.factor(Q04P0SMKNOW)
                   +as.factor(Q04P0SMKDAY)+as.factor(Q04P0DIAB)+as.factor(Q04P0GENDER)  
                   +as.factor(Q04P0HISP)+as.factor(Q04P0DENINS)+as.factor(Q04P0INCOME)+as.factor(Q04P0EDU)
                   +as.factor(Q05D0ENDO)+as.factor(Q05D0PERC)   
                   +as.factor(Q05D0BITE)+as.factor(Q05D0COLD)+as.factor(Q05D0COLDPROL)+as.factor(Q05D0DEEP)
                   +as.factor(Q05D0DEEPLOC)+as.factor(Q_07D1BLEED)+as.factor(Q_07D1PULPECT) +as.factor(Q_07D1CLEAN)+as.factor(Q_07D1NOTNEG)+as.factor(Q_07D1SEPAR)+as.factor(Q_07D1IAFILE)
                   +as.factor(Q_07D1HARDER)+as.factor(Q_07D1ANESQ)+as.factor(Q_07D1FISTULA)+as.factor(Q_07D1SWELL)+as.factor(Q_07D1SMINJ)
                   +as.factor(Q_07D1DIFINJ)+as.factor(Q_07D1PDLINJ)+as.factor(Q_07D1AOSINJ) 
                   +as.factor(Q_07D1APINJ)+as.factor(Q_07D1PAIN6MO)+as.factor(Q_09P1PAIN)+as.factor(Q_09P1NUMB)
                   +as.factor(Q_09P1FEAR)+as.factor(PMOPIOD)    
                   +as.factor(PMNONOPIOD)+as.factor(PMNO)+as.factor(PMGENANES)+as.factor(PMNONE)
                   +as.factor(Q04P0RACEv2)+as.factor(Maxillary)+as.factor(ToothSite)+as.factor(STATE)
                    ,rnd = list(PRACID=~1+$DDSgender+DDShispanic+SPETY+DecGrad),
                    family = family, data = data, 
                    lambda=lambda[j], switch.NR=F,final.re=TRUE,
                    control = list(start=Delta.start[j,],q_start=Q.start[j]))  
  print(colnames(glm$Deltamatrix)[2:7][glm$Deltamatrix[glm$conv.step,2:7]!=0])
  AUC_vec[j]<-glm$auc
  Delta.start<-rbind(Delta.start,glm$Deltamatrix[glm$conv.step,])
  Q.start<-c(Q.start,glm$Q_long[[glm$conv.step+1]])
}
opt<-which.min(AUC_vec)
glm_final <- glmmLasso(data$Onewkpain ~ 1+data$Q04P0AGE +as.factor(data$Q04P0FEAR)+as.factor(Q04P0OUTC)+as.factor(Q04P0PAMED)+as.factor(Q04P0NONE)   
                       +as.factor(Q04P0DULL)+as.factor(Q04P0SHRP)+as.factor(Q04P0ACH)+as.factor(Q04P0THROB)+as.factor(Q04P0BURN)
                       +as.factor(Q04P0SHOOT)+as.factor(Q04P0ELECT)
                       +as.factor(Q04P0SPON)+as.factor(Q04P0PROV)+as.factor(Q04P0NOWRS)+as.factor(Q04P0WORSE)
                       +as.factor(Q04P0BITE)+as.factor(Q04P0TEMP)+as.factor(Q04P0STRES)  
                       +as.factor(Q04P0NWPAIN)+as.factor(Q04P0WSTPAIN)+as.factor(Q04P0AVGPAIN)
                       +as.factor(Q04P0NOACDAY)+as.factor(Q04P0NODAC)+as.factor(Q04P0NOREC)+as.factor(Q04P0NOWRK)   
                       +as.factor(Q04P0TOOTHPA)+as.factor(Q04P0BODYPA)+as.factor(Q04P0LFSMK100)+as.factor(Q04P0SMKNOW)
                       +as.factor(Q04P0SMKDAY)+as.factor(Q04P0DIAB)+as.factor(Q04P0GENDER)  
                       +as.factor(Q04P0HISP)+as.factor(Q04P0DENINS)+as.factor(Q04P0INCOME)+as.factor(Q04P0EDU)
                       +as.factor(Q05D0ENDO)+as.factor(Q05D0PERC)   
                       +as.factor(Q05D0BITE)+as.factor(Q05D0COLD)+as.factor(Q05D0COLDPROL)+as.factor(Q05D0DEEP)
                       +as.factor(Q05D0DEEPLOC)+as.factor(Q_07D1BLEED)+as.factor(Q_07D1PULPECT) +as.factor(Q_07D1CLEAN)+as.factor(Q_07D1NOTNEG)+as.factor(Q_07D1SEPAR)+as.factor(Q_07D1IAFILE)
                       +as.factor(Q_07D1HARDER)+as.factor(Q_07D1ANESQ)+as.factor(Q_07D1FISTULA)+as.factor(Q_07D1SWELL)+as.factor(Q_07D1SMINJ)
                       +as.factor(Q_07D1DIFINJ)+as.factor(Q_07D1PDLINJ)+as.factor(Q_07D1AOSINJ) 
                       +as.factor(Q_07D1APINJ)+as.factor(Q_07D1PAIN6MO)+as.factor(Q_09P1PAIN)+as.factor(Q_09P1NUMB)
                       +as.factor(Q_09P1FEAR)+as.factor(PMOPIOD)    
                       +as.factor(PMNONOPIOD)+as.factor(PMNO)+as.factor(PMGENANES)+as.factor(PMNONE)
                       +as.factor(Q04P0RACEv2)+as.factor(Maxillary)+as.factor(ToothSite)+as.factor(STATE)
                       , rnd = list(PRACID=~1+$DDSgender+DDShispanic+SPETY+DecGrad),  
                        family = family, data = data, lambda=lambda[opt],
                        switch.NR=F,final.re=TRUE,
                        control = list(start=Delta.start[opt,],q_start=Q.start[opt]))  
summary(glm_final)

##Model development - Multilevel logistic regression models:
library(lme4)
model<-glmer(Onewkpain~Q04P0STRES+Q04P0NWPAIN+Q04P0NOACDAY+Q04P0EDU+Q04P0NOREC+Q04P0NOWRK+Q_09P1PAIN+Q04P0RACEv2+STATE
             +Q_07D1SWELL+Q_07D1BLEED+PMNO+Q_09P1NUMB
             +(1|data_1wk.PRACID)+(1|DDShispanic)+(1|DDSgender), 
             data=data, family=binomial(link="logit"))

## Model performance evaluation - H measures:
library(hmeasure)
hm1<-HMeasure(data$Onewkpain,predicted_y)
