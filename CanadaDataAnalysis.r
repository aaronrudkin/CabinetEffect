rm(list=ls())
library(survival) # Survival modeling
library(stargazer) # Output tables
library(sandwich) # Robust SEs
library(lmtest) # Robust SEs
library(dplyr) # Data manipulation 
library(car) # Diagnostics of regression
library(ebal) # Entropy balance weighting
library(Matching) # Matching
library(cobalt) # Love plots for matching
library(ggplot2) # cobalt love.plot override
library(sampleSelection) # Heckman correction
library(mediation) # Mediation analysis

set.seed(19861108)
setwd("~/Dropbox/Field Paper Revisions/")
load("Canada Data/merge/prep_data_final.RData")
# ENSURE THAT WORKING DIRECTORY IS SET PROPERLY TO ROOT OF FIELD PAPER

#### Linear regression: Vote share ####
# Cross-sectional, party fixed effects: 2.45
model.OLS.CS = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party), data=incumbents)
# Cross-sectional, party-year fixed effects, 2.19 -- note that there are multicollinearity problems with PartyInGovt and some party-year fixed effects.
model.OLS.CS.PY = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party)*as.factor(Date), data=incumbents)
# Control for demographics: party fixed effects: 2.42
model.OLS.CS.MemberDemo = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party) + Gender + PastLawJob, data=incumbents)

# Cross-sectional, past performance, 1.41
model.OLS.Control = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + LastVotePct, data=incumbents)
# Within-member: 0.60 (CI admits 0)
model.OLS.withinMember = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(ID), data=incumbents)

#### How many races would flip given coefficient estimate? ####
# Get the margin of victory
coeff_cab = model.OLS.CS$coefficients[2]
coeff_cab_2 = model.OLS.withinMember$coefficients[2]
margin_victory = dataT %>% group_by(Date, Riding) %>% arrange(desc(VotePct)) %>% slice(1:2) %>% mutate(diff_win = max(VotePct) - min(VotePct)) %>% dplyr::select(Date, Riding, diff_win) %>% distinct(Date, Riding, diff_win)
table(margin_victory$diff_win <= coeff_cab)
table(margin_victory[margin_victory$Date=="2015-10-19",]$diff_win <= coeff_cab)

table(margin_victory$diff_win <= coeff_cab_2)
table(margin_victory[margin_victory$Date=="2015-10-19",]$diff_win <= coeff_cab_2)

#### Heckman Correction: Vote Share ####
# Heckman Correction
merge_inc = incumbents %>% dplyr::select(ID, Name.x, Incumbent, Date, Elected, VotePct, CabinetNow, CabinetImportant, CabinetPM, TermsServed, Party, Gender, Province, PartyInGovt, LastVotePct, PastLawJob) %>% mutate(Stub=0)
merge_inc = rbind(merge_inc, ghostObs)
merge_inc = merge_inc %>% mutate(Ran = (Stub-1)*-1)

# Heckman Correction: 2.35
model.heckman.CS = heckit(selection = Ran ~ CabinetNow + PartyInGovt + TermsServed + LastVotePct + Gender + as.factor(Province) + PastLawJob,
                                   outcome = VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party),
                                   method = "ml",
                                   data=merge_inc)

# Heckman Correction: 1.09
model.heckman.CS.Control = heckit(selection = Ran ~ CabinetNow + PartyInGovt + TermsServed + LastVotePct + Gender + as.factor(Province) + PastLawJob,
                          outcome = VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party) + LastVotePct,
                          method = "ml",
                          data=merge_inc)

# Heckman Correction ind fixed effects: 0.61 (CI contains 0) 
model.heckman.withinMember = heckit(selection = Ran ~ CabinetNow + PartyInGovt + TermsServed + LastVotePct + Gender + as.factor(Province) + PastLawJob,
                          outcome = VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(ID),
                          method = "ml",
                          data=merge_inc)

#### Logit Models: Probability of re-election ####
# Logit: 0.30 logit est -> 71.7% to 77.5% percentage elected for modal 1st term liberal in government case flipping 
model.logit.base = glm(Elected ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=incumbents)
# 0.30 logit est in survival-ish (conditional logit) context.
model.logit.cond = glm(Elected ~ CabinetNow + PartyInGovt + as.factor(TermsServed) + as.factor(Party), family=binomial(link=logit), data=incumbents)

#### Selection on Observables: Matching ####
matchCovars = c("Gender","TermsServed","LastVotePct","PartyInGovt", "PastLawJob")
provCovars = c("PROV_1", "PROV_2", "PROV_3", "PROV_4", "PROV_5", 
               "PROV_6", "PROV_7", "PROV_8", "PROV_9", "PROV_10", 
               "PROV_11", "PROV_12", "PROV_13")
exactMatch = c(TRUE, rep(FALSE, 4), rep(TRUE, 13))
matchCovars = c(matchCovars, provCovars)

# 1-to-1 Matching: 1.70
model.match.out.all = Match(incumbents$VotePct, incumbents$CabinetNow, incumbents[,matchCovars], BiasAdjust=1, exact=exactMatch)
c(model.match.out.all$est, model.match.out.all$est - 1.96*model.match.out.all$se, model.match.out.all$est + 1.96*model.match.out.all$se)
# 1-to-1 Matching with Age as a control: 2.35
model.match.out.age = Match(dtAge$VotePct, dtAge$CabinetNow, dtAge[,c("Age",matchCovars)], BiasAdjust=1, exact=c(0, exactMatch))
c(model.match.out.age$est, model.match.out.age$est - 1.96*model.match.out.age$se, model.match.out.age$est + 1.96*model.match.out.age$se)

override_var_names_1 = c("Gender", "Prior Vote %", "Party In Government?", "Past Law Job?", 
                       "Province: AB", "Province: BC", "Province: MB",
                       "Province: NB", "Province: NL", "Province: NT",
                       "Province: NS", "Province: NV", "Province: ON",
                       "Province: PE", "Province: QC", "Province: SK",
                       "Province: YT",
                       "Terms Served")

pdf("Includes/love_plot_match.pdf")
love.plot(bal.tab(model.match.out.all, treat=incumbents$CabinetNow, covs=incumbents[,matchCovars]),
          threshold=0.1, var.names=override_var_names_1, override.title="Covariate Balance: Matching")
dev.off()

pdf("Includes/love_plot_match_age.pdf")
love.plot(bal.tab(model.match.out.age, treat=dtAge$CabinetNow, covs=dtAge[,c("Age",matchCovars)]),
          threshold=0.1, var.names=c("Age", override_var_names_1), override.title="Covariate Balance: Matching w/ Age")
dev.off()

#### Selection on Observables: Weighting ####
# Drop Nunavut from the weighting -- multicollinearity issue
eb.out = ebalance(incumbents$CabinetNow, X=incumbents[,matchCovars[1:17]])
weights = rep(1, nrow(incumbents))
weights[incumbents$CabinetNow==0] = eb.out$w
Xes = incumbents[,matchCovars[1:17]]

# Entropy Balanced: 1.26
model.weight.out = lm(VotePct ~ CabinetNow + PartyInGovt + TermsServed + as.factor(Party), data=incumbents, weights = weights)
treatment_effect_1 = summary(model.weight.out)$coefficients[2,]
unname(c(treatment_effect_1[1], treatment_effect_1[1] - 1.96*treatment_effect_1[2], treatment_effect_1[1] + 1.96*treatment_effect_1[2]))

override_var_names_2 = c("Gender", "Prior Vote %", "Party In Government?", "Past Law Job?", 
                       "Province: AB", "Province: BC", "Province: MB",
                       "Province: NB", "Province: NL", "Province: NT",
                       "Provincce: NS", "Province: ON", "Province: PE", 
                       "Province: QC", "Province: SK", "Province: YT",
                       "Terms Served")

pdf("Includes/love_plot_weight.pdf")
love.plot(bal.tab(eb.out, treat=incumbents$CabinetNow, covs=incumbents[,matchCovars[1:17]]),
          threshold=0.1, var.names=override_var_names_2, override.title = "Covariate Balance: Entropy Balance Weighting")
dev.off()


#### Logit interpretation: All observations within the support. ####
swap_noncabinet = incumbents[incumbents$Party %in% c("Lib", "C", "P.C."),]
swap_noncabinet$CabinetNow = 0
swap_cabinet = swap_noncabinet
swap_cabinet$CabinetNow = 1
pred.0 = predict(model.logit.base, swap_noncabinet, type="response")
pred.1 = predict(model.logit.base, swap_cabinet, type="response")
mean(pred.1 - pred.0) # 4.8% increase

#### Cumulative Survival Plot ####
modal.noncabinet = data.frame(CabinetNow=rep(0, 11), 
                              PartyInGovt=rep(1,11), 
                              TermsServed=seq(1,11), 
                              Party=rep("Lib", 11))
cumulative.noncabinet.prob = cumprod(predict(model.logit.cond, modal.noncabinet, type="response"))

modal.cabinet = modal.noncabinet
modal.cabinet$CabinetNow = 1
cumulative.cabinet.prob = cumprod(predict(model.logit.cond, modal.cabinet, type="response"))

pdf("Includes/cumulative_survival.pdf")
plot(c(1,rep(seq(2,11),each=2),12), rep(cumulative.noncabinet.prob, each=2), type="l", 
     ylim=c(0.1,0.8), 
     xlab="Number of Terms Served", ylab="Cumulative Probability Of Re-Election at Term t", 
     main="Longitudinal Electoral Survival Rate")
lines(c(1,rep(seq(2,11),each=2),12)+0.05, 
      rep(cumulative.cabinet.prob, each=2), 
      type="l", col="red")
legend(8, 0.75, c("Cabinet", "Noncabinet"), lty=1, col=c("red", "black"), cex=0.8)
dev.off()

#### Figure: Participation Gov vs. Opp ####
pdf("Includes/participation_govopp.pdf")
plot(density(log(incumbents[incumbents$NumSpokenLines>0 & incumbents$PartyInGovt==0,]$NumSpokenLines)), 
     main="Parliamentary Participation",
     xlab="Log(Spoken Words in Parliament)", yaxt="n", col="blue")
lines(density(log(incumbents[incumbents$NumSpokenLines>0 & incumbents$PartyInGovt==1,]$NumSpokenLines)), col="red")
legend(5, 0.52, c("Government", "Opposition"), col=c("red","blue"), lty=1, cex=0.8, bty="n")
dev.off()

#### Figure: Participation Cabinet vs. Noncabinet ####
pdf("Includes/participation_cabinet.pdf")
plot(density(log(incumbents[incumbents$NumSpokenLines>0 & incumbents$CabinetNow==1,]$NumSpokenLines)), 
     main="Parliamentary Participation",
     xlab="Log(Spoken Words in Parliament)", yaxt="n", col="black", lty=2)
lines(density(log(incumbents[incumbents$NumSpokenLines>0 & incumbents$CabinetNow==0,]$NumSpokenLines)), col="black")
legend(6.5, 0.46, c("Cabinet", "Backbench"), col=c("black","black"), lty=c(1,2), cex=0.8, bty="n")
dev.off()

spokenLinesPredictor = lm(log(NumSpokenLines) ~ CabinetNow + TermsServed + PartyInGovt + Gender + PastLawJob + LastVotePct + Age + as.factor(Party), 
                          data=incumbents[incumbents$NumSpokenLines>0,])

summary(spokenLinesPredictor)

#### Figure: Media Mentions: Gov vs. Opp ####
pdf("Includes/media_govopp.pdf")
plot(density(log(incumbents[!is.na(incumbents$MediaMentions) & incumbents$MediaMentions>0 & incumbents$PartyInGovt==1,]$MediaMentions)), 
     main="Media Mentions",
     xlab="Log(Media Mentions)", yaxt="n", col="blue")
lines(density(log(incumbents[!is.na(incumbents$MediaMentions) & incumbents$MediaMentions>0 & incumbents$PartyInGovt==0,]$MediaMentions)), col="red")
legend(6.5, 0.28, c("Government", "Opposition"), col=c("red","blue"), lty=1, cex=0.8, bty="n")
dev.off()

#### Figure: Media Mentions: Cabinet vs. Noncabinet ####
pdf("Includes/media_cabinet.pdf")
plot(density(log(incumbents[!is.na(incumbents$MediaMentions) &incumbents$MediaMentions>0 & incumbents$CabinetNow==1,]$MediaMentions)), 
     main="Media Mentions",
     xlab="Log(Media Mentions)", yaxt="n", col="black", lty=1)
lines(density(log(incumbents[!is.na(incumbents$MediaMentions) &incumbents$MediaMentions>0 & incumbents$CabinetNow==0,]$MediaMentions)), col="black", lty=2)
legend(7.5, 0.38, c("Cabinet", "Backbench"), col=c("black","black"), lty=c(1,2), cex=0.8, bty="n")
dev.off()

summary(incumbents$MediaMentions)
summary(incumbents[incumbents$CabinetNow==1,]$MediaMentions)

#### Mediation Analysis ####
mediaPredictorNaive = lm(LogMM ~ CabinetNow + TermsServed + PartyInGovt + Gender + PastLawJob + LastVotePct + Age + as.factor(Party), 
                         data=incumbents[incumbents$MediaMentions>0,])

lmModelCAMM = lm(VotePct ~ CabinetNow + TermsServed + PartyInGovt + as.factor(Party) + LogMM,
                 data=incumbents[incumbents$MediaMentions>0,])

model.mediation.out = mediate(mediaPredictorNaive, lmModelCAMM,
                           treat="CabinetNow", mediator="LogMM", 
                           robustSE=TRUE, sims=1000)

# 2.56 mediated; -0.84 direct; 1.73 total effect (total mediation)
summary(model.mediation.out)

# Direct OLS of media mentions; check that this induces a similar pattern.
model.ols.mediamention = lm(VotePct ~ CabinetNow + TermsServed + PartyInGovt + as.factor(Party) + LogMM, data=incumbents[incumbents$MediaMentions>0, ])

#### Combined Effect Plot ####
effects = c(model.OLS.CS$coefficients[2],
            model.OLS.CS.PY$coefficients[2],
            model.OLS.CS.MemberDemo$coefficients[2],
            model.OLS.Control$coefficients[2],
            model.OLS.withinMember$coefficients[2],
            model.heckman.CS$estimate[21],
            model.heckman.CS.Control$estimate[21],
            model.heckman.withinMember$estimate[21],
            model.match.out.all$est,
            model.match.out.age$est,
            model.weight.out$coefficients[2],
            model.mediation.out$d0)

# This will take a long time because of the within-member fixed effects.
CI = rbind(
        c(coeftest(model.OLS.CS, vcov=sandwich)[2,1] - 1.96*coeftest(model.OLS.CS, vcov=sandwich)[2,2],
          coeftest(model.OLS.CS, vcov=sandwich)[2,1] + 1.96*coeftest(model.OLS.CS, vcov=sandwich)[2,2]),
        c(coeftest(model.OLS.CS.PY, vcov=sandwich)[2,1] - 1.96*coeftest(model.OLS.CS.PY, vcov=sandwich)[2,2],
          coeftest(model.OLS.CS.PY, vcov=sandwich)[2,1] + 1.96*coeftest(model.OLS.CS.PY, vcov=sandwich)[2,2]),
        c(coeftest(model.OLS.CS.MemberDemo, vcov=sandwich)[2,1] - 1.96*coeftest(model.OLS.CS.MemberDemo, vcov=sandwich)[2,2],
          coeftest(model.OLS.CS.MemberDemo, vcov=sandwich)[2,1] + 1.96*coeftest(model.OLS.CS.MemberDemo, vcov=sandwich)[2,2]),
        c(coeftest(model.OLS.Control, vcov=sandwich)[2,1] - 1.96*coeftest(model.OLS.Control, vcov=sandwich)[2,2],
          coeftest(model.OLS.Control, vcov=sandwich)[2,1] + 1.96*coeftest(model.OLS.Control, vcov=sandwich)[2,2]),
        c(coeftest(model.OLS.withinMember, vcov=sandwich)[2,1] - 1.96*coeftest(model.OLS.withinMember, vcov=sandwich)[2,2],
          coeftest(model.OLS.withinMember, vcov=sandwich)[2,1] + 1.96*coeftest(model.OLS.withinMember, vcov=sandwich)[2,2]),
        c(model.heckman.CS$estimate[21] - 1.96*sqrt(diag(solve(-model.heckman.CS$hessian))[21]),
          model.heckman.CS$estimate[21] + 1.96*sqrt(diag(solve(-model.heckman.CS$hessian))[21])),
        c(model.heckman.CS.Control$estimate[21] - 1.96*sqrt(diag(solve(-model.heckman.CS.Control$hessian))[21]),
          model.heckman.CS.Control$estimate[21] + 1.96*sqrt(diag(solve(-model.heckman.CS.Control$hessian))[21])),
        c(model.heckman.withinMember$estimate[21] - 1.96*sqrt(diag(solve(-model.heckman.withinMember$hessian))[21]),
          model.heckman.withinMember$estimate[21] + 1.96*sqrt(diag(solve(-model.heckman.withinMember$hessian))[21])),
        c(model.match.out.all$est - 1.96*model.match.out.all$se,
          model.match.out.all$est + 1.96*model.match.out.all$se),
        c(model.match.out.age$est - 1.96*model.match.out.age$se,
          model.match.out.age$est + 1.96*model.match.out.age$se),
        c(coeftest(model.weight.out)[2,1] - 1.96*coeftest(model.weight.out)[2,2],
          coeftest(model.weight.out)[2,1] + 1.96*coeftest(model.weight.out)[2,2]),
        c(model.mediation.out$d0.ci[1], model.mediation.out$d0.ci[2])
        )

labels = c("Party\nFixed Effects", "Party-Year\nFixed Effects", "Control for Member\nDemographics", "Control for\nPast Vote Share", "Within-Member",
           "Heckman Corrected\nParty F.E.", "Heckman Corrected\nControl Vote Share", "Heckman Corrected\nWithin-Member",
           "SOO: Matching", "SOO: Matching\nw/ Age", "SOO: Weighting",
           "Mediation Estimate:\nMedia Mentions")


parallelEffectPlot = function(effect, CI, labels, ...)
{
    # How many models are we plotting?
    num = length(effect)
    
    # x lim estimates
    domain = c(min(floor(apply(CI, 1, min))),
               max(ceiling(apply(CI, 1, max))))
    
    par(mar=c(5.1, 6.6, 5.5, 2.1))
    par(oma=c(0,0,0,0))
    
    # Point effect plots
    plot(effect, seq(num,1), 
         type="p", xlab="Estimate (Vote %)",
         main="Estimate of CabinetNow Coefficient",
         ylab="",
         xlim=c(domain[1], domain[2]),
         ylim=c(1, num),
         yaxt="n", xaxt="n",
         col=ifelse(CI[,1]<0,"gray50","black"),
         ...)
    
    
    # Now plot CIs and gridlines
    for(i in domain[1]:domain[2])
    {
        abline(v=i, col="gray95")
    }
    for(i in 1:num)
    {
        abline(h=num-i+1, col="gray95")
        lines(c(CI[i,1], CI[i,2]), c(num-i+1, num-i+1), col=ifelse(CI[i,1]<0,"gray50","black"))
        mtext(labels[i], side=2, at=num-i+1, las=1, line=1, cex=0.7)
    }
    
    # Zero effect model
    abline(v=0)
    
    # Axis
    mtext("Model", side=2, font=2, at=num+0.75, las=1, line=1, cex=0.7)
    axis(3, seq(domain[1], domain[2]), tick=TRUE, cex.axis=0.8)
    axis(1, seq(domain[1], domain[2]), tick=TRUE, cex.axis=0.8)
    #axis(2, seq(1, num), labels=labels, tick=TRUE, cex.axis=0.8)
}
pdf("Includes/estimatePlot.pdf")
parallelEffectPlot(effects, CI, labels)
dev.off()


#### Tenure Histogram ####
pdf("Includes/histogramTenureCA.pdf")
frequency_tenure = incumbents %>% group_by(ID) %>% summarise(TermsServed=max(TermsServed)) %>% dplyr::select(TermsServed)
hist(frequency_tenure$TermsServed,
     xlab="Terms Served", yaxt="n", ylab="", main="Histogram of Incumbent Tenure (Canada)")
dev.off()

#### Tables ####
stargazer(model.OLS.CS, model.OLS.Control, model.OLS.withinMember,
          se=list(coeftest(model.OLS.CS, sandwich)[,2],
                  coeftest(model.OLS.Control, sandwich)[,2],
                  coeftest(model.OLS.withinMember, sandwich)[,2]),
          type="latex",
          title="OLS Models",
          ci=TRUE,
          column.separate=c(1,1), 
          notes=c("OLS CIs from Heteroskedasticity-robust standard error estimates"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.", "Constant", "*factor*"), 
          covariate.labels=c("Cabinet Membership", "Party in Government", "Terms Served", "Past Vote \\%"),
          add.lines = list(c("Fixed Effects", "Party", "Party", "Member ID")),
          no.space=TRUE, digits=2, 
          omit.stat=c("f","rsq","res.dev","ser", "ll", "aic"),
          table.placement = "!htb",
          out="Includes/table1.tex")


stargazer(model.heckman.CS, model.heckman.CS.Control, model.heckman.withinMember,
          se=list(coeftest(model.heckman.CS, sandwich)[,2],
                  coeftest(model.heckman.CS.Control, sandwich)[,2],
                  coeftest(model.heckman.withinMember, sandwich)[,2]),
          type="latex",
          title="Heckman Corrected Models",
          ci=TRUE,
          column.separate=c(1,1), 
          notes=c("Outcome stage of Heckman models reported", "OLS CIs from Heteroskedasticity-robust standard error estimates"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.", "Constant", "*factor*"), 
          covariate.labels=c("Cabinet Membership", "Party in Government", "Terms Served", "Past Vote \\%"),
          add.lines = list(c("Fixed Effects", "Party", "Party", "Within-Member")),
          no.space=TRUE, digits=2, 
          #omit.stat=c("f","rsq","res.dev","ser", "ll", "aic"),
          table.placement = "!htb",
          out="Includes/table2.tex")


stargazer(model.weight.out,
          type="latex",
          title="Weighted Least Squares",
          ci=TRUE,
          column.separate=c(1,1), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.", "Constant", "*factor*"), 
          covariate.labels=c("Cabinet Membership", "Party in Government", "Terms Served", "Past Vote \\%"),
          add.lines = list(c("Fixed Effects", "Party")),
          no.space=TRUE, digits=2, 
          omit.stat=c("f","rsq","res.dev","ser", "ll", "aic"),
          table.placement = "!htb",
          out="Includes/table3.tex")

stargazer(model.logit.base, model.logit.cond,
          type="latex",
          title="Logistic Regressions",
          ci=TRUE,
          column.separate=c(1,1), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.", "Constant", "*factor*"), 
          covariate.labels=c("Cabinet Membership", "Party in Government", "Terms Served", "Past Vote \\%"),
          add.lines = list(c("Fixed Effects", "Party", "Party"), c("Longitudinal FEs?", "No", "Yes")),
          no.space=TRUE, digits=2, 
          omit.stat=c("f","rsq","res.dev","ser", "ll", "aic"),
          table.placement = "!htb",
          out="Includes/appendix_logit.tex")

