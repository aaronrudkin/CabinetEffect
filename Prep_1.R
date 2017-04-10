rm(list=ls())
setwd("~/Dropbox/Field Paper Revisions/")
library(dplyr)

############## PREP DATA #############
# Read the election data
dataT = read.csv("Canada Data/Merge/output.csv", header=TRUE)

# Discard data before 1945 general election
# Cabinet members forced to resign up through 1931, and then the
# unusual unity government / party realignment occurring in WW2
dataT = dataT[as.character(dataT$Date)>="1945-06-11",]

# Recoding a few row party names which might not be properly coded
dataT[dataT$Party=="Cons.", ]$Party = "C"
dataT[dataT$Party=="N/A", ]$Party = "Ind."

# Code party in government
dataT$PartyInGovt = 0
dataT[as.character(dataT$Date)>="1935-01-01" & as.character(dataT$Date)<="1948-11-14" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1948-11-15" & as.character(dataT$Date)<="1957-06-20" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1957-06-21" & as.character(dataT$Date)<="1963-04-21" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1963-04-22" & as.character(dataT$Date)<="1968-04-19" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1968-04-20" & as.character(dataT$Date)<="1979-06-02" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1979-06-02" & as.character(dataT$Date)<="1980-03-01" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1980-03-02" & as.character(dataT$Date)<="1984-09-15" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1984-09-17" & as.character(dataT$Date)<="1993-11-02" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1993-11-03" & as.character(dataT$Date)<="2006-02-05" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="2006-02-06" & dataT$Party=="C",]$PartyInGovt = 1

# Recode party labels to only include major parties and other; 
# no substantive reason to keep variation in that regard.
dataT[dataT$Party!="Lib" & dataT$Party!="P.C." & dataT$Party!="C" & 
          dataT$Party!="CA" & dataT$Party!="N.D.P." & dataT$Party!="Ref." & 
          dataT$Party!="CCF" & dataT$Party!="B.Q.",]$Party = "Ind."
dataT$Party = relevel(dataT$Party, "Ind.")

# Discard ridings where there was no competition (uncontested) -- rare but exists
# Future work should maybe consider modelling this, since presumably this is the 
# result of an incumbent being able to deter competition
dataT = dataT[dataT$Votes>-1,]
dataT$Stub = 0

incumbents = dataT[dataT$Incumbent==1, ]

# Add stub observations for Heckman / early exit: 
ghostCandidates = incumbents %>% group_by(ID) %>% 
    summarise(name=last(Name), exitDataSet=last(Elected), lastElection=last(Date), termsServed=last(TermsServed), finalVotePct=last(VotePct),Gender=last(Gender),Province=last(Province),Party=last(Party),PastLawJob=max(PastLawJob)) %>% 
    mutate(needGhostRow = as.integer(as.character(lastElection)!="2015-10-19") * as.integer(exitDataSet)) %>%
    filter(needGhostRow==1)

write.csv(ghostCandidates,"Canada Data/SyntheticObs/synthIn.csv", row.names=FALSE)
save(dataT, file="Canada Data/merge/prep_data_stage_1.RData")


