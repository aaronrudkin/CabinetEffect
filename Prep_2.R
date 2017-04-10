rm(list=ls())
setwd("~/Dropbox/Field Paper Revisions/")
library(dplyr)

load("Canada Data/merge/prep_data_stage_1.RData")
ghostObs = read.csv("Canada Data/SyntheticObs/synthOut.csv")

split_to_binary = function(df, col)
{
    splits = levels(df[,col])
    for(i in 1:length(splits))
    {
        label = paste0("PROV_", i)
        df[,label] = 0
        df[df[,col] == splits[i], label] = 1
    }
    return(df)
}

# This is basically a re-implementation of model.matrix to convert a factor into dummies
dataT = split_to_binary(dataT, "Province")

# Merge in media mention data
media_mentions = read.csv("Canada Data/LexisNexis/l_n_output.csv")
dataT$Date = as.character(dataT$Date) # Coerce to make the join work
media_mentions$Date = as.character(media_mentions$Date)  # Coerce to make the join work
media_mentions = media_mentions[-media_mentions$MediaMentions < 0, ] # Drop NAs
dataT = dataT %>% left_join(media_mentions, by=c("ID" = "ID", "Date" = "Date")) %>% dplyr::select(-Name.y, -ROWID) # Do the join and drop extra fields
dataT$LogMM = log(dataT$MediaMentions)
rm(media_mentions)

# Create relevant data subsets
incumbents = dataT[dataT$Incumbent==1,]
dtAge = incumbents[incumbents$Age>0,]

save(dataT, incumbents, dtAge, ghostObs, file="Canada Data/merge/prep_data_final.RData")