library(dplyr)
setwd("~/Dropbox/Field Paper Revisions/Canada Data/OpenParliament/openparl data")

speaking = read.csv("speaking_amount.csv")
parties = read.csv("party.csv")
politician = read.csv("politician.csv")
bills = read.csv("bills.csv")
elected_members = read.csv("electedmember.csv")
ridings = read.csv("riding.csv")

# Summary stats on legislative activity
bill_summary = bills %>% group_by(parliament_number, sponsor_politician_id, passed) %>% summarise(num_bills=n())
bill_summary_passed = bill_summary[bill_summary$passed==1, ]
bill_summary_failed = bill_summary[bill_summary$passed==0, ]


speaking_summary = speaking %>% left_join(politician, by=c("pid" = "id")) %>% 
    #select(sum, parliament_number, pid, mid, name, dob, gender) %>% 
    left_join(elected_members, by=c("mid" = "id")) %>% 
    left_join(ridings, by=c("riding_id" = "id")) %>% 
    left_join(parties, by=c("party_id" = "id")) %>%
    left_join(bill_summary_passed, by=c("pid" = "sponsor_politician_id", "parliament_number" = "parliament_number")) %>%
    rename(num_bills_passed = num_bills) %>%
    left_join(bill_summary_failed, by=c("pid" = "sponsor_politician_id", "parliament_number" = "parliament_number")) %>%
    rename(num_bills_failed = num_bills) %>%
    rename(speaking_words = sum) %>%
    rename(party_name = name) %>%
    rename(riding_name = name.y) %>%
    rename(name = name.x) %>%
    select(pid, parliament_number, name, dob, gender, start_date, end_date, riding_name, province, party_name, speaking_words, num_bills_passed, num_bills_failed) %>%
    filter(!is.na(pid))

speaking_summary[is.na(speaking_summary$num_bills_failed),]$num_bills_failed = 0
speaking_summary[is.na(speaking_summary$num_bills_passed),]$num_bills_passed = 0

x = table(speaking_summary$party_name)
x[x>0]

speaking_summary$party_name = recode(speaking_summary$party_name, `Canadian Alliance` = "CA", `Conservative Party of Canada` = "C", 
                                               `Liberal Party of Canada` = "Lib", `Progressive Conservative` = "P.C.", 
                                               `New Democratic Party` = "N.D.P.", `Independent` = "Ind.", `Green Party of Canada` = "G.P.",
                                               `Reform Party of Canada` = "Ref.", `Bloc Québécois` = "B.Q.")

speaking_summary$election_number = speaking_summary$parliament_number + 1

write.csv(speaking_summary,"merged_speaking.csv")