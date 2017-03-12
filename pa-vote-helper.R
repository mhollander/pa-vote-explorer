library(data.table)
library(reshape2)
library(rgdal)
library(leaflet)
library(dplyr)
library(BH)
library(rmapshaper)

# all data taken from here: https://github.com/openelections/openelections-data-pa
# I have no idea what half of these columns actually are for, but I'm not going to scrap them for now b/c they
# may prove useful
votes2016 <- read.csv("20161108__pa__general__precinct.csv", na.strings=c(""," ", "NA"), header=FALSE)
votes2014 <- read.csv("20141104__pa__general__precinct.csv", na.strings=c(""," ", "NA"), header=FALSE)

colnames = c("Year", "Type", "CountyCode", "Precinct", "OfficeRank", "CandidateDistrict", 
             "CandidatePartyRank", "CandidateBallotPosition", "CandidateOfficeCode", "CandidateParty",
             "CandidateNumber", "CandidateLastName", "CandidateFirstName", "CandidateMiddleName", 
             "CandidateSuffix", "VoteTotal", "USConDistrict", "StateSenatorialDistrict", "StateHouseDistrict",
             "MunicipalityTypeCode", "MunicipalityName", "MunicipalityBreakdownCode", "MunicipalityBreakdownName",
             "MunicipalityBreakdownCode2", "MunicipalityBreakdownName2",
             "Bi-County Code", "MCDCode", "FIPSCode", "VTDcode", "PreviousPrecinctCode", "PreviousUSConDistrict",
             "PreviousStateSenatorialDistrict", "PreviousStateHouseDistrict")

setnames(votes2016, colnames)
setnames(votes2014, colnames)


# this should fill all of the senatorial districts that weren't in the 2016 election; the !is.na removes write in candidates
# who show up in the results and with a party, even if they have no votes.
votes <- rbind(votes2016, votes2014[votes2014$CandidateOfficeCode=="STS" & !is.na(votes2014$CandidateFirstName),])

votes$StateSenatorialDistrict <- as.integer(votes$StateSenatorialDistrict)
votes$CandidateSuffix <- as.character(votes$CandidateSuffix)
votes$StateSenatorialDistrict <- as.integer(votes$StateSenatorialDistrict)
votes$StateHouseDistrict <- as.integer(votes$StateHouseDistrict)
votes$CandidateSuffix[is.na(votes$CandidateSuffix)] <- ""
votes$CandidateName <- paste(votes$CandidateFirstName, votes$CandidateLastName, votes$CandidateSuffix)

# I hate that I'm doing this, but I need to clean up the data.  There are a number of people who run as D/R or R/D when they
# are really from a specific party.  I'm hardcoding their party in here.
votes$CandidateParty[votes$CandidateName == "JUDITH SCHWANK "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "LISA  BOSCOLA "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "MARK LONGIETTI "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "CHRISTOPHER SAINATO "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "ANITA ASTORINO KULIK "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "H CONKLIN "] <- "DEM"
votes$CandidateParty[votes$CandidateName == "KIM WARD "] <- "REP"
votes$CandidateParty[votes$CandidateName == "JAMES MARSHALL "] <- "REP"
votes$CandidateParty[votes$CandidateName == "HAROLD ENGLISH "] <- "REP"
votes$CandidateParty[votes$CandidateName == "MATTHEW BAKER "] <- "REP"
votes$CandidateParty[votes$CandidateName == "CARL METZGAR "] <- "REP"
votes$CandidateParty[votes$CandidateName == "RONALD MARSICO "] <- "REP"
votes$CandidateParty[votes$CandidateName == "KAREN BOBACK "] <- "REP"
votes$CandidateParty[votes$CandidateName == "JEROME KNOWLES "] <- "REP"
votes$CandidateParty[votes$CandidateName == "GARY DAY "] <- "REP"


# aggregate vote totals for Rs, Ds, and total votes and make a nice table
senateDs <- aggregate(VoteTotal ~ StateSenatorialDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STS" & votes$CandidateParty=="DEM",], FUN="sum")
senateRs <- aggregate(VoteTotal ~ StateSenatorialDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STS" & votes$CandidateParty=="REP",], FUN="sum")
#senateDRs <- aggregate(VoteTotal ~ StateSenatorialDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STS" & votes$CandidateParty %in% c("D/R","R/D"),], FUN="sum")
senateTotalVotes <- aggregate(VoteTotal ~ StateSenatorialDistrict, data=votes[votes$CandidateOfficeCode=="STS",], FUN="sum")
senateVotes <- merge(senateDs,senateRs, by="StateSenatorialDistrict", all=TRUE)
senateVotes <- merge(senateVotes, senateTotalVotes, by="StateSenatorialDistrict", all=TRUE)

# now aggregate the presidential votes for Rs, Ds, and Total votes and combine
senatePresD <- aggregate(VoteTotal ~ StateSenatorialDistrict, data=votes[votes$CandidateOfficeCode=="USP" & votes$CandidateParty=="DEM",], FUN="sum")
senatePresR <- aggregate(VoteTotal ~ StateSenatorialDistrict, data=votes[votes$CandidateOfficeCode=="USP" & votes$CandidateParty=="REP",], FUN="sum")
senatePresTotalVotes <- aggregate(VoteTotal ~ StateSenatorialDistrict, data=votes[votes$CandidateOfficeCode=="USP",], FUN="sum")
senateVotes <- merge(senateVotes,senatePresD, by="StateSenatorialDistrict", all=TRUE)
senateVotes <- merge(senateVotes,senatePresR, by="StateSenatorialDistrict", all=TRUE)
senateVotes <- merge(senateVotes, senatePresTotalVotes, by="StateSenatorialDistrict", all=TRUE)

setnames(senateVotes, c("StateSenatorialDistrict", "DCandidate","DVotes","RCandidate","RVotes", "TotalSenatorVotes", "PresDVotes", "PresRVotes","TotalPresVotes"))
setnames(senateDRs, c("StateSenatorialDistrict", "DCandidate","DVotes"))
#senateDRs <- senateDRs[with(senateDRs, order(StateSenatorialDistrict)),]

#senateVotes$DCandidate[senateVotes$StateSenatorialDistrict %in% senateDRs$StateSenatorialDistrict] <- senateDRs$DCandidate
#senateVotes$RCandidate[senateVotes$StateSenatorialDistrict %in% senateDRs$StateSenatorialDistrict] <- senateDRs$DCandidate
#senateVotes$DVotes[senateVotes$StateSenatorialDistrict %in% senateDRs$StateSenatorialDistrict] <- senateDRs$DVotes
#senateVotes$RVotes[senateVotes$StateSenatorialDistrict %in% senateDRs$StateSenatorialDistrict] <- senateDRs$DVotes


# now do the same, but for house districts
# aggregate vote totals for Rs, Ds, and total votes and make a nice table
houseDs <- aggregate(VoteTotal ~ StateHouseDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STH" & votes$CandidateParty=="DEM",], FUN="sum")
houseRs <- aggregate(VoteTotal ~ StateHouseDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STH" & votes$CandidateParty=="REP",], FUN="sum")
#houseDRs <- aggregate(VoteTotal ~ StateHouseDistrict + CandidateName, data=votes[votes$CandidateOfficeCode=="STH" & votes$CandidateParty %in% c("D/R","R/D"),], FUN="sum")
houseTotalVotes <- aggregate(VoteTotal ~ StateHouseDistrict, data=votes[votes$CandidateOfficeCode=="STH",], FUN="sum")
houseVotes <- merge(houseDs,houseRs, by="StateHouseDistrict", all=TRUE)
houseVotes <- merge(houseVotes, houseTotalVotes, by="StateHouseDistrict", all=TRUE)

# now aggregate the presidential votes for Rs, Ds, and Total votes and combine
housePresD <- aggregate(VoteTotal ~ StateHouseDistrict, data=votes[votes$CandidateOfficeCode=="USP" & votes$CandidateParty=="DEM",], FUN="sum")
housePresR <- aggregate(VoteTotal ~ StateHouseDistrict, data=votes[votes$CandidateOfficeCode=="USP" & votes$CandidateParty=="REP",], FUN="sum")
housePresTotalVotes <- aggregate(VoteTotal ~ StateHouseDistrict, data=votes[votes$CandidateOfficeCode=="USP",], FUN="sum")
houseVotes <- merge(houseVotes,housePresD, by="StateHouseDistrict", all=TRUE)
houseVotes <- merge(houseVotes,housePresR, by="StateHouseDistrict", all=TRUE)
houseVotes <- merge(houseVotes, housePresTotalVotes, by="StateHouseDistrict", all=TRUE)


setnames(houseVotes, c("StateHouseDistrict", "DCandidate","DVotes","RCandidate","RVotes", "TotalHouseVotes", "PresDVotes", "PresRVotes","TotalPresVotes"))
setnames(houseDRs, c("StateHouseDistrict", "DCandidate","DVotes"))
#houseDRs <- houseDRs[with(houseDRs, order(StateHouseDistrict)),]
#houseVotes$DCandidate[houseVotes$StateHouseDistrict %in% houseDRs$StateHouseDistrict] <- houseDRs$DCandidate
#houseVotes$RCandidate[houseVotes$StateHouseDistrict %in% houseDRs$StateHouseDistrict] <- houseDRs$DCandidate
#houseVotes$DVotes[houseVotes$StateHouseDistrict %in% houseDRs$StateHouseDistrict] <- houseDRs$DVotes
#houseVotes$RVotes[houseVotes$StateHouseDistrict %in% houseDRs$StateHouseDistrict] <- houseDRs$DVotes


# how to integrate d/r?

# get rid of NAs in the vote totals.  This can mess up later calcuations
houseVotes$DVotes[is.na(houseVotes$DVotes)] <- 0
houseVotes$RVotes[is.na(houseVotes$RVotes)] <- 0
senateVotes$DVotes[is.na(senateVotes$DVotes)] <- 0
senateVotes$RVotes[is.na(senateVotes$RVotes)] <- 0



# add some fields that do some helpful calculations
houseVotes$HouseVoteDPercent <- round(houseVotes$DVotes/houseVotes$TotalHouseVotes * 100,2)
houseVotes$HouseVoteRPercent <- round(houseVotes$RVotes/houseVotes$TotalHouseVotes * 100,2)
houseVotes$HouseVoteDlessR <- round(houseVotes$DVotes-houseVotes$RVotes,2)
houseVotes$HousePercentDiffDlessR <- round(houseVotes$HouseVoteDPercent - houseVotes$HouseVoteRPercent,2)
houseVotes$PresVoteDPercent <- round(houseVotes$PresDVotes/houseVotes$TotalPresVotes * 100,2)
houseVotes$PresVoteRPercent <- round(houseVotes$PresRVotes/houseVotes$TotalPresVotes * 100,2)
houseVotes$PresVotesDlessR <- round(houseVotes$PresDVotes - houseVotes$PresRVotes,2)
houseVotes$PresPercentDiffDlessR <- round(houseVotes$PresVoteDPercent - houseVotes$PresVoteRPercent,2)
houseVotes$PartyConsistent <-  ifelse(houseVotes$HouseVoteDlessR*houseVotes$PresVotesDlessR > 0, 1, -1) # this is 1 if consistent and -1 if the party for the house and pres are different

# this field calculates the absolute difference between the national win and local win for a party.  If the
# number is negative, it mesuares the size of the difference when there was a party switch.  if positive, 
# it measures the strength of the local vote vs national vote for a party
houseVotes$LocalNationalDifferential <- houseVotes$PartyConsistent * abs(houseVotes$HousePercentDiffDlessR - houseVotes$PresPercentDiffDlessR)
#force to 0 those places where there is a d/r, since we don't know the party
houseVotes$LocalNationalDifferential[houseVotes$DCandidate == houseVotes$RCandidate] <- 0

# this measure is 0 if the party of the local and national candidate are the same and is 1/margin of victory if you and the national
# candidate are from different parties.  This means that the districts most intensley colored are those where the local candidate
# won by a small margin and the national candidate was of the other party (meaning they are vulnerable)
houseVotes$vulnerableLowStateMargin <- ifelse(houseVotes$PartyConsistent > 0,0,abs(1/houseVotes$HousePercentDiffDlessR))
houseVotes$vulnerableHighPresMargin <- ifelse(houseVotes$PartyConsistent > 0,0,abs(houseVotes$PresPercentDiffDlessR) / 100)


senateVotes$SenateVoteDPercent <- round(senateVotes$DVotes/senateVotes$TotalSenatorVotes * 100,2)
senateVotes$SenateVoteRPercent <- round(senateVotes$RVotes/senateVotes$TotalSenatorVotes * 100,2)
senateVotes$SenateVoteDlessR <- round(senateVotes$DVotes-senateVotes$RVotes,2)
senateVotes$SenatePercentDiffDlessR <- round(senateVotes$SenateVoteDPercent - senateVotes$SenateVoteRPercent,2) 
senateVotes$PresVoteDPercent <- round(senateVotes$PresDVotes/senateVotes$TotalPresVotes * 100,2)
senateVotes$PresVoteRPercent <- round(senateVotes$PresRVotes/senateVotes$TotalPresVotes * 100,2)
senateVotes$PresVotesDlessR <- round(senateVotes$PresDVotes - senateVotes$PresRVotes,2)
senateVotes$PresPercentDiffDlessR <- round(senateVotes$PresVoteDPercent - senateVotes$PresVoteRPercent,2)

# this is 1 if the same party won in the state and pres votes and -1 if the parties are different; 
# I divide by 1 to avoid integer overflow errors
senateVotes$PartyConsistent <-  ifelse((1/senateVotes$SenateVoteDlessR)*(1/senateVotes$PresVotesDlessR) > 0, 1, -1) 

# this field calculates the absolute difference between the national win and local win for a party.  If the
# number is negative, it mesuares the size of the difference when there was a party switch.  if positive, 
# it measures the strength of the local vote vs national vote for a party
senateVotes$LocalNationalDifferential <- senateVotes$PartyConsistent * abs(senateVotes$SenatePercentDiffDlessR - senateVotes$PresPercentDiffDlessR)

#force to 0 those places where there is a d/r, since we don't know the party
senateVotes$LocalNationalDifferential[senateVotes$DCandidate == senateVotes$RCandidate] <- 0

# this measure is 0 if the party of the local and national candidate are the same and is 1/margin of victory if you and the national
# candidate are from different parties.  This means that the districts most intensley colored are those where the local candidate
# won by a small margin and the national candidate was of the other party (meaning they are vulnerable)
senateVotes$vulnerableLowStateMargin <- ifelse(senateVotes$PartyConsistent > 0,0,abs(1/senateVotes$SenatePercentDiffDlessR))
senateVotes$vulnerableHighPresMargin <- ifelse(senateVotes$PartyConsistent > 0,0,abs(senateVotes$PresPercentDiffDlessR) / 100)

# map it!
tmp <- tempdir()
unzip("PAHouseShape2012.zip", exdir = tmp)
paHouse <- readOGR(dsn=tmp, layer="House2012Final")
paHouse <- rmapshaper::ms_simplify(paHouse)
unzip("PASenateShape2012.zip", exdir = tmp)
paSenate <- readOGR(dsn=tmp, layer="FinalSenatePlan2012")
paSenate <- rmapshaper::ms_simplify(paSenate)


# use a left join b/c some districts have no data
paHouse@data = paHouse@data %>%
  left_join(houseVotes, by=c("District_1"="StateHouseDistrict"), copy=TRUE)
paSenate@data = paSenate@data %>%
  left_join(senateVotes, by=c("District_1"="StateSenatorialDistrict"), copy=TRUE)

paHouse@data$District_1

house_popup <- paste0("<strong>District: ", 
                      paHouse$District_1, 
                      "</strong><br>", 
                      paHouse$DCandidate," (D): ", paHouse$HouseVoteDPercent, "% <br>",
                      paHouse$RCandidate," (R): ", paHouse$HouseVoteRPercent, "% <br>",
                      "<strong>Margin of Victory: ", abs(paHouse$HousePercentDiffDlessR), "%</strong>",
                      "<br>CLINTON:",paHouse$PresVoteDPercent,"%",
                      "<br>TRUMP:",paHouse$PresVoteRPercent,"%"
)



senate_popup <- paste0("<strong>District: ", 
                      paSenate$District_1, 
                      "</strong><br>", 
                      paSenate$DCandidate," (D): ", paSenate$SenateVoteDPercent, "% <br>",
                      paSenate$RCandidate," (R): ", paSenate$SenateVoteRPercent, "% <br>",
                      "<strong>Margin of Victory: ", abs(paSenate$SenatePercentDiffDlessR), "%</strong>",
                      "<br>CLINTON:",paSenate$PresVoteDPercent,"%",
                      "<br>TRUMP:",paSenate$PresVoteRPercent,"%"
)


pal <- colorNumeric("YlOrBr",domain=0:1)

# ## HOUSE
# # print out the map.  The problem here is that the palettes are white->red as you go from bad->good, which
# # means that we often will want to resort everything 
paHouseMap <- leaflet(data = paHouse) %>%
   addProviderTiles("CartoDB.Positron") %>%
   addPolygons(fillColor = ~pal(vulnerableLowStateMargin), 
               fillOpacity = 0.8, 
               color = ifelse(paHouse$HouseVoteDlessR>0,"#0000A0","#8B0000"),
               weight = 1, 
               popup = house_popup)
  #              group = "vulnerableLowStateMargin") %>%
  # addPolygons(fillColor = ~pal(vulnerableHighPresMargin), 
  #             fillOpacity = 0.8, 
  #             color = "#BDBDC3", 
  #             weight = 1, 
  #             popup = house_popup,
  #             group = "vulnerableHighPresMargin") %>%
  # hideGroup("vulnerableHighPresMargin")
  
 
### SENATE
# # print out the map.  The problem here is that the palettes are white->red as you go from bad->good, which
# # means that we often will want to resort everything 
paSenateMap <- leaflet(data = paSenate) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(vulnerableLowStateMargin), 
              fillOpacity = 0.8, 
              color = ifelse(paSenate$SenateVoteDlessR>0,"#0000A0","#8B0000"), 
              weight = 1, 
              popup = senate_popup)

  #             group = "vulnerableLowStateMargin") %>%
  # addPolygons(fillColor = ~pal(vulnerableHighPresMargin), 
  #             fillOpacity = 0.8, 
  #             color = "#BDBDC3", 
  #             weight = 1, 
  #             popup = house_popup,
  #             group = "vulnerableHighPresMargin") %>%
  # hideGroup("vulnerableHighPresMargin")
