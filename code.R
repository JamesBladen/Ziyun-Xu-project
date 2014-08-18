maindoc <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/combined_pubs.csv")

#i manually got rid of rows without an affiliation, or with corrupted affiliation
#also changed some of the random things into apostrophe in the files
cleaned.up.affiliations <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/cleaned up affiliations.csv")
#noticed some of the names started with a space randomly, so cleaned that up so that i could match more easily with other document
cleaned.up.affiliations[,2] <- gsub("^( )", "",cleaned.up.affiliations[,2])




allID <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/all_ids (updated).csv", header=FALSE)


UniversityLocations <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/Universities_06-12-14.csv", header=FALSE)
UniversityLocations <- UniversityLocations[-13,]

CitationID <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/combined_citation_ids (updated).csv")


justTheID <- CitationID[,4:5]



#first lets get rid of all the stuff that isn't CIS papers
correctID <- justTheID[((justTheID[,1]<4229) + (justTheID[,2]<4229) ==2),]



#now lets remove all the papers that i found to be corrupted in the excel file
check <- vector()
for(i in 1:nrow(correctID)){
  check[i] <- sum(correctID[i,1]==cleaned.up.affiliations[,1])
}
check <- check==1


correctID <- correctID[check,]



check2 <- vector()
for(i in 1:nrow(correctID)){
  check2[i] <- sum(correctID[i,2]==cleaned.up.affiliations[,1])
}
check2 <- check2==1


correctID <- correctID[check2,]





##############################################################################################################



#so now what i want to do is go through all of the first column of affiliations and break up the ones where there are 
#multiple in a single row
#so i record the affiliations with the ID associated with the person they are crediting. 

fromUniversity <- vector()
toID <- vector()
fromID <- vector()
for(i in 1:nrow(correctID)){
  affiliation <-  as.character(cleaned.up.affiliations[correctID[i,1]==cleaned.up.affiliations[,1],2])
  if(regexpr("(;|,|/)",affiliation)[1]!=-1){
    splittingRows <- strsplit(affiliation,split='(;|/|,)')
    
    for (j in 1:length(splittingRows[[1]])){
      fromUniversity <- append(fromUniversity,splittingRows[[1]][j])
      toID <- append(toID,correctID[i,2])
      fromID <- append(fromID,correctID[i,1])
    }
    
  }
  else{
    fromUniversity <- append(fromUniversity,affiliation)
    toID <- append(toID,correctID[i,2])
    fromID <- append(fromID,correctID[i,1])
  }
  
  
}



#so we have now enumerated all the from unversities to their own lines, so we have to do the same for the to universities now

#####################################################################################################
finalFromUniversity <- vector()
finalToUniversity <- vector()


finalFromID <- vector()


for(i in 1:length(toID)){
  affiliation <-  as.character(cleaned.up.affiliations[toID[i]==cleaned.up.affiliations[,1],2])
  if(regexpr("(;|,|/)",affiliation)[1]!=-1){
    splittingRows <- strsplit(affiliation,split='(;|/|,)')
    
    for (j in 1:length(splittingRows[[1]])){
      finalToUniversity <- append(finalToUniversity,splittingRows[[1]][j])
      finalFromUniversity <- append(finalFromUniversity,fromUniversity[i])
      finalFromID <- append(finalFromID,fromID[i])
    }
    
  }
  else{
    finalToUniversity <- append(finalToUniversity,affiliation)
    finalFromUniversity <- append(finalFromUniversity,fromUniversity[i])
    finalFromID <- append(finalFromID,fromID[i])
  }
  
}




finalYear <- vector()
for(i in 1:length(finalFromID)){
  finalYear <- append(finalYear,maindoc[which(finalFromID[i]==maindoc[,1]),17])
}


removeThisRow <- which(is.na(finalYear))

finalYear <- finalYear[-removeThisRow]
finalFromID <- finalFromID[-removeThisRow]
finalFromUniversity <- finalFromUniversity[-removeThisRow]
finalToUniversity <- finalToUniversity[-removeThisRow]






extrachecking <- cbind(finalFromID,finalFromUniversity,finalYear,finalToUniversity)
sum(is.na(extrachecking))
#says 0, so we have no NA values



universityNames <- as.character(UniversityLocations[,1])


##so here i want to see if the names from our university affiliations match with the university names in our other file
someMoreTests <- vector()
for (k in 1:length(finalFromUniversity)){
  someMoreTests <- append(someMoreTests,sum(finalFromUniversity[k]==universityNames))
}
#a coupple have 0 matches (so we have to fix things) but more interestingly there was a few with 2 matches
which(someMoreTests==2)

#lets see what is causing this
whatIsGoingOn <- vector()
for(l in which(someMoreTests==2)){
  whatIsGoingOn <- append(whatIsGoingOn,finalFromUniversity[l])
}
#so dalian university of technology is listed twice. lets just remove one of those rows (i'll do this at the top of the file)








#now lets do the same but for ones without a match to see which names are perhaps corrupted a bit
finalFromUniversity[which(someMoreTests==0)]

#some of these are probably errors in the string split that i used (just blank lines)
#103,104,105,164,165,182,183,184
finalFromUniversity[which(someMoreTests==0)[c(103,104,105,164,165,182,183,184)]]
#these rows were messed up, so lets remove them
which(someMoreTests==0)[c(94, 95, 96, 146, 147, 164, 165, 166)]
finalYear <- finalYear[-which(someMoreTests==0)[c(94, 95, 96, 146, 147, 164, 165, 166)]]
finalFromID <- finalFromID[-which(someMoreTests==0)[c(94, 95, 96, 146, 147, 164, 165, 166)]]
finalFromUniversity <- finalFromUniversity[-which(someMoreTests==0)[c(94, 95, 96, 146, 147, 164, 165, 166)]]
finalToUniversity <- finalToUniversity[-which(someMoreTests==0)[c(94, 95, 96, 146, 147, 164, 165, 166)]]
#these are all stuff that did not perfectly match, so lets instead just try using something that looks for approximate matches



whatIsGoingOn <- vector()
for(l in which(someMoreTests==0)){
  whatIsGoingOn <- append(whatIsGoingOn,agrep(finalFromUniversity[l],universityNames)[1])
}



stuff <- which(someMoreTests==0)
checkTheseThings <- stuff[which(is.na(whatIsGoingOn))]
finalFromUniversity[checkTheseThings]


#so i guess we are left just manually fixing these names since they were repeated 
finalFromUniversity[4282:4285] <- "Anhui University of Technology"









##########################################################
#so now that we've run some tests on the names, we can actually try to produce the regions based on the affiliation

Location <- vector()
for (k in 1:length(finalFromUniversity)){
  if(!is.na(agrep(finalFromUniversity[k],universityNames)[1])){
    Location <- append(Location,universityNames[agrep(finalFromUniversity[k],universityNames)[1]])
  }
  else{
    Location <- append(Location,finalFromUniversity[k])
  }
}


sneakyChecking <- vector()
for(u in 1:length(Location)){
  sneakyChecking <- append(sneakyChecking,sum(Location[u]==universityNames))
}

Location[which(sneakyChecking==0)]

Location[c(4282, 4283, 4284, 4285)] <- "Anhui University of Technology"
Location[c(4372, 4373)] <- "Jiangsu University"
Location[c(4655)] <- "Hebei United University"
Location[c(5005,5006,5007)] <- "Handan College"
Location[c(5179,5181,5183)] <- "Zhejiang Guangxia College of Applied Construction Technology"
Location[c(5262)] <- "Ankang University"
Location[c(6944,6946)] <- "Hangzhou Dianzi University"
Location[c(7313)] <- "Fudan University"
Location[c(7322, 7323, 7324, 7325)] <- "Hangzhou Dianzi University"
Location[c(7554, 7555, 7558, 7559)] <- "Beijing Forestry University"


#running it gives us a few that we are just going to have to input manually when we get there:
#[1] "Hunan University of Humanities Science and Technology"(Hunan) "the fourth military medical university" (Shaanxi)               
#[3] "the fourth military medical university"(Shaanxi)             "the fourth military medical university" (Shaanxi)              
#[5] "University of Leeds"(UK)                                      "University of Leeds"  (UK)                                
#[7] "Chinese University of Hong Kong" (hong kong)


















##so here i want to see if the names from our university affiliations match with the university names in our other file
someMoreTests <- vector()
for (k in 1:length(finalToUniversity)){
  someMoreTests <- append(someMoreTests,sum(finalToUniversity[k]==universityNames))
}


#now lets do the same but for ones without a match to see which names are perhaps corrupted a bit
finalToUniversity[which(someMoreTests==0)]

#some of these are probably errors in the string split that i used (just blank lines)
#23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454
finalToUniversity[which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
#these rows were messed up, so lets remove them
which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]



finalYear <- finalYear[-which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
finalFromID <- finalFromID[-which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
finalFromUniversity <- finalFromUniversity[-which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
finalToUniversity <- finalToUniversity[-which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
Location <- Location[-which(someMoreTests==0)[c(23,97,131,147,148,172,182,267,290,291,296,298,374,411,427,452,454)]]
#these are all stuff that did not perfectly match, so lets instead just try using something that looks for approximate matches

someMoreTests <- vector()
for (k in 1:length(finalToUniversity)){
  someMoreTests <- append(someMoreTests,sum(finalToUniversity[k]==universityNames))
}
finalToUniversity[which(someMoreTests==0)]


whatIsGoingOn <- vector()
for(l in which(someMoreTests==0)){
  whatIsGoingOn <- append(whatIsGoingOn,agrep(finalToUniversity[l],universityNames)[1])
}



stuff <- which(someMoreTests==0)
checkTheseThings <- stuff[which(is.na(whatIsGoingOn))]
finalToUniversity[checkTheseThings]

#okay so similar problem we had before (to be expected). lets just manually fix these few cases


finalToUniversity[1115] <- "Hubei University of Technology"
finalToUniversity[c(2331 ,3920, 6250, 7657, 7658, 7659)] <- "Fudan University"
finalToUniversity[c(3015, 7553)] <- "Luoyang Institute of Science and Technology"
finalToUniversity[3217] <- "Guiyang University"
finalToUniversity[3724] <- "Beijing Forestry University"
finalToUniversity[3864] <- "Guangxi University"
finalToUniversity[4359] <- "Northwest University for Nationalities"
finalToUniversity[c(4509, 4510)] <- "Jiangsu University"
finalToUniversity[c(4930, 5253, 6130, 7093, 7245, 7246)] <- "Guangdong University of Foreign Studies"
finalToUniversity[6466] <- "Hangzhou Dianzi University"











Location2 <- vector()
for (k in 1:length(finalToUniversity)){
  if(!is.na(agrep(finalToUniversity[k],universityNames)[1])){
    Location2 <- append(Location2,universityNames[agrep(finalToUniversity[k],universityNames)[1]])
  }
  else{
    Location2 <- append(Location2,finalToUniversity[k])
  }
}


sneakyChecking <- vector()
for(u in 1:length(Location2)){
  sneakyChecking <- append(sneakyChecking,sum(Location2[u]==universityNames))
}

Location2[which(sneakyChecking==0)]

#success! only the ones that we know won't match are not matching. good. we can fill those in manually at the end
















########################################################################################################################
#so now lets see what we already have
#we have the years
#we have the university affiliation for the from and the to
#so now we just need to match those up with the regions



Region <- vector()
for (k in 1:length(Location)){
  if (sum(Location[k]==universityNames)==1){
  Region <- append(Region, as.character(UniversityLocations[which(Location[k]==universityNames),3]))
  }
  else{
    Region <- append(Region,NA)
  }
}




Region2 <- vector()
for (k in 1:length(Location2)){
  if (sum(Location2[k]==universityNames)==1){
    Region2 <- append(Region2, as.character(UniversityLocations[which(Location2[k]==universityNames),3]))
  }
  else{
    Region2 <- append(Region2,NA)
  }
}



######################################################################################################
#now lets put them all together and manually add in those few universities that weren't in the doc


finalMatrix <- cbind(Region,Location,finalYear,Region2,Location2)
colnames(finalMatrix) <- c("From Region", "From University", "Year of Publication", "To Region", "To University")


fixThese <- which(is.na(finalMatrix[,1]))

finalMatrix[fixThese[1],1] <- "Hunan"
finalMatrix[fixThese[2],1] <- "Shaanxi"
finalMatrix[fixThese[3],1] <- "Shaanxi"
finalMatrix[fixThese[4],1] <- "Shaanxi"
finalMatrix[fixThese[5],1] <- "UK"
finalMatrix[fixThese[6],1] <- "UK"
finalMatrix[fixThese[7],1] <- "Hong Kong"




fixThese2 <- which(is.na(finalMatrix[,4]))
finalMatrix[fixThese2[1],4] <- "Hunan"
finalMatrix[fixThese2[2],4] <- "Hunan"
finalMatrix[fixThese2[3],4] <- "Hunan"
finalMatrix[fixThese2[4],4] <- "US"
finalMatrix[fixThese2[5],4] <- "UK"
finalMatrix[fixThese2[6],4] <- "US"
finalMatrix[fixThese2[7],4] <- "US"
finalMatrix[fixThese2[8],4] <- "UK"
finalMatrix[fixThese2[9],4] <- "US"
finalMatrix[fixThese2[10],4] <- "US"
finalMatrix[fixThese2[11],4] <- "US"
finalMatrix[fixThese2[12],4] <- "US"
finalMatrix[fixThese2[13],4] <- "US"







setwd('C:/Users/User Files/Desktop')
write.csv(finalMatrix, file = "Regional Citations.csv", row.names = F)














