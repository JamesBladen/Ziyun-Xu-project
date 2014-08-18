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

whatIsGoingOn <- vector()
for(l in which(someMoreTests==0)){
  whatIsGoingOn <- append(whatIsGoingOn,finalFromUniversity[l])
}

#these are all stuff that did not perfectly match, so lets instead just try using something that looks for approximate matches



whatIsGoingOn <- vector()
for(l in which(someMoreTests==0)){
  whatIsGoingOn <- append(whatIsGoingOn,agrep(finalFromUniversity[l],universityNames)[1])
}



stuff <- which(someMoreTests==0)
checkTheseThings <- stuff[which(is.na(whatIsGoingOn))]
finalFromUniversity[checkTheseThings]
#so i guess we are left just manually fixing these names since they were repeated 
finalFromUniversity[checkTheseThings][1:5] <- "Anhui University of Technology"
finalFromUniversity[checkTheseThings][6:7] <- "Yanshan University" 
finalFromUniversity[checkTheseThings][8:9] <- "Jiangsu University"
finalFromUniversity[checkTheseThings][10] <- "Hebei United University"
finalFromUniversity[checkTheseThings][12:14] <- "Handan College"
finalFromUniversity[checkTheseThings][15:17] <- "Zhejiang Guangxia College of Applied Construction Technology"
finalFromUniversity[checkTheseThings][18] <- "Ankang University"
finalFromUniversity[checkTheseThings][19] <- "Guangdong University of Foreign Studies"
finalFromUniversity[checkTheseThings][21:22] <- "Hangzhou Dianzi University"
finalFromUniversity[checkTheseThings][25] <- "Fudan University"
finalFromUniversity[checkTheseThings][26:29] <- "Hangzhou Dianzi University"
finalFromUniversity[checkTheseThings][30:33] <- "Beijing Forestry University"



#running it again gives us a few that we are just going to have to input manually when we get there:
#[1] "Hunan University of Humanities Science and Technology"(Hunan) "the fourth military medical university" (Shaanxi)               
#[3] "the fourth military medical university"(Shaanxi)             "the fourth military medical university" (Shaanxi)              
#[5] "University of Leeds"(UK)                                      "University of Leeds"  (UK)                                
#[7] "Chinese University of Hong Kong" (hong kong)








##########################################################
#so now that we've run some tests on the names, we can actually try to produce the regions based on the affiliation

Location <- vector()
for (k in 1:length(finalFromUniversity)){
  Location <- append(Location,universityNames[agrep(finalFromUniversity[k],universityNames)[1]])
}



agrep(finalFromUniversity[4971],universityNames)[1]
universityNames[agrep(finalFromUniversity[4971],universityNames)[1]]



