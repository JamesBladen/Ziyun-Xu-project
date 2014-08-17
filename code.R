maindoc <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/combined_pubs.csv")

#i manually 
cleaned.up.affiliations <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/cleaned up affiliations.csv")
#noticed some of the names started with a space randomly, so cleaned that up so that i could match more easily with other document
cleaned.up.affiliations[,2] <- gsub("^( )", "",cleaned.up.affiliations[,2])




allID <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/all_ids (updated).csv", header=FALSE)


UniversityLocations <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/Universities_06-12-14.csv", header=FALSE)


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



anothercheck <- cbind(finalFromID,finalFromUniversity)
anothercheck[6000:6100,]



regexpr("(;|,|/)",affiliation)[1]


if(grep("(;|,|/)",affiliation)==1){
  print("hello");
}










myMat <- matrix(nrow=6364,ncol=5)
colnames(myMat) <- c("FromRegion","FromUniversity","YearOfPublication","ToRegion","ToUniversity")



for(i in 1:nrow(myMat)){
  myMat[i,2] <- as.character(maindoc[correctID[i,1]==maindoc[,1],2])
}



nrow(myMat)


myMat[4200:4300,2]

myMat[4397,2]



test <- as.character(maindoc[correctID[4397,1]==maindoc[,1],2])








for(i in 1:nrow(myMat)){
  affiliations <- as.character(maindoc[correctID[i,1]==maindoc[,1],2])
  
}





regexpr('+;', test)

strsplit(test,split='(;|/)')


grep('(;|/|,)',test)==1




##ideas:
#








maindoc[1000:2000,2]


nchar(as.character(maindoc[398,2]))



test <- "this is a test/ stuff; stuff, "
test2 <- strsplit(test,split='(;|,|/)')


















