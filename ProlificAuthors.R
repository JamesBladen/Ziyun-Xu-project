combined_pubs <- read.csv("C:/Users/User Files/Desktop/GitHub/Ziyun Xu project/combined_pubs.csv")

authorNames <- combined_pubs[,5]
years <- combined_pubs[,17]








allAuthors <- vector()
allYears <- vector()

for(i in 1:length(authorNames)){
  name <-  as.character(authorNames[i])
  if(regexpr("(;|,|/)",name)[1]!=-1){
    splittingRows <- strsplit(name,split='(;|/|,)')
    for (j in 1:length(splittingRows[[1]])){
      allAuthors <- append(allAuthors,splittingRows[[1]][j])
      allYears <- append(allYears,years[i]) 
    } 
  }
  else{
    allAuthors <- append(allAuthors,as.character(authorNames[i]))
    allYears <- append(allYears,years[i])
  }
}

































