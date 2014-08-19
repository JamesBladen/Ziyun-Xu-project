years <- read.table("C:/Users/User Files/Desktop/years.csv", quote="\"", stringsAsFactors=FALSE)
years <- years[[1]]


plot(table(years),type='h',lwd=15,col="blue", main="Chinese Doctoral Dissertations", xlab="Year", ylab="Amount")



hist(years, breaks=23,freq=T,xaxp=c(1995,2015,20), col='blue', main="Chinese Doctoral Dissertations", xlab="Year", ylab="Amount of Dissertations")


par(las=3)
foo <- hist(years,ylim=c(0,7),breaks=29,xaxt="n", xlim=c(1997,2013), col='blue', main="Chinese Doctoral Dissertations", xlab="Year", ylab="Amount of Dissertations")

foo$mids
axis(side=1,at=seq(1997,2013),labels=seq(1997,2013))









counts
counts <- table(years)
barplot(counts, xlim=c(1997,2013))

barplot(counts)

mp <- barplot(counts,axes=F)




count <- c(1,0,0,0,1,0,0,2,2,1,1,7,0,7,4,5,1)
mp <- barplot(count, col='blue', main="Chinese Doctoral Dissertations", xlab="Year", ylab="Number of Dissertations")
axis(1, at=mp, labels=seq(1997,2013))

