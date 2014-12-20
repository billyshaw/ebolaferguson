# Homework 3
# Billy Susanto
# bs1585@nyu.edu

pause <- function ()
{
  cat(" ")
  cat("Pause. Press <Enter> to continue...")
  readline()
  invisible()
}

# Read all data
ebola <- read.table("Ebola_frequencies.csv", sep=",", row.names=1, header=TRUE)
gun <- read.table("IfTheyGunnedMeDown_frequencies.csv", sep=",", row.names=1, header=TRUE)
uswords <- read.table("USTop10Cities_frequencies.csv", sep=",", row.names=1, header=TRUE)

# Make data frame of all relative frequencies of each word 
ebola_freq <- as.data.frame( lapply(ebola,function(x) { x / 13088357 }) )
row.names(ebola_freq) <- row.names(ebola)

gun_freq <- as.data.frame(lapply(gun,function(x) { x / 948847  })  )
row.names(gun_freq) <- row.names(gun)

uswords_freq <- as.data.frame(lapply(uswords,function(x) { x / 16755061  })  )
row.names(uswords_freq) <- row.names(uswords)

# Most frequent words each week
mostfrequent <- function() {
  
  # Find 5 most frequent words in week 1
  uswords.week1 <- rownames(uswords_freq[order(uswords$X2014.10.31, decreasing=TRUE), ][0:5, ])
  ebola.week1 <- rownames(ebola_freq[order(ebola$X2014.10.06, decreasing=TRUE), ][0:5, ])
  gun.week1 <- rownames(gun_freq[order(gun$X2014.10.01, decreasing=TRUE), ][0:5, ])
  week1.matrix <- cbind(uswords.week1, ebola.week1, gun.week1)
  
  # Find 5 most frequent words in week 2
  uswords.week2 <- rownames(uswords_freq[order(uswords$X2014.11.07, decreasing=TRUE), ][0:5, ])
  ebola.week2 <- rownames(ebola_freq[order(ebola$X2014.10.13, decreasing=TRUE), ][0:5, ])
  gun.week2 <- rownames(gun_freq[order(gun$X2014.10.08, decreasing=TRUE), ][0:5, ])
  week2.matrix <- cbind(uswords.week2, ebola.week2, gun.week2)
  
  # Find 5 most frequent words in week 3
  uswords.week3 <- rownames(uswords_freq[order(uswords$X2014.11.14, decreasing=TRUE), ][0:5, ])
  ebola.week3 <- rownames(ebola_freq[order(ebola$X2014.10.20, decreasing=TRUE), ][0:5, ])
  gun.week3 <- rownames(gun_freq[order(gun$X2014.10.15, decreasing=TRUE), ][0:5, ])
  week3.matrix <- cbind(uswords.week3, ebola.week3, gun.week3)
  
  # Find 5 most frequent words in week 4
  uswords.week4 <- rownames(uswords_freq[order(uswords$X2014.11.21, decreasing=TRUE), ][0:5, ])
  ebola.week4 <- rownames(ebola_freq[order(ebola$X2014.10.27, decreasing=TRUE), ][0:5, ])
  gun.week4 <- rownames(gun_freq[order(gun$X2014.10.22, decreasing=TRUE), ][0:5, ])
  week4.matrix <- cbind(uswords.week4, ebola.week4, gun.week4)
  
  print("Most frequent words in Week 1")
  print(week1.matrix)
  print("Most frequent words in Week 2")
  print(week2.matrix)
  print("Most frequent words in Week 3")
  print(week3.matrix)
  print("Most frequent words in Week 4")
  print(week4.matrix)
  
}

# Find out if ebola or ferguson is more frequently mentioned in US cities data set
# Use Fisher Test
morefrequent <- function()
{
  ebolawords <- rownames(ebola) %in% rownames(cities)
  ebolawords <- table(ebolawords)
  
  fergusonnames <- rownames(ferguson) %in% rownames(cities)
  fergusonnames <- table(fergusonnames)
  
  morefrequent.matrix <- matrix(c(fergusonnames, ebolawords, ebolawords[2], ebolawords[2]), nrow=2,ncol=2)
  
  print('Ferguson is mentioned 1.2x more than Ebola in the Tweets from major cities')
  print(fisher.test(morefrequent.matrix, alternative="greater"))
}

# How does the most frequent word from the first week of Ebola & Ferguson & all US cities trend over course of 4 weeks
trendingfrequency <- function () {
  plot(x=c(1,2,3,4), y=log(uswords_freq[c("emabiggestfansjustinbieber"),]+1e-12), ylim=c(-20, -0))
  plot(x=c(1,2,3,4), y=log(ebola_freq[c("braintree"),]+1e-12), ylim=c(-20, -0))
  plot(x=c(1,2,3,4), y=log(gun_freq[c("detention"),]+1e-12), ylim=c(-20, -0))
  
  print("See plot")
  print("All top words generally trend even stronger as the weeks progress")
}

main <- function()
{
  print("What are the most frequent words in each week of the data sets (all us cities, ebola, ferguson?")
  pause()
  mostfrequent()
  print("Is Ebola or Ferguson more frequently mentioned in US cities data set?")
  pause()
  morefrequent()
  print("How does the most frequent word from the first week of Ebola & Ferguson & all US cities trend over course of 4 weeks?")
  pause()
  trendingfrequency()
}

main()

