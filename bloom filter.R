#Bloom Filter

install.packages("digest")
install.packages("bit")

library(digest)
library(bit)


spam <- read.table("listed_username_30.txt", sep = "\t", stringsAsFactors = FALSE)

colnames(spam) <- "usernames"

str(spam)

n <- nrow(spam)
p <- 0.07
m <- ceiling(-n*log(p) / (log(2)^2))
k <- ceiling(m/n * log(2))

hex_to_int = function(h) {
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16^(rev(seq_along(xx) - 1)))
}


hashing1 <- NULL
hashing2 <- NULL
hashing3 <- NULL
hashing4 <- NULL


h1 <- NULL
h2 <- NULL
h3 <- NULL
h4 <- NULL


for (i in 1:nrow(spam)){
  hashing1[i] <- digest(spam$usernames[i], algo = "murmur32", serialize = TRUE) 
  h1[i] <- hex_to_int(hashing1[i])
  h1[i] <- (h1[i] %% m) + 1
  hashing2[i] <- digest(spam$usernames[i], algo = "xxhash32", serialize = TRUE) 
  h2[i] <- hex_to_int(hashing2[i])
  h2[i] <- (h2[i] %% m) + 1
  hashing3[i] <- digest(spam$usernames[i], algo = "crc32", serialize = TRUE) 
  h3[i] <- hex_to_int(hashing3[i])
  h3[i] <- (h3[i] %% m) + 1
  hashing4[i] <- digest(spam$usernames[i], algo = "xxhash64", serialize = TRUE) 
  h4[i] <- hex_to_int(hashing4[i])
  h4[i] <- (h4[i] %% m) + 1
}

bit_vector <- bit(m)

for (i in 1:nrow(spam)){
  bit_vector[h1[i]] = 1
  bit_vector[h2[i]] = 1
  bit_vector[h3[i]] = 1
  bit_vector[h4[i]] = 1

}


stream <- read.table("listed_username_365.txt", stringsAsFactors = FALSE, sep = "\t")

summary(stream)

colnames(stream) <- "usernames"
str(stream)

h_n1 <- NULL
h_n2 <- NULL
h_n3 <- NULL
h_n4 <- NULL



for (i in 1:nrow(stream)){
  hashing1[i] <- digest(stream$usernames[i], algo = "murmur32", serialize = TRUE) 
  h_n1[i] <- hex_to_int(hashing1[i])
  h_n1[i] <- (h_n1[i] %% m) + 1
  hashing2[i] <- digest(stream$usernames[i], algo = "xxhash32", serialize = TRUE) 
  h_n2[i] <- hex_to_int(hashing2[i])
  h_n2[i] <- (h_n2[i] %% m) + 1
  hashing3[i] <- digest(stream$usernames[i], algo = "crc32", serialize = TRUE) 
  h_n3[i] <- hex_to_int(hashing3[i])
  h_n3[i] <- (h_n3[i] %% m) + 1
  hashing4[i] <- digest(stream$usernames[i], algo = "xxhash64", serialize = TRUE) 
  h_n4[i] <- hex_to_int(hashing4[i])
  h_n4[i] <- (h_n4[i] %% m) + 1
}



fp = 0
tn = 0
for (i in 1: nrow(stream)){
  if (bit_vector[h_n1[i]] ==1 && bit_vector[h_n2[i]] ==1 && bit_vector[h_n3[i]] ==1 && bit_vector[h_n4[i]] ==1){
    fp= fp+1
  }
  else {
    tn = tn+1
  }
}

fp/(fp+tn)*100
 
