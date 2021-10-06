

setwd("/Users/AyushOza/Documents/Edinburgh/StatisticalProgramming/SP-Assesment1") 
a <-scan("1581-0.txt",what="character",skip=156) 
n <-length(a) 
a <-a[-((n-2909):n)] ## strip license

split_punct <- function(x, y, z, l, m, n, o) {
  
  ##split comma punctuation
  
  a <- grep(y, x, fixed= TRUE) ##which elements of x contain punctuation y
  b <- gsub(y, "", x, fixed = TRUE) ##remove punctuation from x
  c <- rep("", length(x)+length(a)) ##creates new vector to store split punctuation
  d <- a + 1:length(a) ##where should punctuation marks go
  c[d] <- y ##insert punctuation marks
  c[-d] <- b ##insert text without punctuation marks
  
  ##split full stop punctuation
  
  aa <- grep(z, c, fixed= TRUE) ##which elements of x contain punctuation y
  bb <- gsub(z, "", c, fixed = TRUE) ##remove punctuation from x
  cc <- rep("", length(c)+length(aa)) ##creates new vector to store split punctuation
  dd <- aa + 1:length(aa) ##where should punctuation marks go
  cc[dd] <- z ##insert punctuation marks
  cc[-dd] <- bb ##insert text without punctuation marks
  
  ##split semi-colon stop punctuation
  
  aaa <- grep(l, cc, fixed= TRUE) ##which elements of x contain punctuation y
  bbb <- gsub(l, "", cc, fixed = TRUE) ##remove punctuation from x
  ccc <- rep("", length(cc)+length(aaa)) ##creates new vector to store split punctuation
  ddd <- aaa + 1:length(aaa) ##where should punctuation marks go
  ccc[ddd] <- l ##insert punctuation marks
  ccc[-ddd] <- bbb ##insert text without punctuation marks
  
  ##split exlamation mark punctuation
  
  aaaa <- grep(m, ccc, fixed= TRUE) ##which elements of x contain punctuation y
  bbbb <- gsub(m, "", ccc, fixed = TRUE) ##remove punctuation from x
  cccc <- rep("", length(ccc)+length(aaaa)) ##creates new vector to store split punctuation
  dddd <- aaaa + 1:length(aaaa) ##where should punctuation marks go
  cccc[dddd] <- m ##insert punctuation marks
  cccc[-dddd] <- bbbb ##insert text without punctuation marks

  
  ##split colon mark punctuation
  
  aaaaa <- grep(n, cccc, fixed= TRUE) ##which elements of x contain punctuation y
  bbbbb <- gsub(n, "", cccc, fixed = TRUE) ##remove punctuation from x
  ccccc <- rep("", length(cccc)+length(aaaaa)) ##creates new vector to store split punctuation
  ddddd <- aaaaa + 1:length(aaaaa) ##where should punctuation marks go
  ccccc[ddddd] <- n ##insert punctuation marks
  ccccc[-ddddd] <- bbbbb ##insert text without punctuation marks
  
  ##split question mark punctuation
  
  aaaaaa <- grep(o, ccccc, fixed= TRUE) ##which elements of x contain punctuation y
  bbbbbb <- gsub(o, "", ccccc, fixed = TRUE) ##remove punctuation from x
  cccccc <- rep("", length(ccccc)+length(aaaaaa)) ##creates new vector to store split punctuation
  dddddd <- aaaaaa + 1:length(aaaaaa) ##where should punctuation marks go
  cccccc[dddddd] <- o ##insert punctuation marks
  cccccc[-dddddd] <- bbbbbb ##insert text without punctuation marks
  

}

New_text <- split_punct(a, ",",".", ";", "!", ":", "?")

low_text <- tolower(New_text) ##Lower Case all elements in bible text vector

Unique_words <- unique(low_text) ##Vector of all  unique words created

z <- match(low_text, Unique_words) ##What position of bible is in Unique words vector

No_words <- tabulate(z) ##Frequency of bible text

Thousand_No_words_freq <- No_words[No_words > 89] ##Imposes lower limit to generate only 1000 words

print(length(Thousand_No_words_freq)) ##1000 words check

b <- Unique_words[match(Thousand_No_words_freq, No_words)]

com_txt <- match(low_text, b)  ##matches lower case bible text with vector b to find indices of common words in bible text

com_txt_indx <- 1:length(com_txt) ##Creates an index of com_txt
com_txt_shift <- rep(NA, length(com_txt))
com_txt_shift[com_txt_indx] <- com_txt[com_txt_indx + 1] ##Creates a new vector where com_txt is shifted one space down
com_txt_shift

com_pairs_vector <- cbind(com_txt, com_txt_shift) ##Creates a new vector indexing subsequent words
com_pairs_vector
com_pairs <- rowSums(cbind(com_txt, com_txt_shift)) ##calculates sum of rows after creating 2 column matrix of indices of subsequent words
com_pairs
no_na <- com_pairs[!is.na(com_pairs)] ##removes the NA values from the sum of rows
no_na

unique_no_na <- unique(no_na) #Keeps unique values
unique_no_na

len_no_na <- length(unique_no_na) ##calculate the number of unique word pairs
len_no_na


A <- matrix(0:0, length(b), length(b)) #creates matrix 

##Loop below to add one to each element of matrix where i,j shows up in text.

for (k in 1:length(com_txt)) {
  l <- 1
  m <- 2
  q <- com_pairs_vector[k,l]
  r <- com_pairs_vector[k,m]
    if (!is.na(com_pairs_vector[k,l]) & !is.na(com_pairs_vector[k,m])) {
      A[q,r] <- A[q,r] + 1
    }
  }

A[1:10,1:10]
  
##Standardize rows of A[i, j] to be interpreted as probability that b[j] will follow b[i]

RSum <- rowSums(A)      # calculating rowSum of A 

Probabilities <- A/RSum # standardizing rows of A
Probabilities[1:10, 1:10]
Probabilities[is.nan(Probabilities)] <- 1/length(b)
  
ProbsSum <- rowSums(Probabilities)  # verifying for probability sum = 1
ProbsSum

NewProbSum <- ProbsSum    # removing unwanted NaNs
NewProbSum[is.nan(NewProbSum)] <- 0
NewProbSum

##Simulate 50-word section from the model

rdn_sampl <- sample.int((length(b)), size = 1)   ##generates random integer index from b

Model_vector <- rep(0, 50)  ##Initiates sample index vector of length 50 with values 0 

Model_vector[1] <- rdn_sampl ##Inputs the index of first word into sample index vector

row_of_a <- rdn_sampl ##Initiates first row of matrix a for sample to take s probability distribution

##Loop to cycle through matrix A, picking the row of a certain word and taking the next word based on the probbaility distribution from the row

for (i in 2:50) {
  Row_probabilities <- Probabilities[row_of_a,]
  Model_vector[i] <- sample(1:length(b), 1, replace = FALSE, prob = Row_probabilities)
  row_of_a <- Model_vector[i]
}

Word_vector <- b[Model_vector] ##creates a new vector which has words, rather than index of words

##Replace most frequent capitalized words

Unique_words_cap <- unique(New_text)
Unique_words_cap

Capital_check <- rep("", length(Unique_words_cap))

for (i in 1:length(Unique_words_cap)) {
  if (substr(Unique_words_cap[i], 1, 1) %in% LETTERS) { 
    Capital_check[i] <- "Yes" 
  } else { 
    Capital_check[i] <- "No"
  }
}

Unique_words_cap <- Unique_words_cap[Capital_check == "Yes"]

z_caps <- match(New_text, Unique_words_cap)

No_words_caps <- tabulate(z_caps)

match_entries <- match(tolower(Unique_words_cap), Unique_words)

Most_likely_caps <- rep("", length(Unique_words_cap))

for (i in 1:length(Unique_words_cap)) {
  if (No_words_caps[i] > (No_words[match_entries[i]] - No_words_caps[i])) {
    Most_likely_caps[i] <- "Yes"
  } else {
    Most_likely_caps[i] <- "No"
  }
}

Unique_words_cap_updated <- Unique_words_cap[Most_likely_caps == "Yes"]

matched_sample <- match(Word_vector, tolower(Unique_words_cap_updated))

Word_vector_caps <- Word_vector

for (i in 1:length(Word_vector)) {
  if (!is.na(matched_sample[i])) {
    Word_vector_caps[i] <- Unique_words_cap_updated[matched_sample[i]]
  }
}

Word_vector_caps <- Word_vector_caps[!is.na((Word_vector_caps))]


Word_vector
Word_vector_caps

