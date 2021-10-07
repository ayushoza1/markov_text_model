## Sanika Baxi - s2159255		Ayush Oza - s2184992		Gowtham Palepu - s2113890

## Statistical Programming - Practical -1 : A Markov Text Model

## 1. A git repo has been created and used for group work and implementation. Updated code along with text file were pushed onto the repo.

## 2. Plain text from given link has been downloaded and used for the model. 
## https://www.gutenberg.org/ebooks/1581 - link for reference. 

## 3. Following lines of code was implemented to read the text file from local machine to R. 
## Steps to calculate length of the text and strip away unwanted license data were implemented.

setwd("/Users/AyushOza/Documents/Edinburgh/StatisticalProgramming/SP-Assesment1") 
a <-scan("1581-0.txt",what="character",skip=156) 
n <-length(a) 
a <-a[-((n-2909):n)] 

## 4. Data pre-processing function 'split_punc' for separation of given punctuation marks has been created. 
## The function 'split_punc' takes in text vector 'a' and punctuation marks as arguments. 

split_punct <- function(x, y, z, l, m, n, o) {
  
  ## Below part of the function separates punctuation argument y wherever occurred. 
  
  a <- grep(y, x, fixed= TRUE) 								## Checks for which element of x contains a punctuation y.
  b <- gsub(y, "", x, fixed = TRUE) 						## Removes punctuation from x.
  c <- rep("", length(x)+length(a)) 						## Creates new vector to store split punctuation.
  d <- a + 1:length(a) 										## These lines of code arrange word and punctuation mark separately. 
  c[d] <- y 							
  c[-d] <- b 							
  
  ## The same has been implemented to separate out all the listed punctuation marks like ",", ".", ";", "!", ":" and "?"
  ## Below part of the function separates punctuation argument 'z' wherever occurred.
  
  aa <- grep(z, c, fixed= TRUE) 			
  bb <- gsub(z, "", c, fixed = TRUE) 		
  cc <- rep("", length(c)+length(aa)) 		
  dd <- aa + 1:length(aa) 					
  cc[dd] <- z 								
  cc[-dd] <- bb 							
  
  ## Below part of the function separates punctuation argument 'l' wherever occurred.
  
  aaa <- grep(l, cc, fixed= TRUE) 				
  bbb <- gsub(l, "", cc, fixed = TRUE) 			
  ccc <- rep("", length(cc)+length(aaa)) 		
  ddd <- aaa + 1:length(aaa) 					
  ccc[ddd] <- l 								
  ccc[-ddd] <- bbb 								
  
  ## Below part of the function separates punctuation argument 'm' wherever occurred.
  
  aaaa <- grep(m, ccc, fixed= TRUE) 						
  bbbb <- gsub(m, "", ccc, fixed = TRUE) 					
  cccc <- rep("", length(ccc)+length(aaaa)) 				
  dddd <- aaaa + 1:length(aaaa) 							
  cccc[dddd] <- m 											
  cccc[-dddd] <- bbbb 										

  
  ## Below part of the function separates punctuation argument 'n' wherever occurred.
  
  aaaaa <- grep(n, cccc, fixed= TRUE) 					
  bbbbb <- gsub(n, "", cccc, fixed = TRUE) 				
  ccccc <- rep("", length(cccc)+length(aaaaa)) 			
  ddddd <- aaaaa + 1:length(aaaaa) 						
  ccccc[ddddd] <- n 									
  ccccc[-ddddd] <- bbbbb 								
  
  ## Below part of the function separates punctuation argument 'o' wherever occurred.
  
  aaaaaa <- grep(o, ccccc, fixed= TRUE) 					
  bbbbbb <- gsub(o, "", ccccc, fixed = TRUE) 				
  cccccc <- rep("", length(ccccc)+length(aaaaaa)) 			
  dddddd <- aaaaaa + 1:length(aaaaaa) 						
  cccccc[dddddd] <- o 										
  cccccc[-dddddd] <- bbbbbb 								
  

}

## 5. The created function has been called by passing the punctuations as arguments. 
## The stated punctuation marks were passed as follows y = ',' z = '.'  l = ';' m = '!' n = ':' and o = '?'

New_text <- split_punct(a, ",",".", ";", "!", ":", "?")
print(New_text)

## New_text holds the output for 5 with words and punctuation marks separated.

## 6.(a) Base-R function 'tolower' was used to convert the text to lower case.  

low_text <- tolower(New_text) 

## 6.(b) 'unique' was used to create a vector of unique words from the text.  

Unique_words <- unique(low_text) 

## 'match' was used to compare the words from lower case text vector and unique word vector. 

z <- match(low_text, Unique_words) 

## 6.(c) 'tabulate' was used to calculate frequency of occurance of each unique word. 

No_words <- tabulate(z) 

## 6.(d) A threshold limit of 1000 frequently occuring words was implemented. 

Thousand_No_words_freq <- No_words[No_words > 89] 			## Imposes lower limit to generate only 1000 words.

## Use this to print Thousand words vector print(length(Thousand_No_words_freq)). 

## 6.(e) A vector 'b' of 1000 most frequently occurring words was created. 

b <- Unique_words[match(Thousand_No_words_freq, No_words)]

## 7. Below steps were all implemented to create a matrix 'A' as required.
## 7.(a) 'match' was used to find the frequently occuring words from 'b' in lower case text. 

com_txt <- match(low_text, b)  

## 7.(b) Below lines of code were implemented to generate a vector to create two column matrix. 		

com_txt_indx <- 1:length(com_txt) 							## Creates an index of com_txt. 
com_txt_shift <- rep(NA, length(com_txt))
com_txt_shift[com_txt_indx] <- com_txt[com_txt_indx + 1] 	## Creates a new vector where com_txt is shifted one space down.

## 'cbind' was used to create a two column matrix with vector created by match and a shifted entry. 

com_pairs_vector <- cbind(com_txt, com_txt_shift) 			## Creates a new vector indexing subsequent words

## Use this to print common pairs vector print(com_pairs_vector). 

## 7.(c) 'rowSums' was used to calculate sum of each row in the matrix and 'is.na' to drop NA pairs. 

com_pairs <- rowSums(cbind(com_txt, com_txt_shift)) 
no_na <- com_pairs[!is.na(com_pairs)] 

## 7.(d) Below lines of code were implemented to loop through and create a matrix 'A'.

unique_no_na <- unique(no_na) 

len_no_na <- length(unique_no_na) 

A <- matrix(0:0, length(b), length(b)) 						## Creates an empty matrix A with necessary dimensions 

## Loop below is to add one to each element of matrix where i,j shows up in text.

for (k in 1:length(com_txt)) {
  l <- 1
  m <- 2
  q <- com_pairs_vector[k,l]
  r <- com_pairs_vector[k,m]
    if (!is.na(com_pairs_vector[k,l]) & !is.na(com_pairs_vector[k,m])) {
      A[q,r] <- A[q,r] + 1
    }
  }
  
## Matrix 'A' created. 

## 7.(e) Below lines of code were implemented to standardize the rows of matrix 'A'. 
## Standardize rows of A[i, j] to be interpreted as probability that b[j] will follow b[i]. 

RSum <- rowSums(A)       

Probabilities <- A/RSum 									## Standardizing rows of A. 
Probabilities[is.nan(Probabilities)] <- 1/length(b)
  
ProbsSum <- rowSums(Probabilities)  						## Verifying each row of probabilities in A add upto 1. 


## 8. Simulation of 50 word section from the model. 

rdn_sampl <- sample.int((length(b)), size = 1)   			## 'sample' function generates a random integer index from 'b'. 

Model_vector <- rep(0, 50)  								## Initiates sample index vector of length 50 with values 0. 

Model_vector[1] <- rdn_sampl 								## Inputs the index of first word into sample index vector. 

row_of_a <- rdn_sampl 										## Initiates first row of matrix for sample to take probability distribution. 

## Loop to cycle through matrix A, picking the row of a certain word and taking the next word based on the probaility distribution from the row. 

for (i in 2:50) {
  Row_probabilities <- Probabilities[row_of_a,]
  Model_vector[i] <- sample(1:length(b), 1, replace = FALSE, prob = Row_probabilities)
  row_of_a <- Model_vector[i]
}

Word_vector <- b[Model_vector] 								## Creates a new vector which has words, rather than index of words. 

## Word_vector now holds the necessary result viz., 50 word sample. 
## Use this to print the word vector print(Word_vector). 


## 9. Implementation of the model to for capital words. 

## Replace most frequent capitalized words. 

Unique_words_cap <- unique(New_text) 						## Generate a list of new unique words with capitalized words. 

Capital_check <- rep("", length(Unique_words_cap))          ## Initiate a vector determining if words are capitalized. 

## Loop through all unique words searching for capitalized words by seeing if first letter is in LETTER vector. 

for (i in 1:length(Unique_words_cap)) {
  if (substr(Unique_words_cap[i], 1, 1) %in% LETTERS) { 
    Capital_check[i] <- "Yes" 
  } else { 
    Capital_check[i] <- "No"
  }
}

Unique_words_cap <- Unique_words_cap[Capital_check == "Yes"] ## Strip out non-capitalized words. 

z_caps <- match(New_text, Unique_words_cap) 				 ## Position of bible text in unique words vector. 

No_words_caps <- tabulate(z_caps) 							 ## Frequency of Capitalized words in bible text. 

match_entries <- match(tolower(Unique_words_cap), Unique_words) 			## Position of capitalized word in unique capitalized words vector to the the corresponding unique words vector. 

Most_likely_caps <- rep("", length(Unique_words_cap)) 		 				## Initial vector determining if the capitalized word occurs more times than coressponding non-capitalized word. 

## Loop through each capitalized word checking if the frequency is greater than the non-capitalized counterpart. 

for (i in 1:length(Unique_words_cap)) {
  if (No_words_caps[i] > (No_words[match_entries[i]] - No_words_caps[i])) {
    Most_likely_caps[i] <- "Yes"
  } else {
    Most_likely_caps[i] <- "No"
  }
}

Unique_words_cap_updated <- Unique_words_cap[Most_likely_caps == "Yes"] 	## Strip all words that occur more in lower case. 

matched_sample <- match(Word_vector, tolower(Unique_words_cap_updated)) 	## Check if any words in our 50 word sample occur in the most frequently occuring capitals list. 

Word_vector_caps <- Word_vector 											## Initiate capitalized sample vector. 

## Loop to replace all words that were matched in the capitalized list with. 

for (i in 1:length(Word_vector)) {
  if (!is.na(matched_sample[i])) {
    Word_vector_caps[i] <- Unique_words_cap_updated[matched_sample[i]]
  }
}

## Printing desired outputs.

cat(Word_vector)

cat(Word_vector_caps)

## End of implementation of the model.