

setwd("/Users/sanik/SP-Assesment1") 
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

New_text2 <- tolower(New_text) ##LowerCase all elements in bible text vector

Unique_words <- unique(New_text2) ##Vector of all  unqiue words created

z <- match(New_text2, Unique_words) ##What position of bible is in Unique words vector

No_words <- tabulate(z) ##Frequency of bible text

Thousand_No_words_freq <- No_words[No_words > 89] ##Imposes lower limit to generate only 1000 words

print(length(Thousand_No_words_freq)) ##1000 words check

b <- Unique_words[match(Thousand_No_words_freq, No_words)]

print(b)

com_txt <- match(New_text2, b)
com_txt
com_pairs <- rowSums(cbind(com_txt, subs = com_txt+1))
com_pairs
wo_na <- na.omit(com_pairs)
wo_na
