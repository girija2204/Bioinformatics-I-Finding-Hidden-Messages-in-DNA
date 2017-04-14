ClumpFinding = function(text, l, k, t){
  FrequentPatterns <- vector()
  FrequencyArray <- vector()
  Clump <- vector()
  x <- '^'(4,k)
  for (i in 1:x) {
    Clump[i] <- 0
  }
  for (i in 1:(nchar(text)-l+1)) {
    text_part = substr(text, i, i + l - 1)
    for (i in 1:x) {
      FrequencyArray[i] <- 0
    }
    FrequencyArray = ComputingFrequencies(FrequencyArray, text_part, k)
    for (i in 1:x) {
      if(FrequencyArray[i] >= t){
        Clump[i] <- 1
      }
    }
  }
  for (i in 1:x) {
    if(Clump[i] == 1){
      FrequentPatterns <- append(FrequentPatterns, NumberToPattern(i-1, k))
    }
  }
  return(FrequentPatterns)
}

text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
l = 50
k = 5
t = 4
FrequentPatterns = BetterClumpFinding(text, l, k, t)
print(FrequentPatterns)
