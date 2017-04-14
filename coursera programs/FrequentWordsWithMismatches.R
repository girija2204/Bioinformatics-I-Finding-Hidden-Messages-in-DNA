source("/home/giri/Downloads/MTech Thesis/coursera programs/Neighbours.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/PatternToNumber.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/NumberToPattern.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/ApproxPatternCount.R")

FrequentWordsWithMismatches = function(text, k, d){
  FrequentPatterns = vector()
  x = '^'(4,k)
  neigbour_array = frequency_array = array(0, dim = c(1,x))
  for (i in 1:(nchar(text)-k+1)) {
    pattern1 = substr(text, i, (i + k - 1))
    Neighbourhood = Neighbours(pattern1, d)
    for (i in 1:length(Neighbourhood)) {
      index = PatternToNumber(Neighbourhood[i])
      neigbour_array[index+1] = 1
    }
  }
  for (i in 1:x) {
    if(neigbour_array[i] == 1){
      pattern = NumberToPattern(i-1, k)
      frequency_array[i] = ApproxPatternCount(pattern, text, d)
    }
  }
  maxCount = max(frequency_array)
  for (i in 1:x) {
    if(frequency_array[i] == maxCount){
      pattern = NumberToPattern(i-1, k)
      FrequentPatterns = append(FrequentPatterns, pattern)
    }
  }
  return(FrequentPatterns)
}

text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
k = 4
d = 1
print(FrequentWordsWithMismatches(text, k, d))
