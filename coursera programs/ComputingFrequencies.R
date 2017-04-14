source("/home/giri/Downloads/MTech Thesis/coursera programs/PatternToNumber.R")
#library(bigmemory)
ComputingFrequencies = function(FrequencyArray, text, k){
  for (i in 1:(nchar(text)-k+1)) {
    Pattern = substr(text, i, i + k - 1)
    j = PatternToNumber(Pattern)
    j = j + 1
    FrequencyArray[1,j] = FrequencyArray[1,j] + 1
  }
  return(FrequencyArray)
}

# text = "GGTCCGTCCGTCC"
# k = 9
# x = '^'(4,k)
# FrequencyArray <- big.matrix(nrow = 1, ncol = '^'(4,k), type = "integer", init = 0)
# FrequencyArray = ComputingFrequencies(FrequencyArray, text, k)
# print(FrequencyArray)
