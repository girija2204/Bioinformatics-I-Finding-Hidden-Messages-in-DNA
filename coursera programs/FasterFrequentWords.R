source("/home/giri/Downloads/MTech Thesis/coursera programs/ComputingFrequencies.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/NumberToPattern.R")
FasterFrequentWords = function(text, k){
  FrequencyArray = array()
  x = '^'(4,k)
  for (i in 1:(x)){
    FrequencyArray[i] = 0
  }
  FrequencyArray = ComputingFrequencies(FrequencyArray, text, k)
  max = max(FrequencyArray)
  for (i in 1:(x)){
    if(FrequencyArray[i] == max){
      Pattern = NumberToPattern(i-1,k)
      FrequentPatterns = append(FrequentPatterns, Pattern)
    }
  }
  print(FrequentPatterns)
  print(max)
}


text = "CGGAGGACTCTAGGTAACGCTTATCAGGTCCATAGGACATTCA"

k = 3
FasterFrequentWords(text, k)

