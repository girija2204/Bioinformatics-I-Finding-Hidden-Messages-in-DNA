source("/home/giri/Downloads/MTech Thesis/coursera programs/HammingDistance.R")
ApproxPatternCount = function(pattern, text, d){
  count = 0
  for (i in 1:(nchar(text)-nchar(pattern)+1)) {
    pattern1 = substr(text, i, (i + nchar(pattern) - 1))
    if(HammingDist(pattern, pattern1) <= d){
      count = count + 1
    }
  }
  return(count)
}

# pattern = "TGT"
# text = "ACTT"
# d = 1
# count = ApproxPatternCount(pattern, text, d)
# print(count)