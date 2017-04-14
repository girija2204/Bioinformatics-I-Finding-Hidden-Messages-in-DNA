source("/home/giri/Downloads/MTech Thesis/coursera programs/HammingDistance.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/Suffix_And_FirstSymbol.R")
dna = c("A", "C", "G", "T")
Neighbours = function(pattern, d){
  if(d == 0)
    return(pattern)
  if(nchar(pattern) == 1){
    return(dna)
  }
  Neighbour = vector()
  FirstSymbol = Suffix_And_FirstSymbol(pattern)[1]
  Suffix = Suffix_And_FirstSymbol(pattern)[2]
  SuffixNeighbour = Neighbours(Suffix, d)
  for (i in 1:length(SuffixNeighbour)) {
    if(HammingDist(Suffix, SuffixNeighbour[i]) < d){
      for (j in 1:4) {
        Neighbour = append(Neighbour, paste(dna[j], SuffixNeighbour[i], sep = ""))
      }
    }
    else{
      Neighbour = append(Neighbour, paste(FirstSymbol, SuffixNeighbour[i], sep = ""))
    }
  }
  return(Neighbour)
}

# pattern = "CACAC"
# d = 1
# Neighbour = Neighbours(pattern, d)
# for (i in 1:length(Neighbour)) {
#   print(Neighbour[i])
# }