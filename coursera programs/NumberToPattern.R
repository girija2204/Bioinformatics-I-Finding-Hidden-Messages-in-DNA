dna = c("A", "C", "G", "T")

NumberToPattern = function(number, k){
  if(k == 1)
    return(dna[number+1])
  prefixIndex = number%/%4
  remainder = number%%4
  symbol = dna[remainder+1]
  PrefixPattern = NumberToPattern(prefixIndex, k - 1)
  return(paste(PrefixPattern, symbol, sep = ""))
}

# number = 1
# k = 3
# NumberToPattern(number, k)
