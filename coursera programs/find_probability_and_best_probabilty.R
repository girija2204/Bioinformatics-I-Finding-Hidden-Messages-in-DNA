find_probability = function(dna_string, result){
  probability = vector()
  length = nchar(dna_string)
  for(i in 1:length){
    dna_neuc = substr(dna_string, i, i)
    if(dna_neuc == "A") x = 1 else if(dna_neuc == "C") x = 2 else if(dna_neuc == "G") x = 3 else x = 4
    probability = append(probability, result[x,i])
  }
  product_prob = prod(probability)
  return(product_prob)
}

find_best_probability = function(length, result){
  best_probabilty = vector()
  aa = vector(length = length)
  for (i in 1:length) {
    max = result[1,i]
    for (j in 2:4) {
      if(result[j,i] > max){
        max = result[j,i]
        aa[i] = c('A','C','G','T')[j]
      }
      # else
      #   aa[i] = 'A'
    }
    best_probabilty = append(best_probabilty, max)
  }
  for (i in 1:length) {
    if(aa[i] == "FALSE")
      aa[i] = 'A'
  }
  print(aa)
  return(best_probabilty)
}



dna_set = c("TCGGGGGTTTTT","CCGGTGACTTAC","ACGGGGATTTTC","TTGGGGACTTTT","AAGGGGACTTCC","TTGGGGACTTCC","TCGGGGATTCAT","TCGGGGATTCCT", "TAGGGGAACTAC","TCGGGTATAACC")
Greedy_or_Laplace = 0
result = Scope(dna_set, Greedy_or_Laplace)
# result = matrix(nrow = 4, ncol = 6)
# result = rbind(c(0.4, 0.3, 0.0, 0.1, 0.0, 0.9), c(0.2, 0.3, 0.0, 0.4, 0.0, 0.1), c(0.1, 0.3, 1.0, 0.1, 0.5, 0.0), c(0.3, 0.1, 0.0, 0.4, 0.5, 0.0))
best_probability = find_best_probability(nchar(dna_set[1]), result[[3]])
print(best_probability)
# print(prod(best_probability))
# 
#  dna_string = "CAGTGGTCATGA"
#  probability = find_probability(dna_string, result[[3]])
#  print(probability)
#  # 