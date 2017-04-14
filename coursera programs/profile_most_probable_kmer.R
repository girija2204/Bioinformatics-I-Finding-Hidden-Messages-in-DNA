source("/home/giri/Downloads/MTech Thesis/coursera programs/find_probability_and_best_probabilty.R")
profile_most_probable_kmer = function(text, k, profile_matrix){
  probability = 0
  pmp_kmer = 0
  for (i in 1:(nchar(text)-k+1)) {
    kmer = substr(text, i, i+k-1)
    pp = find_probability(kmer, profile_matrix)
    if(pp > probability){
      probability = pp
      pmp_kmer = kmer
    }
  }
  if(pmp_kmer == 0) pmp_kmer = substr(text, 1, k)
  return(pmp_kmer)
}

dna_set = c("TCGGGGGTTTTT","CCGGTGACTTAC","ACGGGGATTTTC","TTGGGGACTTTT","AAGGGGACTTCC","TTGGGGACTTCC","TCGGGGATTCAT","TCGGGGATTCCT", "TAGGGGAACTAC","TCGGGTATAACC")
Greedy_or_Laplace = 0
result = Scope(dna_set, Greedy_or_Laplace)
text = "AAGAATCAGTCA"
k = 6
# profile_matrix = rbind(c(0,0,0),c(0,0,1),c(1,1,0),c(0,0,0))
pmp_kmer = profile_most_probable_kmer(text, k, result[[3]])
print(pmp_kmer)
