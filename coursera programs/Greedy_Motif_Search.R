Greedy_Motif_Search = function(dna_set, k, t){
  best_motifs = vector()
  motifs = vector()
  for (i in 1:t) {
    best_motifs = append(best_motifs, substr(dna_set[i], 1, k))
  }
  for (i in 1:(nchar(dna_set[1])-k+1)) {
    motifs = mot = vector()
    mot = append(mot, substr(dna_set[1], i, i + k -1))
    for (j in 2:t) {
      prof = Scope(mot,0)[[3]]
      mot = append(mot, profile_most_probable_kmer(dna_set[j], k, prof))
    }
    if(sum(Scope(mot,0)[[1]])<sum(Scope(best_motifs,0)[[1]]))
      best_motifs = mot
  }
  return(best_motifs)
}

dna_set = c("GTCTGCCATCTCTGATGGATGTGATGGGCA",
"CTGCCACATCAAGGGAATTGTTGAAGGACA",
"CAGCAGCACGCCCACCGGTCGCGACGGGGT",
"CGCCTCGCTCGAGAGAAGCAGATGAGGGAG",
"GTCACGCGGTTGGGGATGGTGTCGGGGATG",
"AGGTAAAGCTCTTGTTTCTGAAGAAGGAGA")
k = 12
t = 5
Greedy_or_Laplace = 0
g = Greedy_Laplacian_Motif_Search(dna_set, Greedy_or_Laplace, k, t)
print(g)

# ss = matrix(nrow = 1,ncol = k)
# g = t(t(g))
# for (zz in 1:k) {
#   aa = cc = gg = tt = 0
#   for (yy in 1:t) {
#     if(substr(g[yy], zz, zz) == "A")
#       aa = aa + 1
#     else if(substr(g[yy], zz, zz) == "C")
#       cc = cc + 1
#     else if(substr(g[yy], zz, zz) == "G")
#       gg = gg + 1
#     else if(substr(g[yy], zz, zz) == "T")
#       tt = tt + 1
#   }
#   ss[1,zz] = length(g)-max(c(aa,cc,gg,tt))
# }
# print(sum(ss))