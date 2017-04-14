source("/home/giri/Downloads/MTech Thesis/coursera programs/Motif_Finding_1.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/Motif_Finding_4.R")

Randomized_Motif = function(k, t, dna_set){
  no_char = (nchar(dna_set[1])-k+1)
  x = floor(runif(length(dna_set), min = 1, max = no_char))
  best_motifs = motifs = vector()
  for (i in 1:length(dna_set)) {
   motif = substr(dna_set[i], x[i], (x[i]+k-1))
   motifs = append(motifs, motif)
  }
  #motifs = c("GTC", "CCC", "ATA", "GCT")
  best_motifs = motifs
  while(1){
    Profile = Scope(motifs,0)[[3]]
    motifs = vector()
    for (i in 1:length(dna_set)) {
      motif = profile_most_probable_kmer(dna_set[i],k,Profile)
      motifs = append(motifs, motif)
    }
    if(sum(Scope(motifs,0)[[1]])<sum(Scope(best_motifs,0)[[1]]))
      best_motifs = motifs
    else
      return(best_motifs)
  }
}


k = 8
t = 1
dna_set = c("CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA",
            "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG",
            "TAGTACCGAGACCGAAAGAAGTATACAGGCGT",
            "TAGATCAAGTTTCAGGTGCACGTCGGTGAACC",
            "AATCCACCAGCTCCACGTGCAATGTTGGCCTA")
print(Randomized_Motif(k,t,dna_set))
# no_of_runs = 10
# all_motifs = matrix(nrow = no_of_runs, ncol = t)
# all_motifs_count = matrix(nrow = no_of_runs, ncol = 1)
# max_mot = best_mot = matrix(nrow = 1, ncol = t)
# 
# for (i in 1:no_of_runs) {
#   motifs = Randomized_Motif(k, t, dna_set)
#   for (j in 1:t) {
#     all_motifs[i,j] = motifs[j]
#   }
# }
# for (i in 1:no_of_runs) {
#   for (j in 1:t) {
#     all_motifs_count[i,j] = 0
#   }
# }
# for (i in 1:t) {
#   for (j in 1:no_of_runs) {
#     mm = all_motifs[j,i]
#     if(j==1)
#       all_motifs_count[j,i] = 1
#     else{
#       for (l in 1:(j-1)) {
#         if(mm == all_motifs[l,i])
#           all_motifs_count[j,i] = all_motifs_count[l,i] + 1
#       }
#       if(all_motifs_count[j,i] == 0)
#         all_motifs_count[j,i] = 1
#     }
#   }
#   max_mot[i] = max(all_motifs_count[,i])
# }
# for (i in 1:t) {
#   for (j in 1:no_of_runs) {
#     if(all_motifs_count[j,i] == max_mot[i])
#       best_mot[i] = all_motifs[j,i]
#   }
# }
# print(all_motifs)
# print(all_motifs_count)
# print(max_mot)
# print(best_mot)
# print(Scope(best_mot,0)[[1]])
