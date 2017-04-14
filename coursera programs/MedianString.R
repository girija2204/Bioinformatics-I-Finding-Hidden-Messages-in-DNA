source("/home/giri/Downloads/MTech Thesis/coursera programs/Motif_Finding_2.R")

dna_set = c("CTCGATGAGTAGGAAAGTAGTTTCACTGGGCGAACCACCCCGGCGCTAATCCTAGTGCCC", "GCAATCCTACCCGAGGCCACATATCAGTAGGAACTAGAACCACCACGGGTGGCTAGTTTC", "GGTGTTGAACCACGGGGTTAGTTTCATCTATTGTAGGAATCGGCTTCAAATCCTACACAG")

distance = 999999
k = 7
x = '^'(4,k)
for (i in 1:x) {
  pattern = NumberToPattern(i,k)
  x = motif(pattern, dna_set)
  if(distance > sum(unlist(x[1]))){
    distance = sum(unlist(x[1]))
    Median = pattern
  }
}
print(Median)
