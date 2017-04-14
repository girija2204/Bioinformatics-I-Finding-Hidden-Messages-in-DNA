implanted_motif_enumerator = function(dna_set, k, d){
  count = 0
  motif_pattern = vector()
  for (i in 1:length(dna_set)) {
    for (j in 1:(nchar(dna_set[i])-k+1)) {
      pattern = substr(dna_set[i], j, j+k-1)
      neighbours = Neighbours(pattern, d)
      for (k in 1:length(neighbours)) {
        gg = 0
        for (l in 1:length(dna_set)) {
          count = ApproxPatternCount(neighbours[k], dna_set[l], d)
          if(count == 0)
            break
          gg = gg + 1
        }
        if(gg == length(dna_set))
          motif_pattern = append(motif_pattern, neighbours[k])
      }
    }
  }
  return(motif_pattern)
}

k = 5
d = 2
dna_set = c("CACACATATACAAATTAGAATGGCT", "AGATAGAATTTCAGGGATGGACATA", "AGGCGGCACAACACATGCCGAATGC", "AGCGATACTCTTGCCTCGTAAGAGA", "ATAAAGAAGCATTCTATCTACAAGA", "AAAAACATAGATTAGTTAAAGTTGA")
motif_pattern = implanted_motif_enumerator(dna_set, k, d)
print(motif_pattern)