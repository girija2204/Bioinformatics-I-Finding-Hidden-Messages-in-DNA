dna = c("A", "C", "G", "T")
Neighbours = function(pattern){
  neighbour_set = vector()
  for (i in 1:length(dna)) {
    indices = unlist(gregexpr(dna[i], pattern))
    if(indices != -1){
      for (j in 1:length(indices)) {
        temp_pattern = pattern
        dd = 0
        for (k in 1:3) {
          if((i+k)<=4)
            dd = dna[i+k]
          else
            dd = dna[(i+k)%%4]
          neighbour = paste(substr(temp_pattern, 1, (indices[j] - 1)), dd, substr(temp_pattern, (indices[j] + 1), nchar(temp_pattern)), sep = "")
          neighbour_set = append(neighbour_set, neighbour)
        }
      }
    }
    else{}
  }
  return(neighbour_set)
}

pattern = "CCAGTCAATG"
neighbour_set = Neighbours(pattern)
print(neighbour_set)