dna = c("A", "C", "G", "T")

Scope = function(dna_set, Greedy_or_Laplace){
  columns = nchar(dna_set[1])
  dna_set_mat = matrix(nrow = length(dna_set), ncol = columns)
  for (i in 1:length(dna_set)) {
    temp_arr = vector()
    for (j in 1:columns) {
      temp_arr = append(temp_arr, substr(dna_set[i], j, j))
    }
    dna_set_mat[i,] = temp_arr
  }
  Scope = matrix(nrow = 1,ncol = columns)
  Count = matrix(nrow = 4,ncol = columns)
  Profile = matrix(nrow = 4, ncol = columns)
  Consensus = vector()
  Total_entropy = 0

  for (i in 1:columns) {
    a = c = g = t = 0
    for (k in 1:length(dna_set)) {
      if(dna_set_mat[k,i] == "A")
        a = a + 1
      else if(dna_set_mat[k,i] == "C")
        c = c + 1
      else if(dna_set_mat[k,i] == "G")
        g = g + 1
      else
        t = t + 1
    }
    Scope[1,i] = length(dna_set)-max(c(a,c,g,t))
    max = max_char = 0
    Entropy_col = 0
    deno = 0
    for (z in 1:4) {
      if(Greedy_or_Laplace == 0)
        Count[z,i] = c(a,c,g,t)[z]
      else
        Count[z,i] = c(a,c,g,t)[z] + 1
      deno = deno + Count[z,i]
      if(Count[z,i] > max){
        max = Count[z,i]
        max_char = c("A","C","G","T")[z]
      }
    }
    for (z in 1:4) {
      Profile[z,i] = Count[z,i]/deno
      if(Profile[z,i]!=0)
        Entropy_col = Entropy_col + (Profile[z,i]*log2(Profile[z,i]))
    }

    Consensus = append(Consensus, max_char)
    Total_entropy = Total_entropy + -Entropy_col
  }
  result = list(Scope, Count, Profile, Consensus, Total_entropy)
  # result = list(dna_set_mat,Scope, Count, Profile, Consensus)
  return(result)
}

# dna_set = c("CGCCTGGTGT", "AGGTCACCCT", "ATGCTAAGGT", "TCAACGGTGG", "GTCCGTAACG")
# Greedy_or_Laplace = 0
# result = Scope(dna_set, Greedy_or_Laplace)
# print(result[1])
# print(result[2])
# print(result[3])
# print(result[4])
# print(result[5])
