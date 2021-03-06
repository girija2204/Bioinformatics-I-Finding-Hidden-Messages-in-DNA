FrequentWords = function(Text, k){
  count_arr = array()
  FrequentPatterns = array()
  
  PatternCount = function(text, pat){
    count = 0
    for(j in 1:(nchar(text)-k+1)){
      if(substr(text, j, j + k - 1) == pat)
        count = count + 1
    }
    return(count)
  }
  
  for(i in 1:(nchar(text)-k+1)){
    Pattern = substr(text, i, i + k - 1)
    count = PatternCount(text, Pattern)
    count_arr[i] = count
  }
  
  max_count = max(count_arr)
  freq_pat = array()
  for(i in 1:(nchar(text)-k+1)){
    if(count_arr[i] == max_count)
      freq_pat = append(freq_pat, substr(text, i, i + k - 1))
  }
  
  FrequentPatterns = append(FrequentPatterns, unique(freq_pat))
  print(FrequentPatterns)
}





# text = c("TCGCGCACGGGTTGGGGGTTGGTCGCGCACTGGTGGTTGTGGTGGTTGGGGTTGGTCGCGCACTGGTGGTTGTGTTGTCTGACCGCTTGGTGGTTGTGTTGTCTGTTGTCTCGCGCACTCGCGCACTGGTGGTTGTGGTGGTTGTGGTGGTTGTGTTGTCGGGTTGGTCGCGCACTCGCGCACTCGCGCACTGTTGTCTGTTGTCTGACCGCTTGTTGTCTCGCGCACTGACCGCTTGGTGGTTGTGACCGCTTGACCGCTTGGTGGTTGTGACCGCTTGGTGGTTGTCGCGCACTGGTGGTTGTGTTGTCGGGTTGGTCGCGCACTGACCGCTTGTTGTCTCGCGCACTGTTGTCTGACCGCTTGACCGCTTCGCGCACGGGTTGGTGGTGGTTGTGACCGCTTCGCGCACTGTTGTCTCGCGCACTCGCGCACTCGCGCACTCGCGCACTCGCGCACTGGTGGTTGTGACCGCTTGGTGGTTGTGTTGTCTGTTGTCTGGTGGTTGTGTTGTCTGTTGTCTCGCGCACGGGTTGGTGGTGGTTGTGTTGTCTGTTGTCTGGTGGTTGTGACCGCTTGTTGTCTCGCGCACTGACCGCTTGGTGGTTGTGGTGGTTGTGGTGGTTGTGACCGCTGGGTTGGTCGCGCACTGTTGTCTGTTGTCTGACCGCTTGTTGTCTGGTGGTTGTGTTGTCGGGTTGGTGACCGCTTGGTGGTTGTGTTGTCTGACCGCTGGGTTGGGGGTTGGTGGTGGTTGTGTTGTCGGGTTGGTGGTGGTTGTGACCGCTTGTTGTCGGGTTGGTGTTGTCTGTTGTCTGACCGCTTCGCGCACTCGCGCACTCGCGCACTGTTGTCTGGTGGTTG")
# k = 11
# FrequentWords(Text, k)