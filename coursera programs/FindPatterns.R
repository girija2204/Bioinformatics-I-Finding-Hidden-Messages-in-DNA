FindPatterns = function(pattern, text){
  k = nchar(pattern)
  positions = vector()
  count = 0
  for (i in 1:(nchar(text)-k+1)) {
    if(substr(text, i, i+k-1) == pattern){
      positions = append(positions, i)
      count = count + 1
    }
  }
  xx = paste((positions-1), collapse = " ")
  print(xx)
  print(count)
}

text = "GACGATATACGACGATA" 
pattern = "ATA"
FindPatterns(pattern, text)
