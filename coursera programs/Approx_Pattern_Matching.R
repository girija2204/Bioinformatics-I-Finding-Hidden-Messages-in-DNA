Approx_pattern_match = function(pattern, text, k){
  array2 = vector()
  count = 0
  for (i in 1:(nchar(text)-k+1)) {
    hd = HammingDist(pattern, substr(text, i, (i + nchar(pattern) - 1)))
    if(hd <= k){
      array2 = append(array2, i-1)
      count = count + 1
    }
  }
  return(array2)
}

pattern = "AGTCTTC"
text = "TTTATCTGTAAGCCGCTGCAGGGAAG"
array2 = vector()
array2 = paste(Approx_pattern_match(pattern, text, 2), collapse = " ")
print(array2)
