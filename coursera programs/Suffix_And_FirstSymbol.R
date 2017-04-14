Suffix_And_FirstSymbol = function(pattern){
  fs = substr(pattern, 1, 1)
  suffix = substr(pattern, 2, nchar(pattern))
  return_values = list(fs, suffix)
  return(return_values)
}

# pattern = "ACCFACGT"
# fs = Suffix_And_FirstSymbol(pattern)
# print(fs)