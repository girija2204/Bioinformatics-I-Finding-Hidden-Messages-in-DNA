dna = c("A", "C", "G", "T")
LastSymbol = function(text){
  return(substr(text, nchar(text), nchar(text)))
}
Prefix = function(text){
  return(substr(text, 1, (nchar(text)-1)))
}
SymbolToNumber = function(symbol){
  x = 0
  for(i in 1:4){
    if(dna[i] == symbol){
      x = i
    }
  }
  return(x-1)
}
PatternToNumber = function(text){
  if(nchar(text) == 0)
    return(0)
  ls = LastSymbol(text)
  prefix = Prefix(text)
  sn = SymbolToNumber(ls)
  return((4*PatternToNumber(prefix))+sn)
}

# text = "AA"
# pn = PatternToNumber(text)
# print(pn)

