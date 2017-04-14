GenerateComplimentaryStrand = function(text){
  text_split = vector()
  rev_text = array()
  for (i in 1:nchar(text)) {
    text_split[i] = substr(text, i, i)
    rev_text[i] = 0
  }
  for (i in 1:nchar(text)) {
    if(text_split[i] == "A"){
      rev_text[i] = "T"
    }
    else if(text_split[i] == "T")
      rev_text[i] = "A"
    else if(text_split[i] == "G")
      rev_text[i] = "C"
    else
      rev_text[i] = "G"
  }
  rev_text_split = rev_text[nchar(text):1]
  return(paste(rev_text_split, collapse = ""))
}

text = "ACGGGT"
print(GenerateComplimentaryStrand(text))