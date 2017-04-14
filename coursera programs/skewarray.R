skew = function(text){
  length = nchar(text)
  skew_text = vector(mode = "integer", length = length + 1)
  for (i in 1:length) {
    if(substr(text, i, i) == "C"){
      skew_text[i+1] = skew_text[i] - 1
    }
    else if(substr(text, i, i) == "G"){
      skew_text[i+1] = skew_text[i] + 1
    }
    else{
      skew_text[i+1] = skew_text[i]
      
    }
  }
  for (i in 1:length(skew_text)) {
    if(skew_text[i]<0){
      #print(i-1)
    }
  }
  return(skew_text)
}

text = "CATTCCAGTACTTCATGATGGCGTGAAGA"
skew_array = skew(text)
max = 0
position = 0
print(skew_array)
for (i in 2:length(skew_array)) {
  if(skew_array[i]>=max)
  {
    max = skew_array[i]
    position = i
  }
}
print(position-1)