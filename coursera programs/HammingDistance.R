HammingDist = function(text1, text2){
  length = nchar(text1)
  count = 0
  for (i in 1:length) {
    if(substr(text1, i, i) != substr(text2, i, i))
      count = count + 1
  }
  return(count)
}

# text1 = "CAGAAAGGAAGGTCCCCATACACCGACGCACCAGTTTA"
# text2 = "CACGCCGTATGCATAAACGAGCCGCACGAACCAGAGAG"
# hd = HammingDist(text1, text2)
# print(hd)