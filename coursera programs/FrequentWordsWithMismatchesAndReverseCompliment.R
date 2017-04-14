source("/home/giri/Downloads/MTech Thesis/coursera programs/FrequentWordsWithMismatches.R")
source("/home/giri/Downloads/MTech Thesis/coursera programs/Suffix_And_FirstSymbol.R")

FrequentWordsWithMismatchesAndReverseCompliment = function(text, k, d){
  sum = 0
  Freq_patterns_with_reverse = list()
  FrequentPatterns = FrequentWordsWithMismatches(text, k, d)
  for (i in 1:length(FrequentPatterns)) {
    complementary = GenerateComplimentaryStrand(FrequentPatterns[i])
    count = ApproxPatternCount(FrequentPatterns[i], text, d)
    count_reverse = ApproxPatternCount(complementary, text, d)
    if((count + count_reverse) > sum){
      sum = count + count_reverse
      Freq_patterns_with_reverse = c(FrequentPatterns[i], complementary)
    }
  }
  return(Freq_patterns_with_reverse)
}

text = "aatgatgatgacgtcaaaaggatccggataaaacatggtgattgcctcgcataacgcggtatgaaaatggattgaagcccgggccgtggattctactcaactttgtcggcttgagaaagacctgggatcctgggtattaaaaagaagatctatttatttagagatctgttctattgtgatctcttattaggatcgcactgcccTGTGGATAAcaaggatccggcttttaagatcaacaacctggaaaggatcattaactgtgaatgatcggtgatcctggaccgtataagctgggatcagaatgaggggTTATACACAactcaaaaactgaacaacagttgttcTTTGGATAActaccggttgatccaagcttcctgacagagTTATCCACAgtagatcgcacgatctgtatacttatttgagtaaattaacccacgatcccagccattcttctgccggatcttccggaatgtcgtgatcaagaatgttgatcttcagtg"
k = 9
d = 1
print(FrequentWordsWithMismatchesAndReverseCompliment(text, k, d))