loadMat = function(fileName)
{
  library(R.matlab)
  rawData = readMat(fileName)
  data = rawData[[1]]
  matrix = do.call(rbind, data['data',,])
  totalSec = unlist(data['data.length.sec',,])
  sequence = unlist(data['sequence',,])
  measures = as.data.frame(t(matrix))
  measures
}