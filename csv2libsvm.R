# http://stackoverflow.com/questions/12112558/read-write-data-in-libsvm-format
# write csv into libsvm
# a train={TRUE, FALSE} argument in case the data doesn't have labels. 
# In this case, the class index is ignored.

write.libsvm = function(data, filename= "out.dat", class = 1, train=TRUE) {
  out = file(filename)
  if(train){
    writeLines(apply(data, 1, function(X) {
      paste(X[class], 
            apply(cbind(which(X!=0)[-class], 
                        X[which(X!=0)[-class]]), 
                  1, paste, collapse=":"), 
            collapse=" ") 
    }), out)
  } else {
    # leaves 1 as default for the new data without predictions. 
    writeLines(apply(data, 1, function(X) {
      paste('1',
            apply(cbind(which(X!=0), X[which(X!=0)]), 1, paste, collapse=":"), 
            collapse=" ") 
    }), out)
  }
  close(out) 
}