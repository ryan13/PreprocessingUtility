# http://stackoverflow.com/questions/12112558/read-write-data-in-libsvm-format
# other option to write csv into libsvm format

library(data.table)

data.table.fm <- function (data = X, fileName = "../out.fm", target = "y_train", 
                           train = TRUE) {
  if (train) {
    if (is.logical(data[[target]]) | sum(levels(factor(data[[target]])) == 
                                           levels(factor(c(0, 1)))) == 2) {
      data[[target]][data[[target]] == TRUE] = 1
      data[[target]][data[[target]] == FALSE] = -1
    }
  }
  specChar = "\\(|\\)|\\||\\:"
  specCharSpace = "\\(|\\)|\\||\\:| "
  parsingNames <- function(x) {
    ret = c()
    for (el in x) ret = append(ret, gsub(specCharSpace, "_", 
                                         el))
    ret
  }
  parsingVar <- function(x, keepSpace, hard_parse) {
    if (!keepSpace) 
      spch = specCharSpace
    else spch = specChar
    if (hard_parse) 
      gsub("(^_( *|_*)+)|(^_$)|(( *|_*)+_$)|( +_+ +)", 
           " ", gsub(specChar, "_", gsub("(^ +)|( +$)", 
                                         "", x)))
    else gsub(spch, "_", x)
  }
  setnames(data, names(data), parsingNames(names(data)))
  target = parsingNames(target)
  format_vw <- function(column, formater) {
    ifelse(as.logical(column), sprintf(formater, j, column), 
           "")
  }
  all_vars = names(data)[!names(data) %in% target]
  cat("Reordering data.table if class isn't first\n")
  target_inx = which(names(data) %in% target)
  rest_inx = which(!names(data) %in% target)
  cat("Adding Variable names to data.table\n")
  for (j in rest_inx) {
    column = data[[j]]
    formater = "%s:%f"
    set(data, i = NULL, j = j, value = format_vw(column, 
                                                 formater))
    cat(sprintf("Fixing %s\n", j))
  }
  data = data[, c(target_inx, rest_inx), with = FALSE]
  drop_extra_space <- function(x) {
    gsub(" {1,}", " ", x)
  }
  cat("Pasting data - Removing extra spaces\n")
  data = apply(data, 1, function(x) drop_extra_space(paste(x, 
                                                           collapse = " ")))
  cat("Writing to disk\n")
  write.table(data, file = fileName, sep = " ", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)
}
