library(rmake)

infile <- getParam('.depends', 'rcode.tex')

print(infile)

cont <- readLines(infile)
starts <- grep('^% --begin: ', cont)
ends <- grep('^% --end: ', cont)

chunkNames <- sub('^.*"(.*)".*$', '\\1', cont[starts])
starts <- starts - 1
ends <- ends + 1

if (length(starts) != length(ends) || length(chunkNames) != length(starts)) {
  stop('different length of starts, ends or chunkNames')
}

log <- c()
for (i in seq_along(chunkNames)) {
  chunk <- cont[seq(starts[i], ends[i])]
  outfile <- paste0('chunks/chunk-', chunkNames[i], '.tex')
  writeLines(chunk, outfile)
  log <- c(log, chunkNames[i])
}

writeLines(log, 'exportChunks.log')
