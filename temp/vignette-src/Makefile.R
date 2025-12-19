library(rmake)

job <- c('rcode.Rtex' %>>% knitrRule('rcode.Rtex') %>>%
           'rcode.tex' %>>% rRule('exportChunks.R') %>>% 'exportChunks.log')


makefile(job, "Makefile")
