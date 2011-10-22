cd c:\temp
set TMPDIR=c:\temp
R CMD build stanford.machine.learning
R CMD check stanford.machine.learning
R CMD INSTALL stanford.machine.learning

PAUSE