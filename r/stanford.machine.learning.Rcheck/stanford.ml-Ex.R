pkgname <- "stanford.ml"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('stanford.ml')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("stanford.machine.learning-package")
### * stanford.machine.learning-package

flush(stderr()); flush(stdout())

### Name: stanford.machine.learning-package
### Title: Provides code to accompany Stanford CS229 lectures on "Machine
###   Learning".
### Aliases: stanford.machine.learning-package stanford.machine.learning
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
