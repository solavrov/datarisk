cat("\nEnter FB password > ")
pwd <- readLines("stdin", n=1)
datarisk::main.refresh(pwd)
cat("\nPress any key...")
readLines("stdin", n=1)

#test
