library(readr)
x<- read_csv2("E:/Data/2016.10/White Card/export 33 White card - Apostol Antonia 2016-10-01 - sheet2.csv", col_names = F, na = c("", "NA"))
utils::View(x)
range(x$X4)
x$X4 <- as.Date(x$X4)

ptm <- proc.time()
x<- read_csv2("E:/Data/2016.10/White Card/export 33 White card - Apostol Antonia 2016-10-01 - sheet2.csv", col_names = F, na = c("", "NA"))
proc.time() - ptm

ptm <- proc.time()
y <- read.csv("E:/Data/2016.10/White Card/export 33 White card - Apostol Antonia 2016-10-01 - sheet2.csv",
                                 head = F, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), colClasses = c("integer","numeric","integer","character"))
proc.time() - ptm

str(x)
str(y)

c <-  read_csv2("E:/Data/2016.09/White Card/export 33 White card - Apostol Antonia 2016-09-01-Sheet4.csv",
                               col_names = F)
str(c)