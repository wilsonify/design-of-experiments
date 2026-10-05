# Summary: Everitt-Handbook-of-Statistical-Analyses-Using-R

A Handbook of Statistical Analyses Using R Brian S. Everitt and Torsten Hothorn Preface 

This book is intended as a guide to data analysis with the R system for statistical computing. 

R is an environment incorporating an implementation of the S programming language, which is powerful, ﬂexible and has excellent graphical facilities (R Development Core Team, 2005). 

In the Handbook we aim to give relatively brief and straightforward descriptions of how to conduct a range of statistical analyses using R. 

Each chapter deals with the analysis appropriate for one or several data sets. 

A brief account of the relevant statistical background is included in each chapter along with appropriate references, but our prime focus is on how to use R and how to interpret results. 

We hope the book will provide students and researchers in many disciplines with a self-contained means of using R to analyse their data. 

R is an open-source project developed by dozens of volunteers for more than ten years now and is available from the Internet under the General Public Licence. 

R has become the lingua franca of statistical computing. 

Increasingly, implementations of new statistical methodology ﬁrst appear as R add-on packages.

## Key Formulas

- `R> vignette("Ch_introduction_to_R", package = "HSAUR")`
- `R> edit(vignette("Ch_introduction_to_R", package = "HSAUR"))`
- `R> vignette(package = "HSAUR")`
- `> options(prompt = "R> ")`
- `R> help(package = "sandwich")`
- `R> data("Forbes2000", package = "HSAUR")`
- `R> seq(from = 1, to = 3, by = 1)`
- `R> csvForbes2000 <- read.table("Forbes2000.csv", header = TRUE,`
- `sep = ",", row.names = 1) The argument header = TRUE indicates that the entries in the ﬁrst line of the`
- `are separated by a comma (sep = ","), users of continental versions of Excel`
- `dec = "."). Finally, the ﬁrst column should be interpreted as row names but not as a variable (row.names = 1). Alternatively, the function read.csv can`
- `sep = ",", row.names = 1, colClasses = c("character",`
- `R> sqlQuery(cnct, "select * from \"Forbes2000$\"")`
- `R> write.table(Forbes2000, file = "Forbes2000.csv",`
- `sep = ",", col.names = NA)`
- `R> save(Forbes2000, file = "Forbes2000.rda")`
- `R> list.files(pattern = ".rda")`
- `R> UKcomp <- subset(Forbes2000, country == "United Kingdom")`
- `median, na.rm = TRUE)`
- `and supply them to the median function with additional argument na.rm =`

## R Functions

packages(, library(, vignette(, edit(, license(, licence(, contributors(, citation(, demo(, help(, start(, options(, sqrt(, print(, data(, ls(, str(, class(, dim(, nrow(, ncol(, names(, length(, seq(, nlevels(

## R Packages

HSAUR, sandwich, RODBC, vcd, coin, KernSmooth, mclust, flexmix, boot, rpart, randomForest, party, lattice, survival, lme4, gee, rmeta, MASS, ape, scatterplot3d

## Figures

- Figure 1.1 ﬁrst divides the plot region into two equally
- Figure 1.2) is rather uninformative due to areas
- Figure 1.3. If the independent variable is a factor, a boxplot repre-
- Figure 1.4.
- Figure 2.1. The layout function (line 1 in
- Figure 2.1) divides the plotting area in three parts. The boxplot function
- Figure 2.5.
- Figure 2.8. In line 1 of Figure 2.8, we
- Figure 2.11 depicts
- Figure 2.12 is 62.888 with a single
- Figure 2.13).
- Figure 2.10

## Tables

- Table 2.3) was analysed in Chapter 2 and here we will extend the
