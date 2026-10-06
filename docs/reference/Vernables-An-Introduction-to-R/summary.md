# Summary: Vernables-An-Introduction-to-R

## Overview

- **Source**: Vernables-An-Introduction-to-R
- **Length**: 261,900 characters, 5,366 lines, 1 paragraphs
- **Sections**: 15 headings detected

## Introduction

- An Introduction to R Notes on R: A Programming Environment for Data Analysis and Graphics Version 3.3.1 (2016-06-21) W.

## Section Outline

- 1 Introduction and preliminaries
- 1.1 The R environment
- 1.2 Related software and documentation
- 1.3 R and statistics
- Chapter 1: Introduction and preliminaries
- 1.4 R and the window system
- 1.5 Using R interactively
- 1. Create a separate sub-directory, say work, to hold data files on which you will use R for
- 2. Start the R program with the command
- 3. At this point R commands may be issued (see later).
- 4. To quit the R program the command is
- 1. Make work the working directory and start the program as before:
- 2. Use the R program, terminating with the q() command at the end of the session.
- 1.6 An introductory session
- 1.7 Getting help with functions and features
- ... and more sections

## Key Concepts

An Introduction to R Notes on R: A Programming Environment for Data Analysis and Graphics Version 3.3.1 (2016-06-21) W. Venables, D. Smith and the R Core Team This manual is for R, version 3.3.1 (2016-06-21). Permission is granted to make and distribute verbatim copies of this manual provided Permission is granted to copy and distribute modified versions of this manual under the conditions for verbatim copying, provided that the entire resulting derived work is distributed under the terms of a permission notice identical to this one. Permission is granted to copy and distribute translations of this manual into an- other language, under the above conditions for modified versions, except that this permission notice may be stated in a translation approved by the R Core Team. i Table of Contents Preface. 1 Introduction and preliminaries. 2 1.1 The R environment . 2 1.2 Related software and documentation . 2 1.3 R and statistics . 2 1.4 R and the window system . 3 1.5 Using R interactively. 3 1.6 An introductory session . 4 1.7 Getting help with functions and features . 4 1.8 R commands, case sensitivity, etc. 4 1.9 Recall and correction of previous commands.

## Key Formulas

- `the value of the expression. In most contexts the ‘=’ operator can be used as an alternative.`
- `from=value and to=value; thus seq(1,30), seq(from=1, to=30) and seq(to=30, from=1) are all the same as 1:30. The next two arguments to seq() may be named by=value and length=value, which specify a step size and a length for the sequence respectively. If neither of these is given, the default by=1 is assumed.`
- `> seq(-5, 5, by=.2) -> s3`
- `> s4 <- seq(length=51, from=-5, by=.2)`
- `The fifth argument may be named along=vector, which is normally used as the only argu-`
- `> s5 <- rep(x, times=5)`
- `> s6 <- rep(x, each=5)`
- `The logical operators are <, <=, >, >=, == for exact equality and != for inequality. In addition`
- `Notice that the logical expression x == NA is quite different from is.na(x) since NA is not really a value but a marker for a quantity that is not available. Thus x == NA is a vector of the`
- `using \ as the escape character, so \\ is entered and printed as \\, and inside double quotes " is entered as \". Other useful escape sequences are \n, newline, \t, tab and \b, backspace—see`
- `changed by the named argument, sep=string, which changes it to string, possibly empty.`
- `> labs <- paste(c("X","Y"), 1:10, sep="")`
- `3 paste(..., collapse=ss) joins the arguments into a single character string putting ss in between, e.g., ss`
- `> c("x","y")[rep(c(1,2,2,1), times=4)]`
- `2 = 24 entries in a and the data vector holds them in the order a[1,1,1], a[2,1,1], ...,`
- `> x <- array(1:20, dim=c(4,5))`
- `> i <- array(c(1:3,3:1), dim=c(3,2))`
- `> Z <- array(h, dim=c(3,4,2))`
- `example if we wished to evaluate the function f(x; y) = cos(y)/(1 + x2) over a regular grid of`
- `> plot(as.numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency")`

## R Functions

tapply(, array(, cbind(, rbind(, attach(, detach(, table(, scan(, glm(, plot(, par(, help(, start(, example(, source(, sink(, objects(, ls(, rm(, assign(, min(, max(, length(, sum(, prod(

