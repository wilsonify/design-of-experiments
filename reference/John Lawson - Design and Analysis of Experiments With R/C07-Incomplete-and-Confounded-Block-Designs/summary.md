# Summary: C07-Incomplete-and-Confounded-Block-Designs

CHAPTER 7 Incomplete and Confounded Block Designs 7.1 Introduction One of the two purposes for randomized block designs, described in Chapter 4, was to group heterogeneous experimental units together in homogeneous subgroups called blocks. 

This increases the power or precision for detecting differences in treatment groups. 

The overall F -test for comparing treatment means, in a randomized block design, is a ratio of the variability among treatment means to the variability of experimental units within the homogeneous blocks. 

One restriction on randomized block designs is that the number of experimental units in the block must be greater than or equal to the number of levels of the treatment factor. 

For the RCB design the number of experimental units per block, t, is equal to the number of levels of the treatment factor, and for the GCB design the number of experimental units per block is equal to the number of levels of the treatment factor times the number of replicates per block, tr. 

When the number of levels of the treatment factor is large in a randomized block design, the corresponding number of experimental units per block must also be large. 

This could cause a problem.

## Key Formulas

- `taste panel described above, if there were t = 6 recipes to be tested, and it was determined that each subject could taste at most k = 3 recipes without`
- `3/parenright.alt1= 20 subjects would be required. 

All possible`
- `in that each treatment level or recipe is replicated r = 10 times (or tasted by`
- `block λ= 4 times. 

For example, treatment levels 1 and 2 occur together only`
- `incomplete block design, λ is the number of times each treatment level occurs`
- `tr =bk (7.2) λ(t−1) =r(k−1) (7.3)`
- `for a BIB design. 

Since r and λ must be integers, by Equation (7.3) we see that λ(t −1) must be divisible by k−1. 

If t = 6 and k = 3, as in the taste test panel, 5λ must be divisible by 2. 

The smallest integer λ for which this is satisﬁed is λ = 2. 

Therefore, it may be possible to ﬁnd a BIB with λ = 2, r = 10/slash.left2= 5, and b = (6×5)/slash.left3= 10. 

The function BIBsize in the R package daewr provides a quick way of ﬁnding values of λ and r to satisfy Equations`
- `Posible BIB design with b= 10 and r= 5 lambda= 2`
- `for some combination of t,b,r,λ, and k, a corresponding BIB may not exist.`
- `BIB designs. 

For example, the code below searches for a BIB with b = 10 blocks of k = 3 experimental units per block and t= 6 levels of the treatment factor. 

The option blocksizes=rep(3,10) speciﬁes 10 blocks of size 3, and the option withinData=factor(1:6) speciﬁes six levels of one factor.`
- `> BIB <- optBlock( ~ ., withinData = factor(1:6), + blocksizes = rep(3, 10))`
- `> des <- matrix(des, nrow = 10, ncol = 3, byrow = TRUE, + dimnames = list(c( "Block1", "Block2", "Block3", "Block4",`
- `be seen that each level of the treatment factor is repeated r = 5 times in this`
- `other treatment levelλ= 2 times. 

Thus, we are assured that this is a balanced`
- `yij =µ+bi+τj+ϵij, (7.4)`
- `(1988), shown in Table 7.1. 

This experiment is a BIB with t=4 levels of the treatment factor or recipe, and block size k=2. 

Thus each panelist tastes only`
- `2/parenright.alt1= 6`
- `ment means are not unbiased estimators of the estimable effects µ+τi. 

For`
- `> mod1 <- aov( score ~ panelist + recipe, data = taste)`
- `The F3,9 value for testing recipes was 3.982, which is signiﬁcant at the α=`

## R Functions

library(, rep(, factor(, optBlock(, dim(, matrix(, list(, aov(, summary(, lsmeans(, cyclic(, response(, lm(, coef(, na(, halfnorm(, names(, factorial(, mod(, cbind(, design(, numeric(, order(, oas(


