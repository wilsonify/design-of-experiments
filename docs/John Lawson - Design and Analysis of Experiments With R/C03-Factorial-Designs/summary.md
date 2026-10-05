# Summary: C03-Factorial-Designs


In Chapter 2 we examined one-factor designs. 

These are useful only when one factor is under study. 

When multiple factors are under study, one classical approach is to study each separately while holding all others constant. 

Fisher (1935) pointed out that this approach is useful for demonstrating known relationships to students in laboratory courses when the inﬂuence of other factors is known. 

However, this approach is both inefficient and potentially misleading when it comes to discovering new knowledge through experimentation. 

A much better strategy for experimenting with multiple factors is to use a factorial design. 

In a factorial design the cells consist of all possible combinations of the levels of the factors under study. 

Factorial designs accentuate the factor effects, allow for estimation of interdependency of effects (or interactions), and are the ﬁrst technique in the category of what is called treatment design. 

By examining all possible combinations of factor levels, the number of replicates of a speciﬁc level of one factor is increased by the product of the number of levels of all other factors in the design, and thus the same power or precision can be obtained with

## Key Formulas

- `is more efficient since it requires only 2 ×16 = 32 total runs as opposed to`
- `replicates of each factor level is 2×4= 8. 



In the factorial plan, the 32 treatment`
- `four factors at two levels, or 2 4 = 16 cells. 



If two replicates were run for each cell, there would be a total of 2×16= 32 experiments or runs. 



To examine the effect of any one of the four factors, half the runs (or 2 ×23 = 16 due to the`
- `would result in 4×16+ 16= 80 experiments, or 2.5 times the number required`
- `> D <- expand.grid( BW = c(3.25, 3.75, 4.25), WL = c(4, 5, 6) )`
- `Body width = BW and Wing length = WL with the supplied levels for these`
- `> write.csv(CopterDes, file = "CopterDes.csv", row.names = FALSE)`
- `yijk =µij+ϵijk, (3.1)`
- `yijk =µ+αi+βj+αβij+ϵijk. 



(3.2) In this model, αi,βj are the main effects and represent the difference between`
- `interaction effects, αβij, represent the difference between the cell mean, µij, andµ+αi+βj. 



With these deﬁnintions, ∑iαi = 0, ∑jβj = 0, ∑iαβij = 0, and ∑jαβij = 0.`
- `ϵijk ∼N(0,σ 2). 



The independence assumption is guaranteed if the treatment`
- `µij = µ+αi +βj +αβij, are estimable functions but the individual effects, αi,βj, andαβij are not estimable functions. 



Contrasts among the effects such as ∑iciαi and ∑jcjβj, where ∑ici = 0, ∑jcj = 0 are estimable only in the additive model where all αβij’s are zero. 



Contrasts of the form ∑i ∑jbijαβij, where ∑ibij = 0, ∑jbij = 0 are estimable even in the non-additive model.`
- `µ+αi+αβi⋅and µ+βj +αβ⋅jare estimable functions and they and the cell`
- `y =Xβ +ϵ= /parenleft.alt11 /divides.alt0XA /divides.alt0XB /divides.alt0XAB /parenright.alt1`
- `(X′X)−1X′y = ˆβ =`
- `ˆµ+ ˆα1+ ˆβ1+ ˆαβ11 ˆα2−ˆα1+ ˆαβ21−ˆαβ11 ˆβ2−ˆβ1+ ˆαβ12−ˆαβ11 ˆβ3−ˆβ1+ ˆαβ13−ˆαβ11 ˆαβ11+ ˆαβ22−ˆαβ12−ˆαβ21 ˆαβ11+ ˆαβ23−ˆαβ13−ˆαβ21`
- `The error sum of squares ssE =y′y−ˆβ′X′y=y′(I−X(X′X)−1X′)y, where ˆβ = (X′X)−1X′y are the estimates produced by the lm function in R. 



To test the hypothesis H0 ∶α1 = α2 = 0,H 0 ∶β1 = β2 = β3 = 0, and H0 ∶αβ11 = αβ21 =αβ12 =αβ22 =αβ13 =αβ23 = 0, the likelihood ratioF -tests are obtained`
- `as the sums of squares for factor A is ssA = ˆβ′X′y −(1′y)2/slash.left(1′1), where`
- `is,X = /parenleft.alt11 /divides.alt0XA /parenright.alt1. 



The error sums of squares for this simpliﬁed model is ssEA. 



The sums of squares for factor A is denoted R(α/divides.alt0µ). 



The sums of squares for factor B is denoted R(β/divides.alt0α,µ)=ssEA−ssEB wheressEB is the error sums of squares from the reduced model where X = /parenleft.alt11 /divides.alt0XA /divides.alt0XB /parenright.alt1. 



Finally, the sums of squares for the interaction AB is denoted R( αβ/divides.alt0β,α,µ)=ssEB−`
- `A a−1 R(α /divides.alt0µ)ssA (a−1) F = msA`

## R Functions

grid(, rbind(, seed(, order(, sample(, csv(, left(, ab(, library(, aov(, summary(, tables(, cbind(, list(, estimable(, with(, plot(, options(, lm(, frame(, lapply(, predict(, tapply(, lsmeans(, data(

