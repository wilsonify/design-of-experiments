# Summary: C10-Response-Surface-Designs



CHAPTER 10 Response Surface Designs 10.1 Introduction In a response surface experiment, the independent variables or factors can be varied over a continuous range. 

The goal is to determine the factor settings that produce a maximum or minimum response or to map the relationship between the response and the factor settings over this contiguous factor space. 

As a practical matter, if we want to know a lot about the relationship between the factors and the response, it will require many experiments. 

For that reason, response surface designs are rarely conducted with more than six factors. 

Response surface experiments are normally used at the last stage of experimentation. 

The important factors have already been determined in earlier experiments, and at this stage of experimentation the purpose is to describe in detail the relationship between the factors and the response. 

It is usually known or assumed that a simple linear model, even with interactions, is not good enough to represent that relationship. 

In order to locate maximums or minimums in the response as a function of the factor settings, at least three levels of each factor should be utilized.

## Key Formulas

- `factor settings (x’s) is assumed to be a nonlinear equation given byy=f(x)+ϵ.`
- `y =f(x1,x 2)+ϵ (10.1)`
- `y =β0+β1x1+β2x2+β11x2`
- `where β1 = ∂f(x1,x2)`
- `βijxixj+ϵ, (10.4)`
- `y= xb+ x′Bx+ϵ (10.5) where x′= (1,x 1,x 2,...,x k), b′= (β0,β 1,...,β k), and the symmetric matrix`
- `β11 β12/slash.left2/uni22EFβ1k/slash.left2 β22 /uni22EFβ2k/slash.left2`
- `When ﬁtting a linear regression model of the form y = xb, the design points are chosen to minimize the variance of the ﬁtted coefficients ˆb= (X′X)−1X′y.`
- `σ2(X′X)−1, this means the design points should be chosen such that the`
- `predicted value atx, that is given by the equationVar [ˆy(x)]=σ2x′(X′X)−1x`
- `points (α in coded units) equal to`
- `are found as (actual level−center value)/(half-range). 

For example, x2 =`
- `solving actual level=(half-range)×xi+center value.`
- `composite design uniform precision for various values ofk =number of factors.`
- `average scaled variance of a predicted value ( NVar (ˆy(x))/slash.leftσ2) as a function`
- `> rotd <- ccd(3, n0 = c(4,2), alpha = "rotatable", + randomize = FALSE)`
- `y =β0+β1x1+β2x2+β3x3+β12x1x2+β13x1x3+β23x2x3 (10.6)`
- `Whereas the CCD requires ﬁve levels (−α, −1, 0,+1, +α) for each factor, Box`
- `practical use. 

This is true for Box-Behnken designs with k = 3 to 6.`
- `Var (ˆy(x))/slash.leftσ2, on the vertical axis versus the fraction of points in the de-`

## R Functions

library(, ccd(, plot(, pick(, list(, head(, bbd(, data(, transform(, sample(, optFederov(, quad(, seq(, rep(, exp(, frame(, rsm(, anova(, summary(, nls(, contour(, persp(, xs(, steepest(, left(

