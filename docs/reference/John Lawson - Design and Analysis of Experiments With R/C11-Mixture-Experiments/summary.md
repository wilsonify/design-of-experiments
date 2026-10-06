# Summary: C11-Mixture-Experiments


CHAPTER 11 Mixture Experiments 11.1 Introduction Many products, such as textile ﬁber blends, explosives, paints, polymers, and ceramics are made by mixing or blending two or more components or ingredients together. 

For example, a cotton-poly fabric is made by mixing cotton and polyester ﬁbers together. 

The characteristics of a product that is composed of a mixture of components is usually a function of the proportion of each component in the mixture and not the total amount present. 

If the proportion of the ith component is xi, and there are k components in a mixture, then the proportions must satisfy the constraints 0.0≤xi ≤1.0, for each component, and k /summation.disp i=1 xi = 1.0. 

(11.1) For example, in a three-component mixture, 0 .0 ≤x1 ≤1.0, 0.0 ≤x2 ≤1.0, 0.0≤x3 ≤1.0, and x1+x2+x3 = 1.0. 

If an experiment is conducted by varying the mixture components in an attempt to determine their effect on the product characteristics, the constraints prevent using standard factorial or response surface experimental design. 

If each component in the mixture can range from 0.0 to 100.0% of the total, a 23 factorial experiment would consist of all possible combinations of proportions 0.00 and 1.00 resulting in the

## Key Formulas

- `0.0≤xi ≤1.0, for each component, and`
- `xi = 1.0. 

(11.1) For example, in a three-component mixture, 0 .0 ≤x1 ≤1.0, 0.0 ≤x2 ≤1.0, 0.0≤x3 ≤1.0, and x1+x2+x3 = 1.0.`
- `However, the constraintx1+x2+x3 = 1.0 reduces the three-dimensional ex-`
- `tal line where component 1 is constant at x1 = 0.166, component 2 varies from x2 = 0.833 at the left, where the line touches the component 2 axis, tox2 = 0.0,`
- `component 3 at any point along this line wherex1 = 0.166 is equal to 1−x1−x2`
- `by planes parallel to the base where x1 = 0.0. 

Likewise, constant proportions`
- `y =β0+β1x1+β2x2+β3x3+ϵ. 

(11.2) However, in a mixture experiment, the constraint x1+x2+x3 = 1.0 makes`
- `periments. 

In his form of the model, the coefficient for β0 in Equation (11.2)`
- `From this point on, the asterisks will be removed from the β∗`
- `characteristics. 

Alternately, in the mixture model βi represents the predicted response at the vertex of the experimental region where xi = 1.0. 

This can be`
- `y =β0+β1x1+β2x2+β3x3+β1x2`
- `+β13x1x3+β23x2x3+ϵ, (11.5)`
- `is also different for mixture experiments. 

Multiplying β0 by x1+x2+x3 and`
- `y =β1x1+β2x2+β3x3+β12x1x2+β13x1x3+β23x2x3+ϵ. 

(11.6)`
- `βijxixj+ϵ. 

(11.7)`
- `signs in three components. 

Only the pure components (i.e., (x1,x 2,x 3) = (1, 0, 0), (x1,x 2,x 3) = (0, 1, 0), and (x1,x 2,x 3) = (0, 0, 1)) are required for a linear design and the coefficient βi in model 11.4 can be estimated as an average of all the response data at the pure component where xi = 1.0. 

In`
- `ﬁcientsβij of the quadratic blending effects in model 11.5, thus the mixtures (x1,x 2,x 3) = ( 1`
- `2, 0), (x1,x 2,x 3) = ( 1`
- `2), and (x1,x 2,x 3) = (0, 1`
- `x3=1.0 x3=1.0x2=1.0 x2=1.0`

## R Functions

library(, data(, lm(, summary(, optFederov(, optBlock(, rep(, matrix(, crvtave(, frame(, subset(, rbind(, cbind(, wheref(, andg(, components(, grid(, merge(, function(, abs(, constrOptim(, lmer(, transform(, sqrt(, rsm(
