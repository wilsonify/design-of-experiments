# Summary: lab4a-twilson

## Overview

- **Source**: lab4a-twilson
- **Length**: 9,300 characters, 604 lines, 1 paragraphs
- **Sections**: 0 headings detected

## Introduction

- Stat 5309 Lab 4a Tom Wilson Feb 20, 2019 1.

## Key Concepts

Stat 5309 Lab 4a Tom Wilson Feb 20, 2019 1. A chemist wishes to test the effect of four chemical agents on the strength of a particular type of cloth. Because there might be variablility from one bolt to another, the chemist decides to use a randomized block design, with the bolts of cloth considered as blocks. She selects ﬁve bolts and applies all four chemicals in random order to each bolt. The resulting tensile strengths follow. Analyze the data from this experiment ( use α = 0.05 ) and draw appropriate conclusions. a Create a vector for Blocks, named “Bold”: 5 levels. Total 20. Create a vector for Treatments, named “Chemical”. Total 20. Create a response vector, named “Strength”. Set up the data frame named “chem”. bolts <- c("b1","b2","b3","b4","b5") chemicals <- c("c1","c2","c3","c4") chem_data <- expand.grid(bolt = bolts,chemical = chemicals) chem_data <- cbind(chem_data,strength = c(73,68,74,71,67, 73,67,75,72,70, 75,68,78,73,68, 73,71,75,75,69)) chem_data %>% kable() bolt chemical strength b1 c1 b2 c1 b3 c1 b4 c1 b5 c1 b1 c2 b2 c2 b3 c2 b4 c2 b5 c2 b1 c3 b2 c3 b3 c3 b4 c3 b5 c3 b1 c4 b2 c4 b3 c4 b4 c4 b5 c4 b Any evidence that the Chemical

## Key Formulas

- `chem_data <- expand.grid(bolt = bolts,chemical = chemicals) chem_data <- cbind(chem_data,strength = c(73,68,74,71,67,`
- `boxplot(strength~chemical,data=chem_data)`
- `strength_model <- aov(strength~chemical+bolt,data = chem_data) TukeyHSD(strength_model, conf.level=0.95)`
- `## Fit: aov(formula = strength ~ chemical + bolt, data = chem_data)`
- `bacteria_data <- expand.grid(solution = solutions, day = days) bacteria_data <- cbind(bacteria_data, growth = c(13,22,18,39,`
- `boxplot(growth~solution,data=bacteria_data)`
- `growth_model <- aov(growth~solution+day,data = bacteria_data) TukeyHSD(growth_model, conf.level=0.95)`
- `## Fit: aov(formula = growth ~ solution + day, data = bacteria_data)`
- `aluminum_data <- expand.grid(stir_rate = stir_rates,furnace = furnaces) aluminum_data <- cbind(aluminum_data, grain_size = c(8,4,5,6,`
- `boxplot(grain_size~stir_rate,data=aluminum_data)`
- `grain_model <- aov(grain_size~stir_rate+furnace,data=aluminum_data)`
- `plot(x=aluminum_data$furnace,y=grain_model$residuals)`
- `TukeyHSD(grain_model, conf.level=0.95)`
- `## Fit: aov(formula = grain_size ~ stir_rate + furnace, data = aluminum_data)`

## R Functions

grid(, cbind(, kable(, boxplot(, plot(, aov(, qqnorm(, qqline(

