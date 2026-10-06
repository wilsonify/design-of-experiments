

############   LAB 4  $+##############

type <-rep( c("basalt", "sel"), each=12)
type

knead <- rep(c("stat","reg", "low","verylow", each=3, times=2))

strength<- c( 68, 63, 65,	126,128,133,	93,101,98,56,59, 57,71,66, 66,	107,110,116,63,  60,59,40,  41,   44)	
              
              
###############################################


chem <- read.csv(file.choose(), header=TRUE)
              
chem <- chem[  , -1]
chem              
              
    attach(chem)        
Bolt<- factor(Bolt)              
Chemist <-factor(Chemist)              
              
chem.aov <- aov(Strength ~ Bolt+Chemist )              
summary(chem.aov)              
              
############   Tukey
summary.aov(chem.aov)

TukeyHSD(chem.aov)
    
   
              
