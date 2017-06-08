########## LINEARE REGRESSION - BASICS
library(tidyverse)
library(broom)
library(DAAG)


## Modell Erstellung
# ?forumla
# a:b = Interaction
# a*b = a + b + a:b
# I() = arithmetisch => I(a+b) = Summe von a und b
model <- lm(formula = mpg ~ disp + hp, data=mtcars)

## Kategorsiche Variable
mtcars$cyl <- as.factor(mtcars$cyl)
# Referenz-level setzen
# (wird bei der dummyfizierung die nicht existente variable)
mtcars$cyl <- relevel(mtcars$cyl, ref = "4")
model <- lm(formula = mpg ~ disp + hp + cyl, data=mtcars)


## Volle Modell-Zusammenfassung
summary(model)                    # Modell zusammenfassung - einzeln... 

## Model Fit
model_summary <- summary(model)
model_summary$r.squared           # R^2
model_summary$adj.r.squared       # R^2_adj
cv.lm(mtcars, model, m=3)         # Cross-Validation


## Modell-Informationen
coef(model)                     # Koeffizienten
confint(model, level=0.95)      # Konfidenzintervalle
fitted(model)                   # Vohergesagte Werte
residuals(model)                # Residuen
anova(model)                    # ANOVA Tabelle
vcov(model)                     # Kovarianz-Matrix der Modell-Parameter
influence(model)                # Modell-Diagnostik

## Broom
model %>% tidy()                # Koeffizienten + statistiken als dataframe
model %>% augment()             # Beobachtungen mit fitted values, residuals,...
# => gut um z.B. Outlier über cooks distance zu finden

## Diagnostik-Plots
layout(matrix(c(1,2,3,4),2,2))
plot(model)

### Vorhersage
new_observations = data.frame(disp = 160)
predict(model,newdata = new_observations)
predict(mmodelod) 

