library(nlme)

data.small$Tleaf2 = data.small$Tleaf^2

a = lmList(A ~ Tleaf2 + Tleaf | curveID, data = data.small)

# Quadratic model fit with random intercepts
method.lme.1 = lme(A ~ Tleaf + Tleaf2,
                   data = data.small,
                   random = ~ 1 | curveID,
                   na.action = na.omit)

method.lme.2 = lme(A ~ method*Tleaf,
                   data = data.small,
                   random = ~ 1 | curveID,
                   na.action = na.omit)

# This would be our "full model", with effects of method on intercept and both "slopes"
method.lme.3 = lme(A ~ method*Tleaf + method*Tleaf2,
                   data = data.small,
                   random = ~ 1 | curveID,
                   na.action = na.omit,
                   method = "ML")

# So now let's make a reduced model without a method effect on the quadratic term
method.lme.3.noquad = update(method.lme.3,
                             fixed = A ~ method*Tleaf + Tleaf2)

# Here the full model is better
anova(method.lme.3, method.lme.3.noquad)

# Reduced model with no method effect on the linear term
method.lme.3.nolin = update(method.lme.3,
                            fixed = A ~ Tleaf + method*Tleaf2)

# Again, the full model is better it appears
anova(method.lme.3, method.lme.3.nolin)

# Can we make a reduced model with no intercept
method.lme.3.noint = update(method.lme.3,
                            fixed = A ~ method*Tleaf + method*Tleaf2 - method)

# Full model is better
anova(method.lme.3, method.lme.3.noint)


# I'm going to try to fit a model with random effects on each parameter
method.lme.4 = lme(A ~ method*Tleaf + method*Tleaf2,
                   data = data.small,
                   random = ~ Tleaf + Tleaf2 | curveID,
                   na.action = na.omit,
                   method = "ML")

# So now let's make a reduced model without a method effect on the quadratic term
method.lme.4.noquad = update(method.lme.4,
                             fixed = A ~ method*Tleaf + Tleaf2)

# Full model favored by AIC but BIC favors reduced
anova(method.lme.4, method.lme.4.noquad)

# Reduced model with no method effect on the linear term
method.lme.4.nolin = update(method.lme.4,
                            fixed = A ~ Tleaf + method*Tleaf2)

# No diff/marginally significant by likelihood ratio (?), BIC favors reduced model
anova(method.lme.4, method.lme.4.nolin)


# Can we make a reduced model with no method effect on intercept
method.lme.4.noint = update(method.lme.4,
                            fixed = A ~ method*Tleaf + method*Tleaf2 - method)

# No diff by AIC or likelihood raio, BIC favors reduced model.
anova(method.lme.4, method.lme.4.noint)

# Am I doing this right?

# What about a reduced model with no fixed method effects at all
method.lme.4.nomethod = update(method.lme.4, 
                               fixed = A ~ Tleaf + Tleaf2)

# The reduced model is again favored by BIC; AIC no diff, loglik no sig dif (marg sig)
anova(method.lme.4, method.lme.4.nomethod)


ranef.1 = lme(A ~ Tleaf + Tleaf2,
              data = data.small,
              random = ~ 1 | curveID,
              na.action = na.omit,
              method = "ML")

ranef.2 = lme(A ~ Tleaf + Tleaf2,
              data = data.small,
              random = ~ Tleaf | curveID,
              na.action = na.omit,
              method = "ML")

ranef.3 = lme(A ~ Tleaf + Tleaf2,
              data = data.small,
              random = ~ Tleaf2 | curveID,
              na.action = na.omit,
              method = "ML")

ranef.4 = lme(A ~ Tleaf + Tleaf2,
              data = data.small,
              random = ~ Tleaf + Tleaf2 | curveID,
              na.action = na.omit,
              method = "ML")

anova(ranef.1, ranef.2) # 2 wins by far
anova(ranef.1, ranef.3) # 3 wins by far
anova(ranef.1, ranef.4) # 4 wins by far
anova(ranef.2, ranef.4) # 4 wins by far
anova(ranef.3, ranef.4) # 4 wins by far
# So this supports my choice of using 

sigtest.1 = lme(A ~ Tleaf + Tleaf2,
                data = data.small,
                random = ~ Tleaf + Tleaf2 | curveID,
                na.action = na.omit,
                method = "ML")

# No improvement
sigtest.1.a = update(sigtest.1,
                     fixed = A ~ method + Tleaf + Tleaf2)

# No improvement
sigtest.1.b = update(sigtest.1,
                     fixed = A ~ method*Tleaf + Tleaf2 - method)

# No improvement
sigtest.1.c = update(sigtest.1,
                     fixed = A ~ Tleaf + method*Tleaf2 - method)

# No improvement
sigtest.1.full = update(sigtest.1,
                        fixed = A ~ method*Tleaf + method*Tleaf2)

