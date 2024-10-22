# lfl 2.2.1
* released: 2024-10-22
* implemented the experimental Center of Gravity defuzzification method (defuzz())
* added "a few" and "several" fuzzy quantifiers to the quantifier() function

# lfl 2.2.0
* released: 2022-09-08
* added equidist(), equifreq()
* added function to compute fuzzy-transform (ft()) and its inverse (ftinv())
* parallel t-norms and t-conorms rewritten in C++


# lfl 2.1.2
* released: 2021-08-23
* added vignette


# lfl 2.1.1
* released: 2020-11-04
* fixed the minimum required version of R to 3.6


# lfl 2.1.0
* released: 2020-10-15
* the result of algebra() is now an S3 class, implemented print.algebra()
* added involutive negation to algebra()$ni
* added nelson(), lowerEst() handlers of NA for algebra()
* added print() method for linguistic contexts
* added negated=FALSE argument to the lingexpr() function
* implemented bi-residuum of the dragonfly algebra
* implemented order operation of the dragonfly and lowerEst algebras
* added sugeno()
* added fuzzy quantifiers
* deprecated some arguments of the compose() function
* removed deprecated functions: lcut3(), lcut5()


# lfl 2.0.1
* released: 2020-05-19
* added support for percentual quantifiers in compose()
* documentation spell-check

