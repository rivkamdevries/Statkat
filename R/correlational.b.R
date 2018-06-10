

correlationalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationalClass",
    inherit = correlationalBase,
    private = list(
        .run = function() {

            nameDependent <- self$options$dep
            namesIndependents <- self$options$independents
            namesControls <- self$options$controls

            if (is.null(nameDependent) || is.null(namesIndependents)) {
                advice <- "Welcome to Statkat!
                          <br><br>
                          This tool will help you to find an appropriate statistical method given the measurement level of your data.
                          Make sure you have correctly defined the measurement levels of your variables on the Data tab.
                          <br><br>
                          You have selected the Correlational Research option. This is the place to be if you are interested in
                          <ul>
                          <li>the relationship between two or more variables, or
                          <li>the difference between independent (unrelated) groups on a certain variable, or</li>
                          <li>predicting one variable from other variables.</li>
                          </ul>
                          To get started, drop a 
                          variable in the box below Variable 1 / Dependent Variable, and one or more variables in the box below Variable 2 / 
                          Independent Variables. Our tool will then come up with a statistical method that may be appropriate for your data! 
                          <br><br>
                          In addition, you can drop one or more variables in the box below Control Variables. Control variables
                          are variables that you are not particularly interested in, but which may be related to the dependent variable and possibly 
                          also to the independent variables. In experiments (with random assignment), control variables are often included to increase 
                          power. In observational studies, control variables are often included mainly to equate subjects on the control variables. This
                          prevents the control variables from confounding the relationships between the independent variables and the dependent variable.
                          <br><br>
                          Note:<br>
                          Our advice is based on the measurement level of your data and on the number of variables entered. There can be details related to your data, task, or assignment 
                          that may render the advice moot. Always check the assumptions made by the statistical method before interpreting the results. 
                          We always try to come up with the least complicated method that might be applicable given your data. Keep in mind that there may be other, more advanced, 
                          methods that might be applicable as well.
                "
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            dependent <- self$data[[nameDependent]]

            dependentEmpty <- length(na.omit(dependent)) == 0
            independentEmpty <- sapply(namesIndependents, function(variable) { length(na.omit(self$data[[variable]])) == 0 })
            controlsEnteredEmpty <- sapply(namesControls, function(variable) { length(na.omit(self$data[[variable]])) == 0 })

            if (dependentEmpty || any(independentEmpty) || (!is.null(namesControls) && any(controlsEnteredEmpty))) {
                advice <- "All entered variables should contain data"
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            numberUniqueValuesDependent <- length(unique(na.omit(dependent)))

            dependentConstant <- numberUniqueValuesDependent < 2
            independentConstant <- sapply(namesIndependents, function(variable) { length(unique(na.omit(self$data[[variable]]))) < 2 })
            controlsEnteredConstant <- sapply(namesControls, function(variable) { length(unique(na.omit(self$data[[variable]]))) < 2 })

            if (dependentConstant || any(independentConstant) || (!is.null(namesControls) && any(controlsEnteredConstant))) {
                advice <- "At least one of the variables entered is constant. This means that all subjects have the same score on the variable.
                                There needs to be variability in the scores on a variable in order to use it in statistical analyses."
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            oneIndependent <- length(namesIndependents) == 1 && is.null(namesControls)
            severalIndependents <- length(namesIndependents) > 1 && is.null(namesControls)

            if (oneIndependent) {
                independent <- self$data[[namesIndependents]]
                numberUniqueValuesIndependent <- length(unique(na.omit(independent)))

                if (is.factor(dependent) && is.factor(independent) && numberUniqueValuesDependent == 2 && numberUniqueValuesIndependent == 2) {
                    advice <- "You have entered a dichotomous variable for Variable 1 / Dependent Variable and a dichotomous variable for 
                                Variable 2 / Independent Variables. Hence,
                                the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                 seems to be a good option for you! In order to run this test in Jamovi, go to:
                                 <br><br>
                                 Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                 <ul>
                                    <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                 </ul>
                                 Click on the link to learn more about this test!<br><br>
                                 Note: since your categorical variables each consist of only two groups, the <i>p</i> value resulting from the
                                 chi-squared test is equivalent to the (two sided) <i>p</i> value that would have resulted form the <i>z</i> test
                                 for the difference between two proportions."
                }

                else if (is.ordered(dependent) && is.ordered(independent)) {
                    if (numberUniqueValuesDependent > 2 && numberUniqueValuesIndependent == 2) {
                        advice <- "You have entered an ordinal variable for Variable 1 / Dependent Variable and a dichotomous variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>
                                        for the difference between two groups on an ordinal variable
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        T-Tests > Independent Samples T-Test
                                        <ul>
                                            <li>Drop your dependent variable in the box below Dependent Variables and your independent (grouping) variable in the box below Grouping Variable.
                                                Your grouping variable is the variable you have put in the box for Variable 2/independent variables - the variable that consists of only two groups</li>
                                            <li>Under Tests, select Mann-Whitney U</li>
                                            <li>Under Hypothesis, select your alternative hypothesis</li>
                                        </ul>
                                        Alternative options would be <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Spearman's rank correlation coefficient</a>,
                                        which is a measure for the strength of the monotonic relationship between two variables, or the
                                        <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>.
                                        Note that the chi-squared test treats both variables as nominal, and therefore does not take the ranked nature of the ordinal variables
                                        into account.<br><br>
                                        Click on the links to learn more about these methods!"
                    }
                    else if (numberUniqueValuesDependent == 2 && numberUniqueValuesIndependent > 2) {
                        advice <- "You have entered a dichotomous variable for Variable 1 / Dependent Variable and an ordinal variable for 
                                    Variable 2 / Independent Variables. Hence, <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Spearman's rank correlation coefficient</a>,
                                        which is a measure for the strength of the monotonic relationship between two variables,
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > Correlation Matrix
                                        <ul>
                                        <li>Drop your two variables in the white box at the right
                                        </li>
                                        <li>Under Correlation Coefficients, select Spearman</li>
                                        <li>Under Hypothesis, select your alternative hypothesis</li>
                                        </ul>
                                        An alternative option would be the
                                        <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>.
                                        Note, however, that the chi-squared test treats both variables as nominal, and therefore does not take the ranked nature of the ordinal variables
                                        into account.<br><br>
                                        If you would flip variable 1 and variable 2, you could also use the <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>
                                        for the difference between two groups on an ordinal variable.<br><br>
                                        Click on the links to learn more about these methods!"
                    }
                    else {
                        advice <- "You have entered an ordinal variable for Variable 1 / Dependent Variable and an ordinal variable for 
                                    Variable 2 / Independent Variables. Hence,
                                        <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Spearman's rank correlation coefficient</a>,
                                        which is a measure for the strength of the monotonic relationship between two variables,
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > Correlation Matrix
                                        <ul>
                                        <li>Drop your two variables in the white box at the right
                                        </li>
                                        <li>Under Correlation Coefficients, select Spearman</li>
                                        <li>Under Hypothesis, select your alternative hypothesis</li>
                                        </ul>
                                        Alternative options would be the
                                        <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a> or the
                                        <a href= 'https://statkat.com/stattest.php?&t=17' target='_blank'>Kruskal-Wallis test</a> for the difference between
                                        several groups on an ordinal variable.
                                        Note, however, that the chi-squared test treats both variables as nominal, and therefore does not take the ranked nature of the ordinal variables
                                        into account. The Kruskal-Wallis test treats the dependent variable as ordinal but the independent (grouping) variable
                                        as nominal.<br><br>
                                        Click on the links to learn more about these methods!"
                    }
                }

                else if (is.ordered(dependent) && is.factor(independent)) {
                    if (numberUniqueValuesDependent > 2 && numberUniqueValuesIndependent == 2) {
                        advice <- "You have entered an ordinal variable for Variable 1 / Dependent Variable and a dichotomous variable for 
                                    Variable 2 / Independent Variables. Hence,
                                        the <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>
                                        for the difference between two groups on an ordinal variable
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        T-Tests > Independent Samples T-Test
                                        <ul>
                                        <li>Drop your dependent variable in the box below Dependent Variables and your independent (grouping) variable in the box below Grouping Variable.
                                        Your grouping variable is the variable you have put in the box for Variable 2/independent variables - the variable that consists of only two groups</li>
                                        <li>Under Tests, select Mann-Whitney U</li>
                                        <li>Under Hypothesis, select your alternative hypothesis</li>
                                        </ul>
                                        An alternative option would be the
                                        <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>.
                                        Note, however, that the chi-squared test treats both variables as nominal, and therefore does not take the ranked nature of the ordinal variable
                                        into account.<br><br>
                                        Click on the links to learn more about these tests!"
                    }
                    else if (numberUniqueValuesDependent == 2 && numberUniqueValuesIndependent > 2) {
                        advice <- "You have entered a dichotomous variable for Variable 1 / Dependent Variable and a nominal variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                        <ul>
                                        <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                        </ul>
                                        Click on the link to learn more about this test!"
                    }
                    else {
                        advice <- "You have entered an ordinal variable for Variable 1 / Dependent Variable and a nominal variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=17' target='_blank'>Kruskal-Wallis test</a>
                                        for the difference between several groups on an ordinal variable
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        ANOVA > One Way ANOVA - Kruskal-Wallis
                                        <ul>
                                        <li>Drop your dependent variable in the box below Dependent Variables and your independent (the nominal, grouping) variable in the box below Grouping Variable</li>
                                        </ul>
                                        An alternative option would be the
                                        <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>.
                                        Note, however, that the chi-squared test treats both variables as nominal, and therefore does not take the ranked nature of the ordinal variable
                                        into account.<br><br>
                                        Click on the links to learn more about these tests!"
                    }
                }

                else if (is.factor(dependent) && is.ordered(independent)) {
                    if (numberUniqueValuesDependent > 2 && numberUniqueValuesIndependent == 2) {
                        advice <- "You have entered a nominal variable for Variable 1 / Dependent Variable and a dichotomous variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                        <ul>
                                        <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                        </ul>
                                        Click on the link to learn more about this test!"
                    }
                    else if (numberUniqueValuesDependent == 2 && numberUniqueValuesIndependent > 2) {
                        advice <- "You have entered a dichotomous variable for Variable 1 / Dependent Variable and an ordinal variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                        <ul>
                                        <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                        </ul>
                                        If you would flip variable 1 and variable 2, you could also use the <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>
                                        for the difference between two groups on an ordinal variable.<br><br>
                                        Click on the links to learn more about these tests!"
                    }
                    else {
                        advice <- "You have entered a nominal variable for Variable 1 / Dependent Variable and an ordinal variable for 
                                    Variable 2 / Independent Variables. Hence,the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                        <ul>
                                        <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                        </ul>
                                        If you would flip variable 1 and variable 2, you could also use the <a href= 'https://statkat.com/stattest.php?&t=17' target='_blank'>Kruskal-Wallis test</a>
                                        for the difference between several groups on an ordinal variable.<br><br>
                                        Click on the links to learn more about these tests!"
                    }
                }

                else if (is.factor(dependent) && is.factor(independent)) {
                    advice <- "You have entered a nominal variable for Variable 1 / Dependent Variable and a nominal variable for 
                                    Variable 2 / Independent Variables. Both variables consist of two or more groups. 
                                    Hence, the <a href= 'https://statkat.com/stattest.php?&t=4' target='_blank'>chi-squared test of association</a>
                                        seems to be a good option for you! In order to run this test in Jamovi, go to:
                                        <br><br>
                                        Frequencies > Independent Samples - &chi;<sup>2</sup> test of association
                                        <ul>
                                        <li>Put one of your two categorical variables in the box below Rows, and the other categorical variable in the box below Columns</li>
                                        </ul>
                                        Click on the link to learn more about this test!"
                }

                else if (is.numeric(dependent) && is.factor(independent) && numberUniqueValuesIndependent == 2) {
                    advice <- "You have entered a numeric variable for Variable 1 / Dependent Variable and a dichotomous variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=10' target='_blank'>two sample <i>t</i> test assuming equal population variances</a>
                                    or the <a href= 'https://statkat.com/stattest.php?&t=9' target='_blank'>two sample <i>t</i> test not assuming equal population variances</a>
                                    seems to be a good option for you! Both tests are tests for the difference between two population means.
                                    In order to run these tests in Jamovi, go to:
                                    <br><br>
                                    T-Tests > Independent Samples T-Test
                                    <ul>
                                    <li>Drop your dependent (numeric) variable in the box below Dependent Variables and your independent (grouping) variable in the box below Grouping Variable</li>
                                    <li>Under Tests, select Student's if you want to assume equal population variances, and Welch's if you don't want to assume
                                        equal population variances</li>
                                    <li>Under Hypothesis, select your alternative hypothesis</li>
                                    </ul>
                                    If the normality assumption is violated, you could use the non-parametric
                                    <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>.
                                    Click on the links to learn more about these tests!"
                }

                else if (is.numeric(dependent) && is.ordered(independent)) {
                    advice <- "You have entered a numeric variable for Variable 1 / Dependent Variable and an ordinal variable for 
                                    Variable 2 / Independent Variables. Hence, <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Spearman's rank correlation coefficient</a>,
                                        which is a measure for the strength of the monotonic relationship between two variables,
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > Correlation Matrix
                                        <ul>
                                        <li>Drop your two variables in the white box at the right
                                        </li>
                                        <li>Under Correlation Coefficients, select Spearman</li>
                                        <li>Under Hypothesis, select your alternative hypothesis</li>
                                        </ul>
                                        An alternative option would be <a href= 'https://statkat.com/stattest.php?&t=11' target='_blank'>one way ANOVA</a>, which is
                                        is a test for the difference between several population means. Your numeric variable would be the dependent
                                        variable and your ordinal variable would be the independent variable (grouping factor).<br><br>
                                        The disadvantage of the one way ANOVA is that it will not take the ranked nature of the ordinal
                                        independent variable into account. That is, it will treat the ordinal independent variable as a nominal variable.
                                        On the other hand, the disadvantage of Spearman's rho is that it treats the numeric variable as an ordinal variable, rather than an interval/ratio variable.
                                        <br><br>
                                        Click on the links to learn more about these methods!"
                }

                else if (is.numeric(dependent) && is.factor(independent)) {
                    advice <- "You have entered a numeric variable for Variable 1 / Dependent Variable and a nominal variable for 
                                    Variable 2 / Independent Variables. Hence, a <a href= 'https://statkat.com/stattest.php?&t=11' target='_blank'>one way ANOVA</a>, which is
                                        is a test for the difference between several population means,
                                    seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                    <br><br>
                                    ANOVA > ANOVA
                                    <ul>
                                    <li>Drop your dependent (numeric) variable in the box below Dependent Variable and your independent (grouping) variable in the box below Fixed Factors</li>
                                    </ul>
                                    If the normality or homoscedasticity assumption is violated, you could use the non-parametric
                                    <a href= 'https://statkat.com/stattest.php?&t=17' target='_blank'>Kruskal-Wallis test</a>.
                                    <br><br>
                                    Click on the links to learn more about these tests!"
                }

                else if (is.factor(dependent) && is.numeric(independent) && numberUniqueValuesDependent == 2) {
                    advice <- "You have entered a dichotomous variable for Variable 1 / Dependent Variable and a numeric variable for 
                                    Variable 2 / Independent Variables. Hence, <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                    seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                    <br><br>
                                    Regression > 2 Outcomes - Binomial
                                    <ul>
                                    <li>Drop your dependent (dichotomous) variable in the box below Dependent Variable and your independent (numeric) variable in the box below Covariates
                                    </li>
                                    </ul>
                                    If you would flip variable 1 and variable 2, you could also do a <a href= 'https://statkat.com/stattest.php?&t=10&t2=9' target='_blank'>two sample <i>t</i> test</a>
                                    for the difference between two population means, or the non-parametric <a href= 'https://statkat.com/stattest.php?&t=14' target='_blank'>Mann-Whitney U test</a>.
                                    <br><br>
                                    Click on the links to learn more about these methods!"
                }

                else if (is.ordered(dependent) && is.numeric(independent)) {
                    advice <- "You have entered an ordinal variable for Variable 1 / Dependent Variable and a numeric variable for 
                                    Variable 2 / Independent Variables. Hence, <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Spearman's rank correlation coefficient</a>,
                                    which is a measure for the strength of the monotonic relationship between two variables,
                                    seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                    <br><br>
                                    Regression > Correlation Matrix
                                    <ul>
                                    <li>Drop your two variables in the white box at the right
                                    </li>
                                    <li>Under Correlation Coefficients, select Spearman</li>
                                    <li>Under Hypothesis, select your alternative hypothesis</li>
                                    </ul>
                                    An alternative option would be ordinal logistic regression, with your ordinal variable as dependent variable and your
                                    numeric variable as independent variable. The advantage of this method is that it treats the
                                    numeric variable as an interval variable, whereas the Spearman correlation treats both variables as ordinal. However,
                                    the Spearman correlation is a much easier option than ordinal logistic regression. Also, ordinal logistic regression
                                    is currently not available in Jamovi, so you would need to find other software to perform the analysis (e.g., R or SPSS).
                                    <br><br>
                                    Finally, if you would flip variable 1 and variable 2, you could also do a <a href= 'https://statkat.com/stattest.php?&t=11' target='_blank'>one way ANOVA</a>, which is
                                    a test for the difference between several population means. Your numeric variable would be the dependent
                                    variable and your ordinal variable would be the independent variable (grouping factor). The disadvantage of the one way ANOVA is that it will not take the ranked nature of the ordinal
                                    independent variable into account. That is, it will treat the ordinal independent variable as a nominal variable.
                                    <br><br>
                                    Click on the links to learn more about these methods!"
                }

                else if (is.factor(dependent) && is.numeric(independent)) {
                    advice <-  "You have entered a nominal variable for Variable 1 / Dependent Variable and a numeric variable for 
                                    Variable 2 / Independent Variables. Hence, multinomial logistic regression seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                    <br><br>
                                    Regression > N Outcomes - Multinomial
                                    <ul>
                                    <li>Drop your dependent (nominal) variable in the box below Dependent Variable and your independent (numeric) variable in the box below Covariates
                                    </li>
                                    </ul>
                                    If you would flip variable 1 and variable 2, you could also do a <a href= 'https://statkat.com/stattest.php?&t=11' target='_blank'>one way ANOVA</a>,
                                    which is a test for the difference between several population means. Your numeric variable would be the dependent
                                    variable and your nominal variable would be the independent variable (grouping factor).
                                    Click on the link to learn more about this method!"
                }


                else if (is.numeric(dependent) && is.numeric(independent)) {
                    advice <- "You have entered a numeric variable for Variable 1 / Dependent Variable and a numeric variable for 
                                    Variable 2 / Independent Variables. Hence, the <a href= 'https://statkat.com/stattest.php?&t=18' target='_blank'>Pearson correlation coefficient</a>,
                                    which is a measure for the strength of the linear relationship between two variables,
                                    seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                    <br><br>
                                    Regression > Correlation Matrix
                                    <ul>
                                    <li>Drop your two variables in the white box at the right
                                    </li>
                                    <li>Under Correlation Coefficients, select Pearson (selected by default)</li>
                                    <li>Under Hypothesis, select your alternative hypothesis</li>
                                    </ul>
                                    Alternatively, you could perform a
                                    <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a>. The test outcomes of
                                    both methods will be equivalent.
                                    <br><br>
                                    Click on the links to learn more about these methods!"
                }

                else {
                    stop("At least one of the variable types unknown")
                }

            }

            else if (severalIndependents) {
                independentCategorical <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) })
                independentNeedsDummy <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) > 2 })
                independentDichotomous <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) == 2 })

                stepsNoControlsCodeVars <- "<ul>
                                            <li>Drop your dependent variable in the box below Dependent Variable</li>
                                            <li>Drop your independent variables in the box below Covariates. Independent variables of nominal or ordinal measurement level that consist of more than two groups
                                            should be transformed into code variables before they are included in the analysis. In Jamovi, instead of transforming your categorical
                                            independent variables into code variables yourself, you can also put the untransformed categorical independent variables in the
                                            box below Factors. Jamovi will then make the code variables for you 'behind the scenes'</li>
                                            </ul>"

                stepsNoControlsCategorical <- "<ul>
                                                <li>Drop your dependent variable in the box below Dependent Variable</li>
                                                <li>Drop your independent variables in the box below Covariates. Independent variables of nominal or ordinal measurement level that consist of more than two groups
                                                should be transformed into code variables before they are included in the analysis. Independent variables of nominal or ordinal measurement
                                                level that consist of two groups can be transformed into code variables, but they don't need to be. In Jamovi, instead of transforming your categorical
                                                independent variables into code variables yourself, you can also put the untransformed categorical independent variables in the
                                                box below Factors. Jamovi will then make the code variables for you 'behind the scenes'</li>
                                               </ul>"

                stepsNoControlsAllNumeric <- "<ul>
                                                <li>Drop your dependent variable in the box below Dependent Variable</li>
                                                <li>Drop your independent variables in the box below Covariates</li>
                                              </ul>"

                if (is.factor(dependent) && numberUniqueValuesDependent == 2) {
                    if (any(independentNeedsDummy) && !any(independentDichotomous)) {
                        advice <- paste("You have entered a dichotomous dependent variable and several independent variables. 
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                         seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                         <br><br>
                                         Regression > 2 Outcomes - Binomial",
                                         stepsNoControlsCodeVars,
                                         "Click on the link to learn more about this method!")

                    }
                    else if (any(independentCategorical)) {
                        advice <- paste("You have entered a dichotomous dependent variable and several independent variables. 
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > 2 Outcomes - Binomial",
                                        stepsNoControlsCategorical,
                                        "Click on the link to learn more about this method!")
                    }
                    else {
                        advice <- paste("You have entered a dichotomous dependent variable and several independent variables. 
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > 2 Outcomes - Binomial",
                                        stepsNoControlsAllNumeric,
                                        "Click on the link to learn more about this method!")
                    }
                }
                else if (is.ordered(dependent)) {
                    advice <- paste("You have entered an ordinal dependent variable and several independent variables. 
                                         Hence, ordinal logistic regression analysis seems to be a good option for you! Ordinal logistic regression
                                     is currently not available in Jamovi. You could use other software to perform the analysis (e.g., R or SPSS).")
                }
                else if (is.factor(dependent)) {
                    if (any(independentNeedsDummy) && !any(independentDichotomous)) {
                        advice <- paste("You have entered a nominal dependent variable and several independent variables. 
                                         Hence, multinomial logistic regression analysis
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > N Outcomes - Multinomial",
                                        stepsNoControlsCodeVars)
                    }
                    else if (any(independentCategorical)) {
                        advice <- paste("You have entered a nominal dependent variable and several independent variables. 
                                         Hence, multinomial logistic regression analysis
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > N Outcomes - Multinomial",
                                        stepsNoControlsCategorical)
                    }
                    else {
                        advice <- paste("You have entered a nominal dependent variable and several independent variables. 
                                         Hence, multinomial logistic regression analysis
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > N Outcomes - Multinomial",
                                        stepsNoControlsAllNumeric)
                    }
                }
                else if (is.numeric(dependent)) {
                    if (all(independentCategorical)) {
                        if (length(namesIndependents) == 2) {
                            advice <- "You have entered a numeric dependent variable and two categorical (nominal/ordinal) independent variables. 
                                            Hence, a <a href= 'https://statkat.com/stattest.php?&t=12' target='_blank'>two way ANOVA</a>
                                            seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                            <br><br>
                                            ANOVA > ANOVA
                                            <ul>
                                            <li>Drop your numeric dependent variable in the box below Dependent Variable and your two independent (grouping) variables
                                                in the box below Fixed Factors</li>
                                            </ul>
                                            Click on the link to learn more about this method!"
                        }
                        else {
                            advice <- "You have entered a numeric dependent variable and several categorical (nominal/ordinal) independent variables. 
                                            Hence, an ANOVA
                                            seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                            <br><br>
                                            ANOVA > ANOVA
                                            <ul>
                                            <li>Drop your numeric dependent variable in the box below Dependent Variable and your independent (grouping) variables
                                            in the box below Fixed Factors</li>
                                            </ul>"
                        }

                    }
                    else if (any(independentNeedsDummy) && !any(independentDichotomous)) {
                        advice <- paste("You have entered a numeric dependent variable and several independent variables. Some independent variables are numeric
                                        and some independent variables are categorical (nominal/ordinal). 
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > Linear Regression",
                                        stepsNoControlsCodeVars,
                                        "Click on the link to learn more about this method!")
                    }
                    else if (any(independentCategorical)) {
                        advice <-  paste("You have entered a numeric dependent variable and several independent variables. Some independent variables are numeric
                                        and some independent variables are categorical (nominal/ordinal). 
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                         seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                         <br><br>
                                         Regression > Linear Regression",
                                         stepsNoControlsCategorical,
                                         "Click on the link to learn more about this method!")
                    }
                    else {
                        advice <- paste("You have entered a numeric dependent variable and several numeric independent variables. 
                                        Hence, <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > Linear Regression",
                                        stepsNoControlsAllNumeric,
                                        "Click on the link to learn more about this method!")
                    }
                }
                else {
                    stop("At least one of the variable types unknown")
                }
            }


            else {
                independentCategorical <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) })
                independentNeedsDummy <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) > 2 })
                independentDichotomous <- sapply(namesIndependents, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) == 2 })
                controlCategorical <- sapply(namesControls, function(variable) { is.factor(self$data[[variable]]) })
                controlNeedsDummy <- sapply(namesControls, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) > 2  })
                controlDichotomous <- sapply(namesControls, function(variable) { is.factor(self$data[[variable]]) && length(unique(na.omit(self$data[[variable]]))) == 2 })

                stepsWithControlsCodeVars <- "<ul>
                                                <li>Drop your dependent variable in the box below Dependent Variable</li>
                                                <li>Drop your independent variables and control variables in the box below Covariates. Independent/control variables of nominal or ordinal measurement level that consist of more than two groups
                                                should be transformed into code variables before they are included in the analysis. In Jamovi, instead of transforming your categorical
                                                independent/control variables into code variables yourself, you can also put the untransformed categorical variables in the
                                                box below Factors. Jamovi will then make the code variables for you 'behind the scenes'</li>
                                              </ul>"

                stepsWithControlsCategorical <- "<ul>
                                                    <li>Drop your dependent variable in the box below Dependent Variable</li>
                                                    <li>Drop your independent variables and control variables in the box below Covariates. Independent/control variables of nominal or ordinal measurement level that consist of more than two groups
                                                    should be transformed into code variables before they are included in the analysis. Independent/control variables of nominal or ordinal measurement
                                                    level that consist of two groups can be transformed into code variables, but they don't need to be. In Jamovi, instead of transforming your categorical
                                                    independent/control variables into code variables yourself, you can also put the untransformed categorical variables in the
                                                    box below Factors. Jamovi will then make the code variables for you 'behind the scenes'</li>
                                                 </ul>"

                stepsWithControlsAllNumeric <- "<ul>
                                                    <li>Drop your dependent variable in the box below Dependent Variable</li>
                                                    <li>Drop your independent variables and control variables in the box below Covariates</li>
                                                 </ul>"

                if (is.factor(dependent) && numberUniqueValuesDependent == 2) {
                    if (any(independentNeedsDummy) || any(controlNeedsDummy) && !any(independentDichotomous) && !any(controlDichotomous)) {
                        advice <- paste("You have entered a dichotomous dependent variable, one or more independent variables, and one or more control variables. Hence, 
                                        <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > 2 Outcomes - Binomial",
                                        stepsWithControlsCodeVars,
                                       " Click on the link to learn more about this method!")
                    }
                    else if (any(independentCategorical) || any(controlCategorical)) {
                        advice <- paste("You have entered a dichotomous dependent variable, one or more independent variables, and one or more control variables. Hence, 
                                        <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > 2 Outcomes - Binomial",
                                        stepsWithControlsCategorical,
                                        "Click on the link to learn more about this method!")
                    }
                    else {
                        advice <- paste("You have entered a dichotomous dependent variable, one or more independent variables, and one or more control variables. Hence, 
                                        <a href= 'https://statkat.com/stattest.php?&t=20' target='_blank'>logistic regression analysis</a> 
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > 2 Outcomes - Binomial",
                                        stepsWithControlsAllNumeric,
                                        "Click on the link to learn more about this method!")
                    }
                }

                else if (is.ordered(dependent)) {
                    advice <- paste("You have entered an ordinal dependent variable, one or more independent variables, and one or more control variables. 
                                    Hence, ordinal logistic regression analysis seems to be a good option for you! Ordinal logistic regression
                                    is currently not available in Jamovi. You could use other software to perform the analysis (e.g., R or SPSS).")
                }

                else if (is.factor(dependent)) {
                    if (any(independentNeedsDummy) || any(controlNeedsDummy) && !any(independentDichotomous) && !any(controlDichotomous)) {
                        advice <- paste("You have entered a nominal dependent variable, one or more independent variables, and one or more control variables. 
                                   Hence, multinomial logistic regression
                                   seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                   <br><br>
                                   Regression > N Outcomes - Multinomial",
                                   stepsWithControlsCodeVars)
                    }
                    else if (any(independentCategorical) || any(controlCategorical)) {
                        advice <- paste("You have entered a nominal dependent variable, one or more independent variables, and one or more control variables. 
                                   Hence, multinomial logistic regression
                                      seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                      <br><br>
                                      Regression > N Outcomes - Multinomial",
                                      stepsWithControlsCategorical)
                    }
                    else {
                        advice <- paste("You have entered a nominal dependent variable, one or more independent variables, and one or more control variables. 
                                        Hence, multinomial logistic regression
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        Regression > N Outcomes - Multinomial",
                                        stepsWithControlsAllNumeric)
                    }
                }

                else if (is.numeric(dependent)) {
                    if (all(independentCategorical) && all(controlCategorical)) {
                        if (length(namesIndependents) == 1 && length(namesControls) == 1) {
                            advice <- "You have entered a numeric dependent variable, a categorical (nominal/ordinal) independent variable, and a 
                                        categorical (nominal/ordinal) control variable.  
                                        Hence, a <a href= 'https://statkat.com/stattest.php?&t=12' target='_blank'>two way ANOVA</a>
                                            seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                            <br><br>
                                            ANOVA > ANOVA
                                            <ul>
                                            <li>Drop your numeric dependent variable in the box below Dependent Variable and your nominal/ordinal independent and control
                                            variables in the box below Fixed Factors</li>
                                            </ul>
                                            Click on the link to learn more about this method!"
                        }
                        else {
                            advice <- "You have entered a numeric dependent variable, one or more categorical (nominal/ordinal) independent variables, and 
                                        one or more categorical (nominal/ordinal) control variables.  
                                        Hence, an ANOVA
                                            seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                            <br><br>
                                            ANOVA > ANOVA
                                            <ul>
                                            <li>Drop your numeric dependent variable in the box below Dependent Variable and your nominal/ordinal independent and control
                                            variables in the box below Fixed Factors</li>
                                            </ul>"
                        }
                    }

                    else if (all(independentCategorical) && !any(controlCategorical)) {
                        advice <- "You have entered a numeric dependent variable, one or more categorical (nominal/ordinal) independent variables, and 
                                        one or more numeric control variables.  
                                        Hence, an ANCOVA
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        ANOVA > ANCOVA
                                        <ul>
                                        <li>Drop your numeric dependent variable in the box below Dependent Variable</li>
                                        <li>Drop your nominal/ordinal independent variables in the box below Fixed Factors</li>
                                        <li>Drop your numeric control variables in the box below Covariates</li>
                                        </ul>"
                    }
                    else if (all(independentCategorical)) {
                        advice <- "You have entered a numeric dependent variable, one or more categorical (nominal/ordinal) independent variables, and 
                                        one or more control variables. Some of your control variables are categorical (nominal/ordinal) and some are numeric.  
                                        Hence, an ANCOVA
                                        seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                        <br><br>
                                        ANOVA > ANCOVA
                                        <ul>
                                        <li>Drop your numeric dependent variable in the box below Dependent Variable</li>
                                        <li>Drop your nominal/ordinal independent and control variables in the box below Fixed Factors</li>
                                        <li>Drop your numeric control variables in the box below Covariates</li>
                                        </ul>"
                    }
                    else if (any(independentNeedsDummy) || any(controlNeedsDummy) && !any(independentDichotomous) && !any(controlDichotomous)) {
                        advice <- paste("You have entered a numeric dependent variable, one or more independent variables, and one or more control variables.
                                        At least some of your independent variables are numeric.   
                                        Hence, a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                          seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                          <br><br>
                                          Regression > Linear Regression",
                                          stepsWithControlsCodeVars,
                                          "Click on the link to learn more about this method!")
                                }
                    else if (any(independentCategorical) || any(controlCategorical)) {
                        advice <- paste("You have entered a numeric dependent variable, one or more independent variables, and one or more control variables.
                                        At least some of your independent variables are numeric. Hence, 
                                      <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                     seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                     <br><br>
                                     Regression > Linear Regression",
                                     stepsWithControlsCategorical,
                                    "Click on the link to learn more about this method!")
                    }
                    else {
                        advice <- paste("You have entered a numeric dependent variable, one or more numeric independent variables, and one or more numeric control variables.
                                         Hence, <a href= 'https://statkat.com/stattest.php?&t=13' target='_blank'>linear regression analysis</a> 
                                         seems to be a good option for you! In order to run this analysis in Jamovi, go to:
                                         <br><br>
                                         Regression > Linear Regression",
                                         stepsWithControlsAllNumeric,
                                         "Click on the link to learn more about this method!")
                    }

                }

                else {
                    stop("At least one of the variable types unknown")
                }
            }


            html <- self$results$advice
            html$setContent(advice)

        },
        .plot=function(image, theme, ggtheme, ...) {
          if (is.null(self$options$dep)) {
            return (FALSE)
          }
          
          nameDependent <- self$options$dep
          nameIndependent <- image$key
          
          y <- self$data[[nameDependent]]
          x <- self$data[[nameIndependent]]
          
          if (is.factor(y) && is.factor(x)) {
            xaxisLabel <- paste(nameIndependent, " (jittered)")
            yaxisLabel <- paste(nameDependent, " (jittered)")
            widthJitter = .1
            heightJitter = .1
          }
          
          else if (is.factor(x)) {
            xaxisLabel <- paste(nameIndependent, " (jittered)")
            yaxisLabel <- nameDependent
            widthJitter = .1
            heightJitter = 0
          }
          
          else if (is.factor(y)) {
            xaxisLabel <- nameIndependent
            yaxisLabel <- paste(nameDependent, " (jittered)")
            widthJitter = 0
            heightJitter = .1
          }
          
          else {
            xaxisLabel <- nameIndependent
            yaxisLabel <- nameDependent
            widthJitter = 0
            heightJitter = 0
          }
          
          plotData <- data.frame(x = x, y = y)
          
          plot <- ggplot(plotData, aes(x = x, y = y)) + 
            geom_jitter(width = widthJitter, height = heightJitter) + 
            ggtheme + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black")) +
            xlab(xaxisLabel) + ylab(yaxisLabel) +
            ggtitle(nameIndependent) +
            theme(plot.title = element_text(hjust = 0.5))
        
          
          suppressWarnings(
            suppressMessages(
              print(plot)
            )
          )
          TRUE
        })
)
