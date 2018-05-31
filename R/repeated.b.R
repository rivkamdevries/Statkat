

repeatedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "repeatedClass",
    inherit = repeatedBase,
    private = list(
        .run = function() {

            namesVariables <- self$options$variables


            if (length(namesVariables) < 2) {
                advice <- "Welcome to Statkat!
                          <br><br>
                          You have selected the Repeated Measurements option. This is the place to be if you are 
                          interested in differences between related variables. To get started, drop two or more related variables 
                          in the white box below Related Variables. Our tool will then come up with a statistical method that may 
                          be appropriate for your data!
                "
                html <- self$results$advice
                return(html$setContent(advice))
            }

            dataVector = unlist(self$data[namesVariables])

            if (length(na.omit(dataVector)) == 0) {
                advice <- "The entered variables should contain data"
                html <- self$results$advice
                return(html$setContent(advice))
            }

            numberUniqueValues <- length(unique(na.omit(dataVector)))

            if (numberUniqueValues < 2) {
                advice <- "All subjects have the same score on all the variables. There should be some variability in the scores in order to perform statistical analysis"
                html <- self$results$advice
                return(html$setContent(advice))
            }

            measurementLevel <- sapply(namesVariables, function(variable) {
                                                            if (is.ordered(self$data[[variable]]))
                                                                "ordinal"
                                                            else if (is.factor(self$data[[variable]]))
                                                                "nominal"
                                                            else if (is.numeric(self$data[[variable]]))
                                                                "numeric"
                                                            else
                                                                "unknown"
                                                            })

            if (any(measurementLevel == "unknown")) {
                stop("unknown measurement level")
            }

            if (length(unique(measurementLevel)) > 1) {
                advice <- "All entered variables should have the same measurement level"
                html <- self$results$advice
                return(html$setContent(advice))
            }

            if (length(namesVariables) == 2) {
                if ((measurementLevel[1] == "nominal" || measurementLevel[1] == "ordinal") && numberUniqueValues == 2) {
                    advice <- "Given the measurement level of your entered variables,
                               <a href= 'https://statkat.com/stattest.php?&t=39' target='_blank'>McNemar's test</a>
                                 seems to be a good option for you! In order to perform this test, go to:
                                 Frequencies > Paired Samples - McNemar test
                                <ul>
                                <li>Put one of the two paired variables in the box below Rows and the other paired variable in the box below Columns</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "ordinal") {
                    advice <- "Given the measurement level of your entered variables, the
                               <a href= 'https://statkat.com/stattest.php?&t=39' target='_blank'>sign test</a>
                                 seems to be a good option for you! Jamovi does not have a specific option for the sign test.
                                However, you can do the <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a> instead. The <i>p</i> value resulting from this Friedman test
                                is equivalent to the two sided <i>p</i> value that would have resulted from the sign test. Go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Put the two paired variables in the box below Measures</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "nominal") {
                    advice <- "Given the measurement level of your entered variables, the
                               <a href= 'https://statkat.com/stattest.php?&t=41' target='_blank'>marginal homogeneity test</a>
                                 seems to be a good option for you! The marginal homogeneity test is currently not available in Jamovi.
                                You could use other software to perform the analysis (e.g., R or SPSS).
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "numeric") {
                    advice <- "Given the measurement level of your entered variables, the
                               <a href= 'https://statkat.com/stattest.php?&t=7' target='_blank'>paired sample <i>t</i> test</a>
                                 seems to be a good option for you! In order to perform this test, go to:
                                <br><br>
                               T-Tests > Paired Samples T-Test
                                <ul>
                                <li>Put the two paired variables in the box below Paired Variables, one on the left side of the vertical
                                    line and one on the right side of the vertical line</li>
                                <li>Under Hypothesis, select your alternative hypothesis</li>
                                </ul>
                                If the normality assumption is violated, you could use the non-parametric
                                <a href= 'https://statkat.com/stattest.php?&t=35' target='_blank'>Wilcoxon signed rank test</a>.
                                Click on the links to learn more about these tests!"
                }
            }
            else {
                if ((measurementLevel[1] == "nominal" || measurementLevel[1] == "ordinal") && numberUniqueValues == 2) {
                    advice <- "Given the measurement level of your entered variables,
                               <a href= 'https://statkat.com/stattest.php?&t=40' target='_blank'>Cochran's <i>Q</i> test</a>
                                 seems to be a good option for you! Jamovi does not have a specific option for Cochran's <i>Q</i> test.
                                However, you can do the <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a> instead. The <i>p</i> value resulting from this Friedman test
                                is equivalent to the <i>p</i> value that would have resulted from the Cochran's <i>Q</i> test. Go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Put the related variables in the box below Measures</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "ordinal") {
                    advice <- "Given the measurement level of your entered variables, the
                               <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a>
                                seems to be a good option for you! In order to perform this test, go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Put the related variables in the box below Measures</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "nominal") {
                    advice <- "Given the measurement level of your entered variables, the Madansky test of interchangeability
                                seems to be a good option for you! The Madansky test of interchangeability is currently not available in Jamovi.
                                You could use other software to perform the analysis (e.g., R)."
                }
                else if (measurementLevel[1] == "numeric") {
                    advice <- "Given the measurement level of your entered variables, a repeated measures ANOVA
                                seems to be a good option for you! In order to perform this test, go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA
                                <ul>
                                <li>Under Repeated Measures Factors, replace the name RM Factor 1 with a more appropriate name (e.g., 'measurement point'). Then
                                give a name to each level (e.g., measurement 1, measurement 2, etc.). Make sure that the number of levels you have defined equals
                                the number of related variables you have</li>
                                <li>Drag the related variables to the box below Repeated Measures Cells, one per level</li>
                                </ul>
                                Alternatively, if distributional assumptions are violated, you could use the non-parametric
                                <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a>.
                                Click on the link to learn more about this test!"                }
            }


            html <- self$results$advice
            html$setContent(advice)

        })
)
