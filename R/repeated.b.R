

repeatedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "repeatedClass",
    inherit = repeatedBase,
    private = list(
        .run = function() {

            namesVariables <- self$options$variables


            if (length(namesVariables) < 2) {
                advice <- "Welcome to Statkat!
                          <br><br>
                          This tool will help you to find an appropriate statistical method given the measurement level of your data.
                          Make sure you have correctly defined the measurement levels of your variables on the Data tab. You can change the measurement level
                          of a variable via the Setup button on the Data tab, or by double clicking on a column header of interest. 
                          <br><br>
                          You have selected the Repeated Measurements option. This is the place to be if you are 
                          interested in differences between related variables. To get started, drop two or more related variables 
                          in the white box below Related Variables. Our tool will then come up with a statistical method that may 
                          be appropriate for your data!
                         <br><br>
                          Note:<br>
                          Our advice is based on the measurement level of your data and on the number of related variables entered. There can be details related to your data, task, or assignment 
                          that may render the advice moot. Always check the assumptions made by the statistical method before interpreting the results. 
                          We always try to come up with the least complicated method that might be applicable given your data. Keep in mind that there may be other, more advanced, 
                          methods that might be applicable as well.
                "
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            dataVector = unlist(self$data[namesVariables])

            if (length(na.omit(dataVector)) == 0) {
                advice <- "The entered variables should contain data"
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            numberUniqueValues <- length(unique(na.omit(dataVector)))

            if (numberUniqueValues < 2) {
                advice <- "All subjects have the same score on all the variables. There should be some variability in the scores in order to perform statistical analysis"
                html <- self$results$advice
                html$setContent(advice)
                return()
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
                html$setContent(advice)
                return()
            }

            if (length(namesVariables) == 2) {
                if ((measurementLevel[1] == "nominal" || measurementLevel[1] == "ordinal") && numberUniqueValues == 2) {
                    advice <- "You have entered two related dichotomous variables. Hence , 
                               <a href= 'https://statkat.com/stattest.php?&t=39' target='_blank'>McNemar's test</a>
                                 seems to be a good option for you! In order to run this test in Jamovi, go to:
                                 Frequencies > Paired Samples - McNemar test
                                <ul>
                                <li>Drop one of the two paired variables in the box below Rows and the other paired variable in the box below Columns</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "ordinal") {
                    advice <- "You have entered two related ordinal variables. Hence,  the
                               <a href= 'https://statkat.com/stattest.php?&t=34' target='_blank'>sign test</a>
                                 seems to be a good option for you! Jamovi does not have a specific option for the sign test.
                                However, you can use the <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a> instead. The <i>p</i> value resulting from this Friedman test
                                is equivalent to the two sided <i>p</i> value that would have resulted from the sign test. In order to run the Friedman test in Jamovi, go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Drop the two paired variables in the box below Measures</li>
                                </ul>
                                Click on the links to learn more about these test!"
                }
                else if (measurementLevel[1] == "nominal") {
                    advice <- "You have entered two related nominal variables. Hence, the
                               <a href= 'https://statkat.com/stattest.php?&t=41' target='_blank'>marginal homogeneity test</a>
                                 seems to be a good option for you! The marginal homogeneity test is currently not available in Jamovi.
                                You could use other software to perform the analysis (e.g., R or SPSS).
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "numeric") {
                    advice <- "You have entered two related numeric variables. Hence, the
                               <a href= 'https://statkat.com/stattest.php?&t=7' target='_blank'>paired sample <i>t</i> test</a>
                                 seems to be a good option for you! In order to run this test in Jamovi, go to:
                                <br><br>
                               T-Tests > Paired Samples T-Test
                                <ul>
                                <li>Drop the two paired variables in the box below Paired Variables, one on the left side of the vertical
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
                    advice <- "You have entered several related dichotomous variables. Hence, 
                               <a href= 'https://statkat.com/stattest.php?&t=40' target='_blank'>Cochran's <i>Q</i> test</a>
                                 seems to be a good option for you! Jamovi does not have a specific option for Cochran's <i>Q</i> test.
                                However, you can use the <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a> instead. The <i>p</i> value resulting from this Friedman test
                                is equivalent to the <i>p</i> value that would have resulted from the Cochran's <i>Q</i> test. In order to run the Friedman test in Jamovi, go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Drop the related variables in the box below Measures</li>
                                </ul>
                                Click on the links to learn more about these tests!"
                }
                else if (measurementLevel[1] == "ordinal") {
                    advice <- "You have entered several related ordinal variables. Hence, the
                               <a href= 'https://statkat.com/stattest.php?&t=33' target='_blank'>Friedman test</a>
                                seems to be a good option for you! In order to run this test in Jamovi, go to:
                                <br><br>
                                ANOVA > Repeated Measures ANOVA - Friedman
                                <ul>
                                <li>Drop the related variables in the box below Measures</li>
                                </ul>
                                Click on the link to learn more about this test!"
                }
                else if (measurementLevel[1] == "nominal") {
                    advice <- "You have entered several related nominal variables. Hence, the Madansky test of interchangeability
                                seems to be a good option for you! The Madansky test of interchangeability is currently not available in Jamovi.
                                You could use other software to perform the analysis (e.g., R)."
                }
                else if (measurementLevel[1] == "numeric") {
                    advice <- "You have entered several related numeric variables. Hence, a repeated measures ANOVA
                                seems to be a good option for you! In order to run this analysis in Jamovi, go to:
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

        },
        .plot = function(image, theme, ggtheme, ...) {
          namesVariables <- self$options$variables
          if (length(namesVariables) < 2) {
            return (FALSE)
          }
          
          N <- length(self$data[[namesVariables[1]]])
          
          group <- c()
          for (variable in namesVariables) {
            group <- c(group, rep(variable, N))
          }
          group <- factor(group, levels = unique(group))
          dataVector = unlist(self$data[namesVariables])
          
          plotData <- data.frame(x = group, y = dataVector)
          
          plot <- ggplot(plotData, aes(x = x, y = y)) +
            geom_jitter(width = .1, height = 0, alpha = 0.2) +
            ggtheme +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black")) +
            xlab("Variable") + ylab("Scores") 
           
          suppressWarnings(
            suppressMessages(
              print(plot)
            )
          )
          TRUE
        })
)
