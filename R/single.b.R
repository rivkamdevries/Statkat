

singleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "singleClass",
    inherit = singleBase,
    private = list(
        .run = function() {
            nameVar <- self$options$variable
            if (is.null(nameVar)) {
                advice <- "Welcome to Statkat!
                          <br><br>
                          This tool will help you to find an appropriate statistical method given the measurement level of your data.
                          Make sure you have correctly defined the measurement levels of your variables on the Data tab.
                          <br><br>
                          To get started, drop a variable in the white box below Variable. Our tool will then come up with a statistical method that may be appropriate for your data!
                          <br><br>
                          Note:<br>
                          Our advice is based on the measurement level of your data. There can be details related to your data, task, or assignment that may render the advice moot. 
                          Always check the assumptions made by the statistical method before interpreting the results. 
                          We always try to come up with the least complicated method that might be applicable given your data. Keep in mind that there may be other, more advanced, 
                          methods that might be applicable as well.
                          "
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            var <- self$data[[nameVar]]

            if (length(na.omit(var)) == 0) {
                advice <- "The entered variable should contain data."
                html <- self$results$advice
                html$setContent(advice)
                return()
            }

            numberUniqueValues <- length(unique(na.omit(var)))

            if (numberUniqueValues < 2) {
                advice <- "The entered variable is constant. This means that all subjects have the same score on the variable.
                           There needs to be variability in the scores on a variable in order to use it in statistical analyses."
                html <- self$results$advice
                html$setContent(advice)
                return()
            }


            if (is.factor(var) && numberUniqueValues == 2) {
                advice <- "You have entered a dichotomous variable. Hence, the Binomial test for the population proportion may be a good option for you!
                           In order to perform this test, go to:
                           <br><br>
                           Frequencies > 2 Outcomes - Binomial test
                           <ul>
                            <li>Drop your dichotomous variable in the white box at the right</li>
                            <li>Fill in the value for &pi;<sub>0</sub> in the box next to Test value</li>
                            <li>Under Hypothesis, select your alternative hypothesis</li>
                           </ul>"
            }
            else if (is.ordered(var)) {
                advice <- "You have entered a variable of ordinal measurement level. Hence, 
                           the <a href= 'https://statkat.com/stattest.php?&t=42' target='_blank'>Wilcoxon signed-rank test</a> for the population median
                           may be a good option for you! In order to perform this test, go to:
                           <br><br>
                           T-Tests > One Sample T-Test
                             <ul>
                                <li>Drop your variable in the box below Dependent Variables</li>
                                <li>Under Tests, select Wilcoxon rank</li>
                                <li>Under Hypothesis, fill in the value for $m_0$ in the box next to Test Value, and select your alternative hypothesis</li>
                            </ul>
                            Click on the link to learn more about this test!"
            }
            else if (is.factor(var)) {
                advice <- "You have entered a variable of nominal measurement level. Hence, 
                           the  <a href= 'https://statkat.com/stattest.php?&t=3' target='_blank'>goodness of fit test</a>
                           may be a good option for you! In order to perform this test, go to:
                           <br><br>
                           Frequencies > N Outcomes - &Chi;<sup>2</sup> Goodness of fit
                           <ul>
                              <li>Drop your nominal variable in the box below Variable</li>
                              <li>Click on Expected Proportions and fill in the population proportions / probabilities according to your null hypothesis in the boxes below Ratio.
                                  If your null hypothesis states that they are all equal, you can leave the ratios equal to the default values (1)</li>
                           </ul>
                           Click on the link to learn more about this test!"
            }
            else if (is.numeric(var)) {
                advice <- "You have entered a numeric variable. Hence, 
                           the  <a href= 'https://statkat.com/stattest.php?&t=6' target='_blank'>one sample <i>t</i> test</a> for the population mean
                may be a good option for you! In order to perform this test, go to:
                <br><br>
                T-Tests > One Sample T-Test
                <ul>
                    <li>Drop your variable in the box below Dependent Variables</li>
                    <li>Under Hypothesis, fill in the value for &mu;<sub>0</sub> in the box next to Test Value, and select your alternative hypothesis</li>
                </ul>
                If the normality assumption is violated, you could use the non-parametric
                <a href= 'https://statkat.com/stattest.php?&t=42' target='_blank'>Wilcoxon signed-rank test</a> for the population median instead.
                <br><br>
                Click on the links to learn more about these tests!"
            }
            else {
                stop("Unknown data type")
            }
            
            html <- self$results$advice
            html$setContent(advice)
            
        },
        .plot = function(image, theme, ggtheme, ...) {
          if (is.null(self$options$variable)) {
            return (FALSE)
          }
          
          var <- self$data[[self$options$variable]]
          N <- length(var)
          
          if (N == 0) {
            return (FALSE)
          }
          
          plotData <- data.frame(x = rnorm(N, 0, .02), y = var)
          
          plot <- ggplot(plotData, aes(x = x, y = y)) + 
            geom_point() + 
            xlim(-1, 1) + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black")) +
            xlab("") + ylab(self$options$variable) +
            ggtheme

          print(plot)
          TRUE
        })
)
