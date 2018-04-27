### Titanic Project for ITM 883 (Statistics)

### Description

This project was for a Master's-level stats class, focused on big data and data analytics. The objective was to take a dataset, and analyze it using the statistical methods we learned in class.

Please note that this is a group project, and we all contributed equally to the team! I, Justin Gould, worked with Mengmeng Zhang, Baylee Adams, Kyle Shope, and Tyler Lawrence to complete this project - where we contributed to all steps of the data analysis process: pre-processing/cleaning, question/objective formulation, analysis (t-test, ANOVA, logistic regression models), and communication.

 
### Interpreting the Results

T-Tests:

- The mean age between males and females who SURVIVED are likely equal, showing no significant impact of age between genders for surviving passengers.
- The mean age between males and females who did NOT SURVIVED are likely unequal, showing a significant impact of age between genders for non-surviving passengers.
- The mean age between passengers surviving vs. not surviving are likely equal, showing no significant impact of age between passenger groups.

ANOVA:

- Tested the 3 Pclasses against Survived. Resulted in significant difference of means, so we conducted a Tukey test. The Tukey test resulted in small p-values, suggesting significance between the Pclasses.


Logistic model accuracy:

Overall accuracy- 74.86%
Sensitivity Predicting Survival- 36.23%
Specificity Predicting Death- 99.09%

Logistic model case studies

- President Donald Trump's probability of surviving the Titanic (sex = male, Pclass = 1, age = 71) was 26.08%.
- Oprah Winfrey's probability of surviving the Titanic (sex = female, Pclass = 1, age = 64) was 88.86%.