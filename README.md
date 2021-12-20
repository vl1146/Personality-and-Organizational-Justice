# Personality-and-Organizational-Justice

This was a survey based research project with data collected using SurveyMonkey. The project sought to clarify the relationship between employee perceptions of justice and their associated organizational commitment. Employee personality was investigated as a potential moderating factor in the relationship between justice perceptions and organizational commitment.

Reverse coded responses were recoded in excel and responses with more than 25% of data missing were dropped. Data imputation was performed in R using a median impute.

data.csv contains information from respondents gathered in the survey.
  - Scores on Q1-Q3 represent measurements on the Neuroticism scale of the Big Five
  - Scores on Q4-Q6 represent measurements on the Extraversion scale of the Big Five 
  - Scores on Q7-Q9 represent measurements on the Openness scale of the Big Five 
  - Scores on Q10-Q12 represent measurements on the Agreeableness scale of the Big Five
  - Scores on Q13-Q15 represent measurements on the Conscientiousness scale of the Big Five
  - Scores on Q17-Q19 represent measurements on the Affective Commitment scale
  - Scores on Q20-Q22 represent measurements on the Continuance Commitment scale
  - Scores on Q23-Q25 represent measurements on the Normative Commitment scale
  - Scores on Q26-Q31 represent measurements on the Justice scale

personalityjustice.R is a R script containing data restructuring, analyses, and their associated visualizations. Analyses run include multiple regression, t-tests, and correlations.



