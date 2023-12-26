Food security is defined when all people, 
at all times, have physical and economic 
access to sufficient safe and nutritious 
food that meets their dietary needs and 
food preferences for an active and 
healthy life.
• The Global Food Security Index (GFSI) 
is an annual assessment that measures 
the state of food security in countries 
around the world. It is designed to 
evaluate the underlying factors and key 
risks affecting food security in each 
country and across regions. 
• Understanding and optimizing these 
factors can improve food security of any 
country.
• These factors are corruption, availability 
to adequate water supply,
• Trade Freedom, Presence of food safety 
net program, etc.
Data was sourced from a global food security index, FAO and world data bank.
We pre-processed the data by removing the missing values from Score.
Data was split into 80% training set and 20% testing set.
We had 67 predictors, we performed feature selection by visually inspecting the 
scatterplots and boxplots of the dependent variable vs the individual predictors.
We removed the outliers 
from all numeric variables 
in our data by removing 
the top and bottom 2.5% 
of observations.
Then, we scaled the 
numeric variables except 
the target variable in our 
data using minmax scaler 
before beginning the 
modeling phase.
After that we split our data 
into 80% train and 20% 
test sets which will be 
used for all our 
subsequent models.
