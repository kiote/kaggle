import pandas as pd
import numpy as np
import pylab as P

# For .read_csv, always use header=0 when you know row 0 is the header row
df = pd.read_csv('train.csv', header=0)

# first, turn string gender to integer
df['Gender'] = df['Sex'].map( {'female': 0, 'male': 1} ).astype(int)

# df['Gender'].hist()
# P.show()

# we have empty Ages, which is not good for futher calculations
print df[ df['Age'].isnull() ][['Gender','Pclass','Age']].head(10)

# array of median ages by gender and class
# first fills with zeros
median_ages = np.zeros((2,3))

# then calculate
for i in range(0, 2):
  for j in range(0, 3):
    median_ages[i,j] = df[(df['Gender'] == i) & (df['Pclass'] == j+1)]['Age'].dropna().median()
 
print median_ages

# we need to populate new column with median age, if age is empty
# first just copy Age to new column
df['AgeFill'] = df['Age']

# then populate it for different genders and classes
for i in range(0, 2):
  for j in range(0, 3):
    df.loc[ (df.Age.isnull()) & (df.Gender == i) & (df.Pclass == j+1), 'AgeFill'] = median_ages[i,j]

# let's see what do we have now for empty ages:
print df[ df['Age'].isnull() ][['Gender','Pclass','Age', 'AgeFill']].head(10)
