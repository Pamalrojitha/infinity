#!/usr/bin/env python
# coding: utf-8

# # Reading Data set

# In[6]:


import pandas as pd
df = pd.read_csv(r'D:/ACADEMICS/case studies/data storm/1-credit_card_default_train.csv', header=0, index_col=0)

df.head() 


# In[7]:


df.shape


# In[8]:


df.columns


# # Preparing Data 

# In[ ]:


# def process_categorical_features(df):
    dummies_education = pd.get_dummies(df.EDUCATION_STATUS, prefix="EDUCATION_STATUS", drop_first=True)
    dummies_marital = pd.get_dummies(df.MARITAL_STATUS, prefix="MARITAL_STATUS", drop_first=True)
    dummies_age = pd.get_dummies(df.AGE, prefix="AGE", drop_first=True)
    dummies_gender = pd.get_dummies(df.Gender, prefix="Gender", drop_first=True)
    df.drop(["EDUCATION_STATUS", "MARITAL_STATUS","AGE","Gender"], axis=1, inplace=True)
    return pd.concat([df, dummies_education, dummies_marital, dummies_age, dummies_gender], axis=1)
df = process_categorical_features(df)

df.head()


# In[10]:


y = df['NEXT_MONTH_DEFAULT']
X = df[[col for col in df.columns if col!="NEXT_MONTH_DEFAULT"]]


# # Train/Test Split

# In[55]:


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.2, random_state=0)
print("Size of train dataset: {} rows".format(X_train.shape[0]))
print("Size of test dataset: {} rows".format(X_test.shape[0]))


# # Correlation Matrix

# In[12]:


import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
get_ipython().run_line_magic('matplotlib', 'inline')

corr=df.corr()
corr = (corr)
plt.figure(figsize=(10,10))
sns.heatmap(corr, cbar = True,  square = True, annot=True, fmt= '.2f',annot_kws={'size': 6},
            xticklabels=corr.columns.values,
            yticklabels=corr.columns.values)
plt.title('Heatmap of Correlation Matrix')


# # LDA

# In[74]:


import numpy as np
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
lda = LinearDiscriminantAnalysis()
lda.fit(X_train, y_train)


# In[75]:


print("Model Accuray: {:.2f}%".format(100*lda.score(X_test, y_test)))


# In[53]:


y2 = lda.predict(X2)
import pandas as pd
df3= pd.DataFrame(y2, index=X2.index, columns=['Predicted default'])
df3.head()


# In[54]:


df3.to_csv("lda.csv")


# # Predicting for Test data

# In[33]:


import pandas as pd
df2 = pd.read_csv(r'D:/ACADEMICS/case studies/data storm/1-credit_card_default_test.csv', header=0, index_col=0)
df2.columns


# In[34]:


df2.shape


# In[35]:


def process_categorical_features(df2):
    dummies_education = pd.get_dummies(df2.EDUCATION_STATUS, prefix="EDUCATION_STATUS", drop_first=True)
    dummies_marital = pd.get_dummies(df2.MARITAL_STATUS, prefix="MARITAL_STATUS", drop_first=True)
    dummies_age = pd.get_dummies(df2.AGE, prefix="AGE", drop_first=True)
    dummies_gender = pd.get_dummies(df2.Gender, prefix="Gender", drop_first=True)
    df2.drop(["EDUCATION_STATUS", "MARITAL_STATUS","AGE","Gender"], axis=1, inplace=True)
    return pd.concat([df2, dummies_education, dummies_marital, dummies_age, dummies_gender], axis=1)
df2 = process_categorical_features(df2)

df2.head()


# In[36]:


y2 = df2['NEXT_MONTH_DEFAULT']
X2 = df2[[col for col in df2.columns if col!="NEXT_MONTH_DEFAULT"]]


# In[39]:


import pandas as pd
df3= pd.DataFrame(y2, index=X2.index, columns=['Predicted default'])
df3.head()


# In[41]:


df3.shape


# In[40]:


df3.to_csv("Prediction using LDA.csv")

