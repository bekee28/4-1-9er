#!/usr/bin/env python
# coding: utf-8

# In[1]:


from logistic_reg_module import *

pd.options.display.max_columns = None
pd.options.display.max_rows = None


# In[2]:


model = absenteeism_model('model', 'scaler')


# In[3]:


model.load_and_clean_data('Absenteeism_new_data.csv')


# In[4]:


df_new_obs = model.predicted_outputs()


# In[5]:


new_data_table = pd.read_csv('Absenteeism_new_data.csv')


# In[6]:


preds = df_new_obs.iloc[:,13:14]


# In[7]:


new_data_table_with_pred = pd.concat([new_data_table,preds],axis=1)


# In[8]:


new_data_table_with_pred

