#!/usr/bin/env python
# coding: utf-8

# In[ ]:


##Whole process is following

#star translations on mobile or desktop
#export saved_translations file on occasion
#run script to create import files and save cleaned file
#import into ANKI
#clear duplicates in ANKI


# In[ ]:


##Script does the following:

#download saved_translations from google drive API
#remove duplicates
#create importable ANKI csv files for each language
#uploads cleaned saved_translations file to google drive


# In[ ]:





# In[1]:


import pandas as pd


# In[2]:


from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive


# In[3]:


gauth = GoogleAuth()
gauth.LocalWebserverAuth() # client_secrets.json need to be in the same directory as the script
drive = GoogleDrive(gauth)


# In[8]:


# View all folders and file in your Google Drive
fileList = drive.ListFile({'q': "'root' in parents and trashed=false"}).GetList()


# In[28]:


jsonList = []
for file in fileList:
    if file['title'] == 'Saved translations':
        print('ID: %s, Date %s' % (file['id'], file['createdDate']))
        jsonList.append({'id': file['id'], 'date': file['createdDate']})


# In[29]:


jsonList = sorted(jsonList, key=lambda x: x['date'], reverse = True)


# In[31]:


fileId = jsonList[0]['id']


# In[32]:


_file = drive.CreateFile({'id': fileId})


# In[34]:


_file.GetContentFile('/Users/byronking/Desktop/anki_import/translations.xlsx',
                    mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')


# In[ ]:


##now upload as clean_translations
##rid of other save translations files


# In[93]:


ogdf = pd.read_excel('/Users/byronking/Desktop/anki_import/translations.xlsx', header = None)


# In[94]:


ogdf.columns = ['from_lang', 'to_lang', 'from_trans', 'to_trans']


# In[95]:


from copy import deepcopy


# In[96]:


df = deepcopy(ogdf)


# In[97]:


df.drop_duplicates(['from_trans'], keep= 'first', inplace = True)
df.drop_duplicates(['to_trans'], keep= 'first', inplace = True)
df = df[df['to_trans']!=df['from_trans']]


# In[98]:


all_langs = set(df.from_lang.values).union(set(df.to_lang.values))


# In[99]:


all_langs.remove('English')


# In[100]:


export_path = '/Users/byronking/Desktop/anki_import/'


# In[104]:


for lang in all_langs:
    if lang == 'Russian':
        exportdf = df[(df['from_lang']==lang)|(df['to_lang']==lang)]
        ##call to russiagram here
    if lang == 'Polish':
        exportdf = df[((df['from_lang']==lang)|(df['to_lang']==lang))&(df['from_lang']!='Russian')&(df['to_lang']!='Russian')]
    exportdf[['from_trans', 'to_trans']].to_csv(export_path+lang+'_anki_import.csv', 
                                               index = False,
                                               header = None,
                                               encoding="utf-8-sig")


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:




