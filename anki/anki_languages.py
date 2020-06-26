import pandas as pd
from copy import deepcopy
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive


export_path = '/Users/byronking/Desktop/anki_import/'

##Authenticate (note: client_secrets.json need to be in the same directory as the script)
gauth = GoogleAuth()
# Try to load saved client credentials
gauth.LoadCredentialsFile("mycreds.txt")
if gauth.credentials is None:
    # Authenticate if they're not there
    gauth.LocalWebserverAuth()
elif gauth.access_token_expired:
    # Refresh them if expired
    gauth.Refresh()
else:
    # Initialize the saved creds
    gauth.Authorize()
# Save the current credentials to a file
gauth.SaveCredentialsFile("mycreds.txt")

drive = GoogleDrive(gauth)


##Collect Saved Translations files from Drive and save file
fileList = drive.ListFile({'q': "'root' in parents and trashed=false"}).GetList()

jsonList = []
for file in fileList:
    if file['title'] == 'Saved translations':
        print('ID: %s, Date %s' % (file['id'], file['createdDate']))
        jsonList.append({'id': file['id'], 'date': file['createdDate']})

jsonList = sorted(jsonList, key=lambda x: x['date'], reverse = True)

fileId = jsonList[0]['id']

_file = drive.CreateFile({'id': fileId})
_file.GetContentFile(export_path+'translations.xlsx',
                    mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')


##Clean files and save Anki upload files
ogdf = pd.read_excel(export_path+'translations.xlsx', header = None)
ogdf.columns = ['from_lang', 'to_lang', 'from_trans', 'to_trans']

df = deepcopy(ogdf)
df.drop_duplicates(['from_trans'], keep= 'first', inplace = True)
df.drop_duplicates(['to_trans'], keep= 'first', inplace = True)
df = df[df['to_trans']!=df['from_trans']]

all_langs = set(df.from_lang.values).union(set(df.to_lang.values))
all_langs.remove('English')

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

df.to_csv(export_path + "clean_translations.csv", index = False)


##Upload to Drive and delete all Saved Translation files
upload = drive.CreateFile({'title': 'clean_translations.csv'})
upload.SetContentFile(export_path + "clean_translations.csv")
upload.Upload()
print('Uploaded file with ID {}'.format(upload.get('id')))

for file in jsonList:
    removeFile = drive.CreateFile({'id': file['id']})
    removeFile.Delete()

