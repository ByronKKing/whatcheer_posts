import click

import pandas as pd
from copy import deepcopy
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

@click.command()
@click.option('export_path', '--export_path', required = True, help = "Path to folder where upload files should be saved.")
def create_anki_uploads(export_path):

	assert export_path.endswith('/')

	## Authenticate (note: client_secrets.json need to be in the same directory as the script)
	gauth = GoogleAuth()

	gauth.LoadCredentialsFile("mycreds.txt") # Try to load saved client credentials
	if gauth.credentials is None:
		gauth.LocalWebserverAuth() # Authenticate if they're not there
	elif gauth.access_token_expired:
		gauth.Refresh() # Refresh them if expired
	else:
		gauth.Authorize() # Initialize the saved creds
	gauth.SaveCredentialsFile("mycreds.txt") # Save the current credentials to a file

	drive = GoogleDrive(gauth)


	## Collect Saved Translations files from Drive and save file
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


	## Clean files and save Anki upload files
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


	## Upload to Drive and delete all Saved Translation files
	upload = drive.CreateFile({'title': 'clean_translations.csv'})
	upload.SetContentFile(export_path + "clean_translations.csv")
	upload.Upload()
	print('Uploaded file with ID {}'.format(upload.get('id')))

	for file in jsonList:
	    removeFile = drive.CreateFile({'id': file['id']})
	    removeFile.Delete()

if __name__ == "__main__":
	create_anki_uploads()



