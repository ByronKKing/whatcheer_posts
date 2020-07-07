import click
import sys
import time
from copy import deepcopy

import pandas as pd
from selenium import webdriver

from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive


def get_stress_marks(phrase, driver, rooglish):
    inputElement = driver.find_element_by_id("MainContent_UserSentenceTextbox")
    inputElement.send_keys(phrase)
    driver.find_element_by_id("MainContent_SubmitButton").click()
    time.sleep(3)
    inputElement = driver.find_element_by_id("MainContent_UserSentenceTextbox")
    stressed_phrase = inputElement.text
    if rooglish:
    	soundElements = driver.find_elements_by_class_name("sounds-like")
    	sounds_like = ''
    	for elem in soundElements:
    		sounds_like = sounds_like + ' ' + elem.text
    	sounds_like = sounds_like.lstrip()
    	if stressed_phrase != sounds_like:
    		stressed_phrase = stressed_phrase + ' (' + sounds_like + ')'
    inputElement.clear()
    return(stressed_phrase)


def clean_df(df):
	df.drop_duplicates(['from_trans'], keep= 'first', inplace = True)
	df.drop_duplicates(['to_trans'], keep= 'first', inplace = True)
	df = df[df['to_trans']!=df['from_trans']]
	return(df)


@click.command()
@click.option('export_path', '--export_path', required = True, help = "Path to folder where upload files should be saved.")
@click.option('russian_stress', '--russian_stress', is_flag = True, help = "Boolean flag to scrape pronunciation symbols.")
@click.option('rooglish', '--rooglish', is_flag = True, help = "Boolean flag to get rooglish pronunciation.")
def create_anki_uploads(export_path, russian_stress, rooglish):

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
	        print('Found Saved Translation File -- ID: %s, Date Created %s' % (file['id'], file['createdDate']))
	        jsonList.append({'id': file['id'], 'date': file['createdDate']})

	if len(jsonList) < 1:
		sys.exit('No saved translation files')

	jsonList = sorted(jsonList, key=lambda x: x['date'], reverse = True)

	fileId = jsonList[0]['id']

	_file = drive.CreateFile({'id': fileId})
	_file.GetContentFile(export_path+'translations.xlsx',
	                    mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')


	## Clean files and save Anki upload files
	ogdf = pd.read_excel(export_path+'translations.xlsx', header = None)
	ogdf.columns = ['from_lang', 'to_lang', 'from_trans', 'to_trans']

	df = deepcopy(ogdf)

	russiandf = df[(df['from_lang']=='Russian')|(df['to_lang']=='Russian')].copy()

	if russian_stress:
		print("Accessing Russiangram")
		driver = webdriver.Chrome()
		driver.get("http://russiangram.com/")
		russiandf['from_trans'] = russiandf.apply(lambda x: get_stress_marks(x.from_trans, driver, rooglish) if x.from_lang == 'Russian' else x.from_trans, axis = 1)
		russiandf['to_trans'] = russiandf.apply(lambda x: get_stress_marks(x.to_trans, driver, rooglish) if x.to_lang == 'Russian' else x.to_trans, axis = 1)

	df = clean_df(df)
	russiandf = clean_df(russiandf)

	all_langs = set(df.from_lang.values).union(set(df.to_lang.values))
	all_langs.remove('English')

	for lang in all_langs:
	    if lang == 'Russian':
	        exportdf = russiandf
	    if lang == 'Polish':
	        exportdf = df[((df['from_lang']==lang)|(df['to_lang']==lang))&(df['from_lang']!='Russian')&(df['to_lang']!='Russian')]
	    exportdf[['from_trans', 'to_trans']].to_csv(export_path+lang+'_anki_import.csv', 
	                                               index = False,
	                                               header = None,
	                                               encoding="utf-8-sig")

	finaldf = pd.concat([df, russiandf])
	finaldf.to_csv(export_path + "clean_translations.csv", index = False, encoding="utf-8-sig")


	## Upload to Drive and delete all Saved Translation files
	upload = drive.CreateFile({'title': 'clean_translations.csv'})
	upload.SetContentFile(export_path + "clean_translations.csv")
	upload.Upload()
	print('Successfully uploaded clean file with ID {}'.format(upload.get('id')))

	for file in jsonList:
	    removeFile = drive.CreateFile({'id': file['id']})
	    removeFile.Delete()


if __name__ == "__main__":
	create_anki_uploads()



