from requests import get
from bs4 import BeautifulSoup
import json
import numpy as np
import xlsxwriter
import pandas as pd


#scrape sklepy cynaminowe
url = 'http://brunoschulz.org/sklepy.html'
response = get(url)
soup = BeautifulSoup(response.text, 'html.parser')

all_links = []
for a in soup.find_all('a', href=True):
    all_links.append(a['href'])

pattern = re.compile("^[0-9].")
all_links = [link for link in all_links if pattern.match(link)]

story_links = []
for i in range(1,14):
	for link in all_links:
		if i <= 9:
			if i == int(link[0]):
				story_links.append(link)
				break
		if i > 9 :
			try:
				int(link[0:2])
			except:
				continue
			if i == int(link[0:2]):
				story_links.append(link)
				break


#go through each story -- some of the encodings are screwed up!! take only border-left
story_text = []

for link in story_links:
	#make call to page
	url = 'http://brunoschulz.org/'+link
	response = get(url)
	soup = BeautifulSoup(response.text, 'html.parser', from_encoding='utf-8')
	#get story text
	rows = soup.findAll("tr", {"style" : re.compile('mso-yfti-irow:*')})
	all_words = ''
	for row in rows:
		words = row.findAll("span",{"lang" : 'PL'})
		for word in words:
			all_words = all_words + ' ' + word.text
	story_text.append(all_words)

book_title = ['Sklepy Cynamonowe'] * 13
story_titles = ['Sierpień','Nawiedzenie','Ptaki','Manekiny','Traktat o Manekinach albo Wtórna Księga Rodzaju',
'Nemrod','Pan','Pan Karol','Sklepy Cynamonowe','Ulica Krokodyli','Karakony','Wichura','Noc Wielkiego Sezonu']
df = pd.DataFrame(list(zip(book_title,story_titles, story_text)), columns =['book','story_title', 'text_polish']) 


#create dataframe
##book
##story_name
##text_polish
##text_russian
##text_english
##text_spanish






entire_table = html_soup.find("table",{"class":"wikitable sortable"})
all_rows = entire_table.find("tbody").find_all("tr")
#loop to create dataframe
table_columns = ['body','image','radius_km','radius_r','volume_km',
'volume_v','mass_kg','mass_m','density','gravity_ms',
'gravity_g','type','shape','number']
jsonList = []
for row in all_rows[2:len(all_rows)-2]:
    rowList = []
    for cell in row.find_all('td'):
        rowList.append(cell.text)
    currJson = dict(zip(table_columns,rowList))
    jsonList.append(currJson)
full_df = pd.DataFrame(jsonList)




