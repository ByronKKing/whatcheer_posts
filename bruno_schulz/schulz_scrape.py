from requests import get
from bs4 import BeautifulSoup
import json
import numpy as np
import xlsxwriter
import pandas as pd
import re


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


#go through each story -- take only border-left!
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


#scrape sanatorium pod klepysdra
url = 'http://brunoschulz.org/sanatorium.html'

story_links = ['http://brunoschulz.org/01-knjiga.html','http://brunoschulz.org/2-genijalna-epoha.html',
'http://brunoschulz.org/3-prolece.html','http://brunoschulz.org/4-julska-noc.html','http://brunoschulz.org/5-moj-otac.html',
'http://brunoschulz.org/drugajesen.htm','http://brunoschulz.org/7-mrtva-sezona.html',
'http://brunoschulz.org/8-sanatorijum.html','http://brunoschulz.org/9-dodo.html',
'http://brunoschulz.org/10-edjo.html','http://brunoschulz.org/11-penzioner.html','http://brunoschulz.org/samotnosc.htm',
'http://brunoschulz.org/13-ostatnia-eng.htm']

#take story text
story_text = []

for link in story_links:
	#make call to page
	response = get(link)
	soup = BeautifulSoup(response.text, 'html.parser', from_encoding='utf-8')
	#get story text
	rows = soup.findAll("tr", {"style" : re.compile('mso-yfti-irow:*')})
	rows = [row.findAll("td", {"style" : re.compile('width:50.0%;border:none;border-left:*')}) for row in rows] #fix this to catch different style elements
	rows = [row for row in rows if row]
	all_words = ''
	for row in rows:
		try:
			words = row[0].find("span",{"lang" : 'PL'}).text
			all_words = all_words + ' ' + words
		except:
			continue
	story_text.append(all_words)

book_title = ['Sanatorium Pod Klepysdrą'] * 13
story_titles = ['Księga','Genialna Epoka ','Wiosna','Noc lipcowa','Mój ojciec wstępuje do strażaków',
'Druga jesień','Martwy sezon','Sanatorium pod Klepsydrą','Dodo','Edzio','Emeryt','Samotność',
'Ostatnia ucieczka ojca']
df2 = pd.DataFrame(list(zip(book_title,story_titles, story_text)), columns =['book','story_title', 'text_polish']) 

finaldf = pd.concat([df,df2],ignore_index=True)

finaldf.to_excel("~/whatcheer_posts/bruno_schulz/schulz_stories.xlsx")


#create dataframe
##book
##story_name
##text_polish
##text_russian
##text_english
##text_spanish
