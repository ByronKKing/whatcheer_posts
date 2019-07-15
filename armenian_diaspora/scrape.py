from requests import get
from bs4 import BeautifulSoup
import json
import numpy as np
import xlsxwriter
import re

#scrape table
url = 'https://en.wikipedia.org/wiki/Largest_Armenian_diaspora_communities'
response = get(url)
html_soup = BeautifulSoup(response.text, 'html.parser')
entire_table = html_soup.find("table",{"class":"wikitable plainrowheaders sortable"})
all_rows = entire_table.find("tbody").find_all("tr")


#loop to create dataframe
table_columns = ['number','area','country'
,'official_data','estimations','largest_community']
jsonList = []

for row in all_rows[1:len(all_rows)]:
    rowList = []
    for cell in row.find_all('td'):
        rowList.append(cell.text)
    currJson = dict(zip(table_columns,rowList))
    jsonList.append(currJson)
full_df = pd.DataFrame(jsonList)

#process dataframe
df = full_df

df.head(1)


df.country = df.country.str.strip('\n\t')
df.area = df.area.str.strip('\n\t')

df['census_year'] = df.official_data.apply(lambda x: x[x.find("(")+len("("):x.rfind(")")])
df['census_year'] = df.census_year.apply(lambda x: re.sub('[^0-9]','', x))

df['official_data'] = df.official_data.apply(lambda x: x.split("(")[0])
df['official_data'] = df.official_data.apply(lambda x: re.sub('[^0-9]','', x))

##strip whitespace and paragraph characters, remove commas
df = df.apply(lambda x: x.str.strip() if x.dtype == "object" else x)
df = df.apply(lambda x: x.str.strip('\n\t') if x.dtype == "object" else x)
df[list(df.columns)] = df[list(df.columns)].replace({',': ''}, regex=True)
##process radius_km with regex
df.radius_km = df.radius_km.apply(lambda x: x.split("♠")[1])
def regex_func(x):
    x = re.sub("[\(\[].*?[\)\]]", "", x)
    return(x)
df.radius_km = df.radius_km.map(regex_func)
df = df.apply(lambda x: x.map(regex_func) if x.dtype == "object" else x)