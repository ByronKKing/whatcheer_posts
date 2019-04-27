from requests import get
from bs4 import BeautifulSoup
import json
import numpy as np
import xlsxwriter

#scrape table
url = 'https://en.wikipedia.org/wiki/List_of_Solar_System_objects_by_size'
response = get(url)
html_soup = BeautifulSoup(response.text, 'html.parser')

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

full_df.head(5)

#process dataframe
df = full_df

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
df.head(20)

##write to csv
df.to_csv('~/solar_system/processed_data.csv',index=False)