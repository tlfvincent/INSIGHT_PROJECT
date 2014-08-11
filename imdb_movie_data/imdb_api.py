def multipleReplace(text, wordDict):
	for word in wordDict:
		text = text.replace(word, '')
	return text

def movie_title_year():
	# open new database to store IMDB and RT ratings data
	df = pd.DataFrame.from_csv('budget_data_from_the_numbers.txt', sep='\t', header=0, index_col=False)
	return(df)

def imdb_api():
	# define punctuation and English dictionnary stop words
	wordDict = ['/n', '[', ']' , '(', ')', '?', '!', '.', ',', '\'', '-', '\"', '\\']

	# extract movie title and release date from SQL database
	df = movie_title_year()
	movie_title = list(df['title'])
	release_date = list(df['release_date'])
	movie_year = [str(d.split('/')[2]) for d in release_date]

	# open new database to store IMDB and RT ratings data
	db = sqlite3.connect('movie_info.db')
	print "Opened database successfully";

	db.execute('''CREATE TABLE MOVIE_INFO
		(Plot BLOB,
		Rated BLOB,
		tomatoImage BLOB,
		Title BLOB,
		DVD BLOB,
		tomatoMeter BLOB,
		Writer BLOB,
		tomatoUserRating BLOB,
		Production BLOB,
		Actors BLOB,
		tomatoFresh BLOB,
		Type BLOB,
		imdbVotes BLOB,
		Website BLOB,
		tomatoConsensus BLOB,
		Poster BLOB,
		tomatoRotten BLOB,
		Director BLOB,
		Released BLOB,
		tomatoUserReviews BLOB,
		Awards BLOB,
		Genre BLOB,
		tomatoUserMeter BLOB,
		imdbRating BLOB,
		Language BLOB,
		Country BLOB,
		BoxOffice BLOB,
		Runtime BLOB,
		tomatoReviews BLOB,
		imdbID BLOB,
		Metascore BLOB,
		Response BLOB,
		tomatoRating BLOB,
		Year BLOB
		);''')

	for i in xrange(0, len(movie_title)):
	#for i in xrange(0, 20):
		print i
		title = movie_title[i]
		year = movie_year[i]
		url = 'http://www.omdbapi.com/?t=%s&y=%s&tomatoes=true' % (title, year)
		response = requests.get(url)
		data = response.json()
		# chech for TRUE response in API query
		lst = []
		if data['Response'] == 'True':
			for key in data.keys():
				lst.append(data[key])
		else:
			lst = ['NULL'] *34

		if len(lst) != 34:
			lst=[]
			lst = ['NULL'] * 34

		string_insert = ','.join('?'*len(lst))
		insertstmt = ("INSERT INTO MOVIE_INFO VALUES (%s)" %  string_insert)
		db.execute(insertstmt, lst)
	db.commit()
	db.close()

if __name__ == '__main__':
	import re
	import requests
	import json
	import string
	import urllib2
	import sqlite3
	import pandas as pd
	imdb_api()
