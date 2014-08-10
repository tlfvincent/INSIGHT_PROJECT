def multipleReplace(text, wordDict):
	for word in wordDict:
		text = text.replace(word, '')
	return text

def movie_title_year():
	# open and parse SQL database
	db = sqlite3.connect('movie_titles_year.db')
	cursor = db.execute('SELECT * from MOVIE_TITLE')
	# add movie title and year to dictionnary
	query_movie = []
	query_year = []
	for row in cursor:
		query_movie.append(row[1])
		query_year.append(row[2])
	# close database
	db.close()
	results = []
	results.append(query_movie)
	results.append(query_year)
	return(results)

def imdb_api():
	import re
	import requests
	import string
	import urllib2
	import sqlite3 


	# define punctuation and English dictionnary stop words
	wordDict = ['/n', '[', ']' , '(', ')', '?', '!', '.', ',', '\'', '-', '\"', '\\']

	# extract movie title and release date from SQL database
	query_movie = movie_title_year()

	# open new database to store IMDB and RT ratings data
	#db = sqlite3.connect('movie_info.db')
	#print "Opened database successfully";

	#db.execute('''CREATE TABLE MOVIE_INFO
	#	(ID INTEGER PRIMARY KEY,
	#	title TEXT,
	#	year INTEGER,
	#	rated BLOB,
	#	tomatoMeter INTEGER,
	#	imdbVotes BLOB,
	#	tomatoConsensus TEXT,
	#	director TEXT,
	#	released INTEGER,
	#	tomatoUserReviews BLOB,
	#	genre TEXT,
	#	imdbRating INTEGER,
	#	boxOffice BLOB,
	#	tomatoRating INTEGER
	#	);''')

	#print "Table created successfully";
	target_keys = ['Genre', 'tomatoMeter', 'tomatoRating', 'tomatoUserRating', 'imdbRating', 'BoxOffice']
	f = open('imdb_rt_api.tab', 'w')
	for i in xrange(0, len(query_movie[0])):
	#for i in xrange(0, 20):
		print i
		title = query_movie[0][i]
		year = query_movie[1][i]
		url = 'http://www.omdbapi.com/?t=%s&y=%s&tomatoes=true' % (title, year)
		response = requests.get(url)
		data = response.json()
		# chech for TRUE response in API query
		if data['Response'] == 'True':
			lst = []
			for key in target_keys:
				if key in data.keys():
					lst.append(data[key])
				else:
					lst.append('0')
				line = '\t'.join(lst)
			f.write(line + '\n')
			#insert all data into SQL database
		#	insertstmt=("INSERT INTO MOVIE_INFO (title, year, rated, tomatoMeter, imdbVotes, tomatoConsensus, director, released, tomatoUserReviews, genre, imdbRating, boxOffice, tomatoRating) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')" % (str(title), str(year), str(data['Rated']), str(data['tomatoMeter']),  str(data['imdbVotes']), str(data['tomatoConsensus']), str(data['Director']), str(data['Released']), str(data['tomatoUserReviews']), str(data['Genre']), str(data['imdbRating']), str(data['BoxOffice']), str(data['tomatoRating'])))
		#	db.execute(insertstmt)
		else:
			lst = []
			for i in xrange(0, len(target_keys) + 1):
				lst.append('NULL')
			line = '\t'.join(lst)
			f.write(line + '\n')
		#	insertstmt=("INSERT INTO MOVIE_INFO (title, year, rated, tomatoMeter, imdbVotes, tomatoConsensus, director, released, tomatoUserReviews, genre, imdbRating, boxOffice, tomatoRating) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')" % (str(title), str(year), 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL'))
	#db.commit()
	#db.close()
	f.close()

if __name__ == '__main__':
	import re
	import requests
	import string
	import urllib2
	import sqlite3 
	imdb_api()
