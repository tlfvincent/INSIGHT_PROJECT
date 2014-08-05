def multipleReplace(text, wordDict):
	'''
	basic function to replace multiple word instances in sentence
	'''
	for word in wordDict:
		text = text.replace(word, '')
	return text

def StripTags(html):
	s = MLStripper()
	s.feed(html)
	return s.get_data()

def ReadEmotionData():
	'''
	basic function to read and return AFFIN word sentiment dataset
	'''
	cr = csv.reader(open("AFINN-111.txt","rb"), delimiter=',')
	word_score = dict()
	for row in cr:
		word_score[row[0]] = row[1]
	return word_score

def ExtractMovieTitle():
	'''
	this functions parser through each movie script in the springfieldspringfield.co.uk
	website and extracts overall sentiment feelins
	'''
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

def MovieSentimentCrawler():
	'''
	this functions parser through each movie script in the springfieldspringfield.co.uk
	website and extracts overall sentiment feelins
	'''
	tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
	stopset = set(stopwords.words('english'))
	wordDict = ['/n', '?', '!', '.', ',', '\'', '-', '\"']

	# extract lexicon of word polarity
	word_score = ReadEmotionData()

	db = sqlite3.connect('movie_sentiment_AFFIN.db')
	print "Opened database successfully";

	db.execute('''CREATE TABLE MOVIE_SENTIMENT_AFFIN
		(title TEXT,
		title_url TEXT,
		year INTEGER,
		pos_sum INTEGER,
		neg_sum INTEGER,
		feeling_sum INTEGER,
		feeling_mean FLOAT,
		feeling_var FLOAT);''')
	print "Table created successfully";

	# define alphabet letters
	alphabet = string.ascii_uppercase
	alphabet = list(alphabet)

	# parse through each letter on springfield website
	for letter in alphabet:
		print 'Processing letter %s' % (str(letter))
		page_init = 0
		url = 'http://www.springfieldspringfield.co.uk/movie_scripts.php?order=' + str(letter)
		response = urllib2.urlopen(url)
		page_source = response.read()
		main_page = BeautifulSoup(page_source)

		# find number of pages for movies starting with given letter
		if page_init == 0:
			pagination = main_page.findAll('div', attrs={'class': 'pagination'})
			page_info = pagination[0].findAll('a')[-1]
			pages = page_info.contents
			pages = int(pages[0])
			page_init += 1

		for page in range(1, pages+1):
			print page
			# parse through every page for given letter
			url2 = 'http://www.springfieldspringfield.co.uk/movie_scripts.php?order=' + str(letter) + '&page=' + str(page)
			response = urllib2.urlopen(url2)
			page_source = response.read()
			soup = BeautifulSoup(page_source)

			# grab all movie links on page
			movie_links = soup.findAll('a', attrs={'class':'script-list-item'})
			for link in movie_links:

				href_movie = link.get('href')
				url_movie = 'http://www.springfieldspringfield.co.uk' + str(href_movie)
				# extract mvie url title
				url_title = href_movie.split('=')[1]

				# get movie title and year of release
				movie_info = link.contents
				movie_info_content = re.split('[()]', movie_info[0]) 
				title = movie_info_content[0].strip()
				title = multipleReplace(title, wordDict)
				
				# convert title to ASCII format
				title = filter(lambda x: x in string.printable, title)
				year = re.search('(\d{4})', movie_info[0]).group()
				print title

				html_test = requests.get(url_movie)
				if html_test != 200:
					corpus_sentiment = []
					corpus_sentiment.append(title)
					corpus_sentiment.append(url_title)
					corpus_sentiment.append(year)
					corpus_sentiment.append('NULL')
					corpus_sentiment.append('NULL')
					corpus_sentiment.append('NULL')
					corpus_sentiment.append('NULL')
					corpus_sentiment.append('NULL')
					# insert into SQLITE database
					db.execute('INSERT INTO MOVIE_SENTIMENT_AFFIN VALUES (?, ?, ?, ?, ?, ?, ?, ?)', corpus_sentiment)
				else:
					response = urllib2.urlopen(url_movie)
					page_source = response.read()
					page_source = re.sub('<br>',' ',page_source)
					soup = BeautifulSoup(page_source)

					# extract text lines from html
					movie_script = soup.findAll('div', attrs={'class': 'scrolling-script-container'})
					lines = movie_script[0].contents

					# splits all script into individual sentences
					sentences = tokenizer.tokenize(str(lines))

					# delete last element of javascript content
					del sentences[-1]
					sentences = [w.lower() for w in sentences]
					sentences = [w.replace('\\', '') for w in sentences]
					new_sentence = []
					
					# remove punctuation
					for sentence in sentences:
						new_sentence.append(multipleReplace(sentence.strip(), wordDict))

					# find frequency of words in episode and print to file
					word_distribution = FreqDist(new_sentence)
					positive_sentiment = []
					negative_sentiment = []
					all_sentiment = []
					for word in word_distribution.keys():
						if word in word_score.keys():
							#print '%s, %s, %s' % (str(word), str(word_distribution[word]), str(sentiment_sum))
							sentiment_sum = int(word_distribution[word]) * int(word_score[word])
							all_sentiment.append(sentiment_sum)
							if sentiment_sum > 0:
								positive_sentiment.append(sentiment_sum)
							elif sentiment_sum <0 :
								negative_sentiment.append(sentiment_sum)

					# compuet sentiment data for movie script
					corpus_sentiment = []
					corpus_sentiment.append(title)
					corpus_sentiment.append(url_title)
					corpus_sentiment.append(year)
					corpus_sentiment.append(np.sum(positive_sentiment))
					corpus_sentiment.append(np.sum(negative_sentiment))
					corpus_sentiment.append(np.sum(all_sentiment))
					corpus_sentiment.append(np.mean(all_sentiment))
					corpus_sentiment.append(np.var(all_sentiment))

					# insert into SQLITE database
					db.execute('INSERT INTO MOVIE_SENTIMENT_AFFIN VALUES (?, ?, ?, ?, ?, ?, ?, ?)', corpus_sentiment)

	db.commit()
	db.close()

if __name__ == '__main__':
	import re
	import csv
	import string
	import urllib2
	import pickle
	import sqlite3
	import numpy as np
	import pandas as pd
	from bs4 import BeautifulSoup
	from nltk import FreqDist
	from nltk import word_tokenize
	from nltk.corpus import stopwords
	import nltk.data
	import json, requests
	MovieSentimentCrawler()
