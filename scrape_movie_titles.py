def multipleReplace(text, wordDict):
	for word in wordDict:
		text = text.replace(word, '')
	return text

def strip_tags(html):
	s = MLStripper()
	s.feed(html)
	return s.get_data()

def movie_title_crawler():
	''' 
	This function scrapes the http://www.springfieldspringfield.co.uk/ website
	to extract all movies (and release dates) that are available.
	The results are stored in an SQL database called TABLE_MOVIE, which contains
	a table with two columns named Title and Year.
	'''
	import re
	import csv
	import string
	import urllib2
	from bs4 import BeautifulSoup
	import sqlite3

	db = sqlite3.connect('movie_titles_year.db')
	print "Opened database successfully";

	db.execute('''CREATE TABLE MOVIE_TITLE
	(ID INTEGER PRIMARY KEY,
	Title TEXT,
	Year INTEGER);''')
	print "Table created successfully";

	# define alphabet letters
	alphabet = string.ascii_uppercase
	alphabet = list(alphabet)

	# define punctuation and English dictionnary stop words
	wordDict = ['/n', '[', ']' , '(', ')', '?', '!', '.', ',', '\'', '-', '\"']

	# parse through each letter on springfield website
	for letter in alphabet:
		print 'Processing letter %s' % (str(letter))
		page = 0
		url = 'http://www.springfieldspringfield.co.uk/movie_scripts.php?order=' + str(letter)
		response = urllib2.urlopen(url)
		page_source = response.read()
		main_page = BeautifulSoup(page_source)

		# find number of pages for movies starting with given letter
		if page == 0:
			pagination = main_page.findAll('div', attrs={'class': 'pagination'})
			page_info = pagination[0].findAll('a')[-1]
			pages = page_info.contents
			pages = int(pages[0])

		# iterate through all pages for given letter
		for page in range(1, pages+1):
			print page
			url2 = 'http://www.springfieldspringfield.co.uk/movie_scripts.php?order=' + str(letter) + '&page=' + str(page)
			response = urllib2.urlopen(url)
			page_source = response.read()
			soup = BeautifulSoup(page_source)

			# grab all movie links on page
			movie_links = soup.findAll('a', attrs={'class':'script-list-item'})
			for link in movie_links:
				#print link
				# get title and release year of movie
				movie_info = link.contents
				movie_info_content = re.split('[()]', movie_info[0]) 
				title = movie_info_content[0].strip()
				title = multipleReplace(title[0], wordDict)
				year = movie_info_content[1].strip()
				# insert into SQL database
				insertstmt=("INSERT INTO MOVIE_TITLE (Title, Year) VALUES ('%s', '%s')" % (str(title), str(year)))
				db.execute(insertstmt)
	db.commit()
	db.close()

if __name__ == '__main__':
	movie_title_crawler()




