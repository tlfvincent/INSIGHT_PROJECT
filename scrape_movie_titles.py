def multipleReplace(text, wordDict):
	for word in wordDict:
		text = text.replace(word, '')
	return text

def strip_tags(html):
	s = MLStripper()
	s.feed(html)
	return s.get_data()

def movie_title_crawler():
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
	wordDict = ['/n', '[', ']' , '(', ')', '?', '!', '.', ',', '\'', '-', '\"', '\\']

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
				#print link
				# get title and release year of movie
				movie_info = link.contents
				movie_info_content = re.split('[()]', movie_info[0]) 
				title = movie_info_content[0].strip()
				title = multipleReplace(title, wordDict)
				# convert title to ASCII format
				title = filter(lambda x: x in string.printable, title)
				year = re.search('(\d{4})', movie_info[0]).group()
				#year = movie_info_content[1].strip()
				insertstmt=("INSERT INTO MOVIE_TITLE (Title, Year) VALUES ('%s', '%s')" % (str(title), str(year)))
				db.execute(insertstmt)
	db.commit()
	db.close()

if __name__ == '__main__':
	movie_title_crawler()



