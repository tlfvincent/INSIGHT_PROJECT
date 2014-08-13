import sqlite3
import os
import pandas as pdimport struct
import Image
import scipy
import scipy.misc
import scipy.cluster
from PIL import Image
import urllib2 as urllib
import io
import webcolors
from collections import Counter


def closest_colour(requested_colour):
	'''
	function that finds the closest colour to given RGB code
	'''
	min_colours = {}
	for key, name in webcolors.css3_hex_to_names.items():
		r_c, g_c, b_c = webcolors.hex_to_rgb(key)
		rd = (r_c - requested_colour[0]) ** 2
		gd = (g_c - requested_colour[1]) ** 2
		bd = (b_c - requested_colour[2]) ** 2
		min_colours[(rd + gd + bd)] = name
	return min_colours[min(min_colours.keys())]

def get_colour_name(requested_colour):
	'''
	function that finds the colour that matches input RGB code.
	If no matching colour is found, call function closest_colour
	'''
	try:
		closest_name = actual_name = webcolors.rgb_to_name(requested_colour)
	except ValueError:
		closest_name = closest_colour(requested_colour)
		actual_name = None
	return actual_name, closest_name

os.chdir('~/Desktop/MillionDollarStory/data/')

# load IMDB info in SQL database
title = []
image_link = []
db = sqlite3.connect('imdb_movie_info.db')
df = pd.read_sql("SELECT Title, Poster, Genre from MOVIE_INFO WHERE Title NOT NULL AND Poster NOT NULL", db)
db.close()

# iterate through movie posters and find top two dominant colors
NUM_CLUSTERS = 10
top_colours = []
for i in range(0, len(df)):
	print i
	# define url of image
	url_of_image = str(df['Poster'][i])
	if (url_of_image == 'N/A') or (url_of_image == 'NULL'):
		print 'skipping poster on row %s' % (i)
		top_colours.append('NULL')
	else:
	# extract pixel information from image
		fd = urllib.urlopen(url_of_image)
		image_file = io.BytesIO(fd.read())
		im = Image.open(image_file)
		im = im.resize((150, 150)) # optional, to reduce time
		ar = scipy.misc.fromimage(im)
		shape = ar.shape
		ar = ar.reshape(scipy.product(shape[:2]), shape[2])
		#print 'finding clusters'
		codes, dist = scipy.cluster.vq.kmeans(ar, NUM_CLUSTERS)
		#print 'cluster centres:\n', codes
		# extract top two colors in image
		vecs, dist = scipy.cluster.vq.vq(ar, codes) # assign codes
		counts, bins = scipy.histogram(vecs, len(codes)) # count occurrences
		top_index = scipy.argsort(counts)[::-1][:3]
		peak = codes[top_index]
		# find colour or closest colour to dominant hues in poster
		col_temp = []
		for p in peak:
			actual_name, closest_name = get_colour_name(list(p))
			if actual_name == None:
				col_temp.append(closest_name)
			else:
				col_temp.append(actual_name)
		top_colours.append(col_temp)

# find all colors
all_colors = [item for sublist in top_colours for item in sublist if sublist != 'NULL']
counts = Counter(all_colors)

# find genres for all movies
genres = []
for i in range(0, len(df)):
	genres.extend(str(df['Genre'][i]).split(', '))

# find unique genres
unique_genres = set(genres)

# for each movie, match up color scheme and genre
color_to_genre = dict()
for g in unique_genres:
	
	top_colors[i]

