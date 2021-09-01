#!/usr/bin/env python3


# script converts a pasta dwca to one for gbif. 
# tasks (gbif will ignore what it cannot use. TBD if we should leave out certain files, like the report). 
# 1. rename the EML file; 
# 2. rezip and deposit [somewhere]

# 00. imports
import sys, getopt
import requests
import time
import zipfile
import os


# global vars
usage = './pasta2gbif.ph input_pkg_id'
url_head = 'https://pasta-s.lternet.edu/package/archive/eml'


# 0. process args
# print ('Number of arguments:', len(sys.argv), 'arguments.')
# print ('Argument List:', str(sys.argv) )

input_pkg_id = ''
outputfile = ''

try:
	input_pkg_id = sys.argv[1]
	print(input_pkg_id)
except:
	print(usage)
	sys.exit(2)

# 1. download package
# create archive, poll pasta to confirm it is ready.

# build URL:
[scope, docid, rev] = input_pkg_id.split(".")
url = url_head + "/" +  scope + "/" + docid + "/" + rev
print(url)

post = requests.post(url)
status1 = post.status_code
print(status1)
## 202

transaction_id = post.text
# print transaction_id
## 'archive_edi.92.3_163043157066464360'
get = requests.get(url + "/" + transaction_id)
status2 = get.status_code
print(status2)
## 200

content = get.content
type(content)
# <class 'bytes'>
file = open("temp/pkg_temp.zip", "wb")
file.write(content)
file.close()


# 2. unzip
with zipfile.ZipFile("temp/pkg_temp.zip", "r") as zip_ref:
	zip_ref.extractall("temp/foo")


# 3. rename one file
os.rename('temp/foo/edi.92.3.xml', 'temp/foo/eml.xml')

# 4. zip; save to a location for gbif
with zipfile.ZipFile('output/pkg_for_gbif.zip', 'w') as zip_ref2: 
	zip_ref2.write("temp/foo/eml.xml")
	zip_ref2.write("temp/foo/meta.xml")
	zip_ref2.close()





# works, almost. issues (zip)
# 1. new zip keeps the temp dir structure (temp/foo/eml.xml ...)
# 2. first choice is to zip contents of the dir, flat struct (no dirs). TBD if we want to leave off certain files, like the report.

# issues other: 
# best location for temp files, output. 
# mechanism for delivery to GBIF.

