#!/bin/bash

# get_dois - script loops thru a list and grabs a doi for each. output to a file.
# list='3_docids.txt';
list='ecocomDP_docids.txt';

# curl command example: 
# curl -k -X GET https://pasta.lternet.edu/package/doi/eml/edi/27
curl_head='curl -k -X GET https://pasta.lternet.edu/package/doi/eml';

output='ecocomDP_dois.txt';
rm $output;

for docid in `cat $list`
do
	echo $docid;
	# IFS='.';
	# read -ra TMP <<< "$docid";
	scope=$(echo $docid | cut -d'.' -f1);
	id=$(echo $docid | cut -d'.' -f2);
	rev=$(echo $docid | cut -d'.' -f3);

	curl_cmd=${curl_head}/${scope}/${id}/${rev};
	echo $curl_cmd;

	response=`$curl_cmd`;
	echo -n "response: ";echo "$response";
	doi=$(echo "$response" | cut -d':' -f2);
	echo $doi;
	printf "%s,%s\n" $docid $doi >> $output
done
