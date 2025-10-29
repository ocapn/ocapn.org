haunt: haunt.scm
	# Build site
	LANG=en_US.utf8 haunt build

upload: haunt
	rsync --delete --recursive --verbose \
	   site/ scroll.spritely.institute:/srv/ocapn.org/site/

clean:
	rm -r site/
