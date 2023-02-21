haunt: haunt.scm
	cp ocapn/meeting-minutes/*.md posts/minutes/
	# TODO: For some reason this file breaks the build
	rm posts/minutes/2021-09-24.md
	# Build site
	haunt build

upload: haunt
	rsync --delete --recursive --verbose \
	   site/ scroll.spritely.institute:/srv/ocapn.org/site/

clean:
	rm -r site/
