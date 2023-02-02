haunt: haunt.scm
	cp ocapn/meeting-minutes/*.md posts/minutes/
    # TODO: For some reason this file breaks the build
	rm posts/minutes/2021-09-24.md
	# Build site
	haunt build

clean:
	rm -r site/
