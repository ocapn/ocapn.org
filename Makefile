haunt: haunt.scm
	# Fetch the meeting minutes
	mkdir tmp/
	git clone "https://github.com/ocapn/ocapn.git" tmp/ocapn
	mv tmp/ocapn/meeting-minutes/*.md posts/minutes/
	rm -rf tmp/
    # TODO: For some reason this file breaks the build
	rm posts/minutes/2021-09-24.md
	# Build site
	guix shell -m manifest.scm -- haunt build

clean:
	rm -r site/
