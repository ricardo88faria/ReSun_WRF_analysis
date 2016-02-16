.PHONY: run tail kill

run:
	./ReSun_analysis.R > log.out

tail:
	tail -f log.out

kill:
	killall R
