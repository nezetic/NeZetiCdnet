all:
	stack build
	stack exec nezetic.net -- build

clean:
	stack clean
	rm -rf _site _cache

deploy:
	stack exec nezetic.net -- deploy

watch:
	stack exec nezetic.net -- watch

# HLint the generator sources (add `-- --refactor` to auto-fix).
lint:
	stack exec --package hlint -- hlint src

# Build the generator and lint it in one go.
check: all lint

.PHONY: all clean deploy watch lint check
