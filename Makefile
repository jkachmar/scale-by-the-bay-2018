# Quickstart guide for using ghcid for Haskell development
# https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html

package  = scale-by-the-bay
main_exe = demo
test_exe = tests
prof_dir = '.stack-work-profiling'

# Use GHC options informed by this blog post:
# https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/
ghc_opts   = -j +RTS -A128m -RTS

# Use the top-level stack.yaml file
stack_yaml = STACK_YAML="stack.yaml"
stack      = $(stack_yaml) stack

# Standard build (implies default optimization settings)
build:
	$(stack) build $(package) \
	--ghc-options='$(ghc_opts)'

# Force rebuild of packages, even when it doesn't seem necessary, based on file
# dirtiness
build-dirty:
	$(stack) build $(package) \
	--ghc-options='$(ghc_opts)' \
	--force-dirty

# Fast build (implies O0 optimizations)
build-fast:
	$(stack) build $(package) \
	--ghc-options='$(ghc_opts)' \
	--fast

# Compile a profiling build, caching artifacts in a separate work directory
# TODO(jkachmar): Figure out if we should be cleaning before profiling runs
build-profile:
	$(stack) build \
	--work-dir $(prof_dir) \
	--profile \
	--ghc-options='$(ghc_opts)'

# Clean the normal working directory
clean:
	$(stack) clean

# Clean the profiling directory, as specified in $(prof_dir)
clean-profile:
	$(stack) clean --work-dir $(prof_dir)

# Create a ghci REPL for the entire project
ghci:
	$(stack) ghci $(package):lib $(package):test:$(test_exe) \
	--ghci-options='-fobject-code $(ghc_opts)' \
	--main-is $(package):$(main_exe)

# Run ghcid against the entire project
ghcid:
	$(stack) exec -- ghcid \
	--command "stack ghci \
		$(package):lib $(package):test:$(test_exe) \
		--ghci-options='-fobject-code $(ghc_opts)' \
		--main-is $(package):$(main_exe)"

# Have ghcid run the test suite on successful recompilation
ghcid-test:
	$(stack) exec -- ghcid \
	--command "stack ghci \
		$(package):lib $(package):test:$(test_exe) \
		--ghci-options='-fobject-code $(ghc_opts)'" \
  --test "main"

# Print documentation
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

test:
	$(stack) test $(package) --ghc-options='$(ghc_opts)'

.PHONY: build build-dirty build-fast build-profile clean clean-profile ghci ghcid ghcid-test help test
