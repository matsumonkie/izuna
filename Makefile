PACKAGE=mimizuku
STACK=stack --stack-yaml=stack-$(GHC).yaml

run:
	clear; $(STACK) exec $(PACKAGE)-exe

fast:
	clear; echo "building $(PACKAGE)"; $(STACK) build $(PACKAGE) --fast -j 2

app:
	clear; echo "building $(PACKAGE)"; $(STACK) $(PACKAGE) --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror"

devel:
	clear; LC_ALL=C.UTF-8 ghcid --command "$(STACK) ghci $(PACKAGE)" --test "DevelMain.update"

test:
	clear; echo "testing $(PACKAGE)"; LC_ALL=C.UTF-8 $(STACK) test $(PACKAGE) --ghc-options="-Werror"

watch-test:
	clear; LC_ALL=C.UTF-8 ghcid --command '$(STACK) ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec' --test 'main' --warnings

check:
	clear; LC_ALL=C.UTF-8 ghcid --command '$(STACK) ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec --ghc-options="-Werror"' --test ':main' --warnings

install:
	clear; echo "installing binary"; $(STACK) build --copy-bins

hlint:
	clear; hlint .
