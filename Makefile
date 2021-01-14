PACKAGE=izuna-$(APP)
STACK=stack --stack-yaml=stack-$(GHC).yaml

run:
	clear; $(STACK) exec $(PACKAGE)-exe

fast:
	clear; LC_ALL=C.UTF-8 echo "building $(PACKAGE)"; $(STACK) build $(PACKAGE) --fast -j 2

build:
	clear; echo "building $(PACKAGE)"; LC_ALL=C.UTF-8 $(STACK) build $(PACKAGE) --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror"

# there's a bug with -Wunused-packages and ghcid (ghcid fails to reload when unused-packages is present in package.yaml)
check-unusued:
	clear; echo "building $(PACKAGE)"; LC_ALL=C.UTF-8 $(STACK) build $(PACKAGE) --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror -Wunused-packages"

clean:
	clear; LC_ALL=C.UTF-8 echo "cleaning $(PACKAGE)"; $(STACK) clean $(PACKAGE)

devel:
	clear; LC_ALL=C.UTF-8 ghcid --command "$(STACK) ghci $(PACKAGE)" --test "DevelMain.update"

test:
	clear; LC_ALL=C.UTF-8 echo "testing $(PACKAGE)";  $(STACK) test $(PACKAGE) --ghc-options="-Werror"

watch-test:
	clear; LC_ALL=C.UTF-8 ghcid --command '$(STACK) ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec' --test 'main' --warnings

check:
	clear; LC_ALL=C.UTF-8 ghcid --command '$(STACK) ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec --ghc-options="-Werror"' --test ':main' --warnings

install:
	clear; echo "installing binary"; LC_ALL=C.UTF-8 $(STACK) build $(PACKAGE) --copy-bins

hlint:
	clear; hlint .
