out=dist/rvm.hs
utils=src/Utils.hs
rib=src/Rib.hs
vm=src/VM.hs
env=src/Env.hs
main=app/Main.hs

build: bundle
	ghc dist/rvm.hs

bundle:
	mkdir -p dist
	cp rvm.hs.in $(out)

	# Add Haskell extensions
	echo "{-# LANGUAGE LambdaCase, TupleSections, NoMonomorphismRestriction, Strict #-}" >> $(out)

	echo "\nmodule Main where" >> $(out)

	# Copy module imports
	cat $(env) $(rib) $(utils) $(vm) $(main) | grep "import " | sort | uniq >> $(out) || true

	# Copy module bodies
	echo "\n\n-- Utils" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(utils) | tail -n1 | cut -d : -f 1))) $(utils) >> $(out)
	echo "\n\n-- Rib" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(rib) | tail -n1 | cut -d : -f 1))) $(rib) >> $(out)
	echo "\n\n-- VM" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(vm) | tail -n1 | cut -d : -f 1))) $(vm) >> $(out)
	echo "\n\n-- Env" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(env) | tail -n1 | cut -d : -f 1))) $(env) >> $(out)
	echo "\n\n-- Main" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(main) | tail -n1 | cut -d : -f 1))) $(main) >> $(out)

	# Remove module imports
	sed -i '/import Utils/d' $(out)
	sed -i '/import Rib/d' $(out)
	sed -i '/import Env/d' $(out)
	sed -i '/import VM/d' $(out)

clean:
	rm dist/*
