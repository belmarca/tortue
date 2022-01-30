out=dist/rvm.hs
utils=src/Utils.hs
rib=src/Rib.hs
vm=src/VM.hs
env=src/Env.hs

build: bundle
	ghc dist/rvm.hs

bundle:
	mkdir -p dist
	cp rvm.hs.in $(out)

	# Copy module languages
	grep "LANGUAGE" $(utils) >> $(out) || true
	grep "LANGUAGE" $(rib) >> $(out) || true
	grep "LANGUAGE" $(vm) >> $(out) || true

	echo "\nmodule Main where" >> $(out)

	# Copy module imports
	grep "import " $(utils) >> $(out) || true
	grep "import " $(rib) >> $(out) || true
	grep "import " $(vm) >> $(out) || true
	grep "import " $(env) >> $(out) || true

	# Copy module bodies
	echo "\n\n-- Utils" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(utils) | tail -n1 | cut -d : -f 1))) $(utils) >> $(out)
	echo "\n\n-- Rib" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(rib) | tail -n1 | cut -d : -f 1))) $(rib) >> $(out)
	echo "\n\n-- VM" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(vm) | tail -n1 | cut -d : -f 1))) $(vm) >> $(out)
	echo "\n\n-- Env" >> $(out)
	tail -n +$$((1+$(shell grep -n "import" $(env) | tail -n1 | cut -d : -f 1))) $(env) >> $(out)

	# Remove module imports
	sed -i '/import Utils/d' $(out)
	sed -i '/import Rib/d' $(out)
	sed -i '/import Env/d' $(out)

	echo "\n\nmain :: IO ()" >> $(out)
	echo "main = do" >> $(out)
	echo "    putStrLn \"RVM code:\" ;" >> $(out)
	echo "    putStrLn \");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y\" ; -- RVM code that prints HELLO!" >> $(out)
	echo "    return ()" >> $(out)

clean:
	rm dist/*
