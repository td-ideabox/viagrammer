../assets/index.html: Api.elm package.json 
	yarn build	
	cp -r dist/* ../assets/

Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src front/GenerateElm.hs)

run-tests: Api.elm
	elm-test
