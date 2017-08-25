build:
	cd client && elm make --warn --yes src/Routinely.elm --output elm.js

clean:
	rm -rf client/elm-stuff client/elm.js

serve-client:
	cd client && serve

serve-api:
	postgrest postgrest.conf
