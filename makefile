build:
	elm make --warn --yes src/Routinely.elm --output elm.js

clean:
	rm -rf elm-stuff elm.js

serve-api:
	postgrest api/postgrest.conf
