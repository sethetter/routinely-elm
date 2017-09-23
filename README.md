# Routinely

Fun project for practicing Elm!

Also a habit tracking / reward system for kids.

## Uses

- Elm
- PostgreSQL
- PostgREST
- Docker Compose

## Requirements

- [asdf](https://github.com/asdf-vm/asdf) + `asdf plugin-add nodejs`
- [docker](http://docker.com/) +
  [docker-compose](https://github.com/docker/compose)

## Login

Run `user=yourusername npm run set-password` to create a password for the HTTP
basic auth. Or, you can comment out those lines in `api/nginx.conf` and
`docker-compose.yml` to leave it open.

## Run

* `asdf install`
* `npm i`
* `npm start`
* Visit localhost:8080
