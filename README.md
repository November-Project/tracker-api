# November Project Tracker API

This is the API backend for the [November Project Tracker app].

[November Project Tracker app]: https://tracking.november-project.com

## First Time Setup

The Tracker API uses [Docker] to have a consistent development environment.
Mac users can use [Homebrew] to install `docker-machine`, `docker`, and
`docker-compose` or visit the [Docker installation documentation] for more
information. Non-Mac users should also see the [Docker installation
documentation] for instructions regarding their development environment.

[Docker]: https://www.docker.com
[Homebrew]: http://brew.sh
[Docker installation documentation]: https://docs.docker.com/engine/installation/

Once Docker is setup correctly, simply run `docker-compose up` to run the
project. This might take some time to run initially because Docker needs to
build the container images with all the dependencies needed for the project.

## Seed Data

The API requires seed data for the tribes in the database before anything will
function properly. To add seed data, log into the [Postgresql] database using
`docker-compose run web psql -U postgres`. This will start the Docker container
and connect to the [Postgresql] database. You can then insert tribe data like
so:

[Postgresql]: http://www.postgresql.org/

```sql
INSERT INTO tribes VALUES (default, 'Boston, MA', '[1,3,5]', 42.358431, -71.059773, 'America/New_York');
```

## Without Docker

This project now uses Stackage. You can install stack view Homebrew `brew
install haskell-stack`. Then run the setup and build commands.

```
stack setup
stack build
stack install yesod-bin
```

Setup postgres via psql:

```
createuser novproject
psql postgres
CREATE DATABASE novproject_api;
GRANT ALL PRIVILEGES ON DATABASE novproject_api TO novproject;
\q
```

Run the app:

```
stack exec yesod devel
```

Seed the database:

```
psql novproject_api -U novproject -f database/seed.sql
```

## Contributing

If you'd like to contribute, please look at the issues for things to do, or
create your own issues and PRs for things you'd like fixed, improved, or added.
Please follow the [Contributing guide] and [Code of Conduct] when contributing.
Thank you, [contributors]!

[Contributing guide]: CONTRIBUTING.md
[Code of Conduct]: CODE_OF_CONDUCT.md
[contributors]: https://github.com/november-project/tracker-api/graphs/contributors

## License

If not otherwise specified (see below), files in this repository fall under the
following license:

[License](LICENSE)

An exception is made for files in readable text which contain their own license
information.
