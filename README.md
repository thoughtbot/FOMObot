# FOMObot

A slack bot that monitors channels for message activity spikes. When activity
spikes within a channel, FOMObot posts a message to the fomo channel to let
anyone in that channel know that they could be missing out on good conversation.

## Development Environment Setup

This project uses [Docker]. You can use [Homebrew] to install Docker and its
other cli tools.

[Docker]: https://docker.com
[Homebrew]: http://brew.sh

Run:

```
./bin/setup
```

Optionally, you can install docker, docker-compose, and docker-machine (OSX), etc via the [instructions] on their website.

[instructions]: https://docs.docker.com/engine/installation

Also, Docker uses [VirtualBox] so you'll need that too.

[VirtualBox]: https://www.virtualbox.org/wiki/Downloads

Now, Make sure to [setup Docker Machine] properly if it's your first time using
it.

[setup Docker Machine]: https://docs.docker.com/machine/get-started

If you plan to update the dependencies of the project within the `fomobot.cabal`
file, then you'll also need a [Docker Hub] account and be added to the
[thoughtbot organization].

[Docker Hub]: https://hub.docker.com
[thoughtbot organization]: https://hub.docker.com/u/thoughtbot/

Run `docker login` to login to Docker using your Docker Hub creds.

## Running Locally

Run this:

```
./bin/run
```

## Updating the Dependencies

If you update the `fomobot.cabal` file. Build and push the new Docker container
to Docker Hub using the `bin/update-image` script. This script will rebuild the
docker container with the new cabal file, tag it with the `latest` tag, then
push it up to Docker Hub where other developers can take advantage of the
pre-installed dependencies.

## Deployment Environment Setup

This project deploys to [Heroku]. If you want to be able to deploy this project,
first install the Heroku Toolbelt. We also use the `jq` library for parsing JSON
in shell. Via Homebrew run:

[Heroku]: https://www.heroku.com/
[Heroku Toolbelt]: https://toolbelt.heroku.com/

```
brew update
brew install heroku-toolbelt jq
```

Create a Heroku account if you don't already have one. Request to be invited to
the `slack-fomobot` app.

Finally, login to Heroku by running `heroku login`.

## Deploying to Heroku

Simply run `bin/deploy`. This will clean and build the cabal executable, build a
docker slug, release the slug to the Heroku app.

You can monitor the logs by running `heroku logs --app slack-fomobot`.
