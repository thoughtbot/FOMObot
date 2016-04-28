# FOMObot

A Slack bot that monitors channels for message activity spikes. When activity
spikes within a channel, FOMObot posts a message to the `#fomo` channel to let
anyone in that channel know that they could be missing out on an important
conversation.

## Development Environment Setup

This project uses [Stack] to build and run locally. You can install Stack via
the [instructions on their site.](http://docs.haskellstack.org/en/stable/README/)

[Stack]: http://docs.haskellstack.org/en/stable/README/

Then run:

```
bin/setup
```

FOMObot needs a Slack API token. You can create a new Slack bot and
generate an API token [here](https://my.slack.com/services/new/bot).

Insert the Slack API token into the `.env` file.

```
SLACK_API_TOKEN=your_token_goes_here
...
```

## Running Locally

Run this:

```
bin/run
```

## Deployment Environment Setup

This project uses [Docker] to build for deployment. You can install docker and
docker-compose via the [instructions] on their website.

[Docker]: https://docker.com
[instructions]: https://docs.docker.com/engine/installation

If you're using OS X, you will also need docker-machine. Make sure to [setup
Docker Machine] properly if it's your first time using it.

[setup Docker Machine]: https://docs.docker.com/machine/get-started

FOMObot can be easily deployed to [Heroku]. If you would also like to deploy to
Heroku then start by creating a Heroku account if you don't already have one.
Next, install the [Heroku Toolbelt].

[Heroku]: https://www.heroku.com/
[Heroku Toolbelt]: https://toolbelt.heroku.com/

Log in to Heroku by running `heroku login`.

Create a new app for this project by running `heroku apps:create YOUR_APP_NAME`.

Set the same environment variables in your Heroku app as you have in `.env`.

Finally, install the `heroku-docker` plugin.

```
heroku plugins:install heroku-docker
```

## Deploying to Heroku

Simply run `heroku docker:release`.
