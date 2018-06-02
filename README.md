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

Create a new app for this project:

```
heroku apps:create YOUR_APP_NAME
heroku scale web=0 worker=1
```

Set the same environment variables in your Heroku app as you have in `.env` (excluding `HEROKU_APP_NAME`).

Finally, install the Heroku container tools plugin:

```
heroku plugins:install heroku-container-tools
```

## Deploying to Heroku

Confirm `HEROKU_APP_NAME` is set correctly in `.env` and run `bin/deploy`.

## Contributing

See the [CONTRIBUTING] document. Thank you, [contributors]!

[CONTRIBUTING]: CONTRIBUTING.md
[contributors]: https://github.com/thoughtbot/FOMObot/graphs/contributors

## License

FOMObot is Copyright (c) 2016 thoughtbot, inc. It is free software, and may be
redistributed under the terms specified in the [LICENSE] file.

[LICENSE]: /LICENSE

## About

![thoughtbot](https://thoughtbot.com/logo.png)

FOMObot is maintained and funded by thoughtbot, inc. The names and logos for
thoughtbot are trademarks of thoughtbot, inc.

We love open source software! See [our other projects][community] or look at
our product [case studies] and [hire us][hire] to help build your Haskell app.

[community]: https://thoughtbot.com/community?utm_source=github
[case studies]: https://thoughtbot.com/work?utm_source=github
[hire]: https://thoughtbot.com/hire-us?utm_source=github
