*cide* - Isolated test runner with Docker
=========================================

*cide* is a command-line tool that runs tests in an isolated (docker)
environment. It solves a problem where Jenkins workers need all the project's
dependencies (possibly conflicting) to be installed on the boxes. With *cide*
each run gets its own set of temporary docker containers which are scratched
at the end. Incidentally it is also possible to run the same `cide` command on
the developer machine and get the same build environment as on the CI. This
makes configuration iterations much shorter and allows to converge on a
working configuration faster.

Usage
-----

Go to the target project's root and run `cide init` to populate a default
`cide.yml`. This file contains all the instruction to build your project with
cide.

Once the file is configured run `cide` to execute the build. All the output
will appear in the console.

Example
-------

`cide.yml`
```yaml
---
from: "ruby:2.1"
as_root:
- apt-get update -qy && apt-get install -qy libxml2-dev
before:
  add:
  - Gemfile
  - Gemfile.lock
  run: bundle install --jobs=3 --retry=3 --deployment
run: bundle exec rspec
```

See [the cide.yml man page](man/cide.yml.1.md) for the full documentation.

Features
--------

* straighforward to use, just run `cide` inside of your project
* works on OSX with boot2docker
* integrates easily with jenkins or other CI systems
* can use linked containers for backend dependencies like MySQL or redis
* artefact export

Missing features
----------------

* Linked container readiness detection. Some containers take a while to boot
  up. Currently the `script/ci` must implement some sort of detection loop.
* Language detection: default settings per language (see Travis-CI). The
  current format might be a bit daunting for non-experts.
* Build matrix: run variations of the tests, for example with different
  versions of ruby to make sure all are supported (useful for libraries).
* Support for local docker machine

PR welcome !

Installation
------------

The current dependencies are [ruby
2.0+](https://www.ruby-lang.org/en/documentation/installation/) and [docker
1.5.0+](https://docs.docker.com/installation/#installation)

On OSX, boot2docker is automatically used if installed.

Quick OSX docker install:
```sh
brew install boot2docker
boot2docker init
boot2docker up
```

Then install the *cide* ruby gem:
```sh
gem install cide
```

Similar projects
----------------

* [Docker Compose](https://docs.docker.com/compose/) - Docker development environment
* [Travis CI](https://travis-ci.org/) - Great CI for Open Source projects
* [Construi](https://github.com/lstephen/construi) - Another ruby command-line builder

TODO
----

* Explain how to use *cide* with Jenkins
* Explain Travis CI vs *cide* + Jenkins

