cide - Continuous Integration Docker Environment
================================================

`cide` makes it easy to reproduce CI builds on the developer computer by
providing the same docker environment.

Run `cide` in the project root to run a build. Configure by providing a
`.cide.yml` file.

Usage
-----

Just run `cide` inside of your project. cide will look for a .cide.yml file
for configuration but all arguments are also passable trough command-line
arguments. If a Dockerfile already exists it will be used instead.

Example
-------

`.cide.yml`
```
---
image: "ruby:2.1"
as_root:
- apt-get update -qy && apt-get install -qy libxml2-dev
command: bundle && bundle exec rspec
```

Features
--------

* straighforward to use, just run `cide` inside of your project
* works on OSX with boot2docker
* integrates easily with jenkins or other CI systems

Limitations
-----------

A temporary Dockerfile has to be created in the project's root because docker
doesn't allow referencing files outside of the directory (even with a symlink)

Docker version 1.5.0+ is required

Installation
------------

Install docker: https://docs.docker.com/installation/#installation

```
gem install cide
```

OSX docker install:
```
brew install boot2docker
boot2docker init
boot2docker up
# cide auto-detects boot2docker on OSX
```

TODO
----

* linked container
* schema validation
* cleaner output. on error is just outputs a backtrace

