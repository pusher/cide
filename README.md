cide - Continuous Integration Docker Environment
================================================

`cide` is a command-line application that runs your project's CI inside of a
docker container. This allows to test locally in the same environment as
remotely.

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

TODO
----

* schema validation
* use the /cache volume
* multi-container runs
* `cide setup` to configure inside of a project
* `cide gc` to cleanup old cide builds
* travis.yml compatiblity with docker containers that map to languages
* add ways of exporting artifacts
* ENV GEM_HOME is container specific
