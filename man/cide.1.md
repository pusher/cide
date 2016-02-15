CIDE 1 "JUNE 2015" cide "Cide Manual"
======================================

NAME
----

cide - Isolated test runner with Docker

SYNOPSIS
--------

*cide* [`-f`|`--force`] [`-p`|`--pretend`|`--no-pretend`]
     [`-q`|`--quiet`|`--no-quiet`] [`-s`|`--skip`|`--no-skip`]
     <*command*> [<*args*>]

DESCRIPTION
-----------

*cide* is a command-line tool that executes tests in isolation using a
container (through Docker).

The default *cide build* command reads a `.cide.yml` file in the current
folder and takes its content to build a temporary Dockerfile that imports all
of the current directory's structure, builds an image with it and then runs a
container from it that executes the specified tests. See the cide.yml(1) man
page for the structure of that file.

Cide can also be used to package and upload projects after they have been
built. See cide-package(1) for more information.

OPTIONS
-------

These are global options that set the behavior of all commands.

`-f`, `--force`
  Overwrite files that already exist

`-p`, `--pretend`, `--no-pretend`
  Run but do not make any changes

`-q`, `--quiet`, `--no-quiet`
  Suppress status output

`-s`, `--skip`, `--no-skip`
  Skip files that already exist

COMMANDS
--------

*cide exec* [<*options*>]
  Builds an image and executes the run script. This is the default command
  when none is provided.

  See cide-package(1)

*cide package* [<*options*>]
  Packages a directory that resulting from a run script. Can also upload the
  package to Amazon s3.

  See cide-package(1)


*cide clean* [<*options*>]
  Removes old containers.

  See cide-clean(1)

*cide debug* [<*options*>]
  Opens a debug console in the last project image

  See cide-debug(1)

*cide help* [<*command*>]
  Shows the general help or the one for the given *command*

*cide init*
  Creates a blank `cide.yml` file in the project.

  See cide-init(1)

FILES
-----

*cide.yml*
  Per project build and test settings. See cide.yml(1)

ENVIRONMENT
-----------

The following environment variables are used by the `docker` client to connect
to the `docker` daemon. See also
https://docs.docker.com/docker/reference/commandline/cli/#environment-variables:b659b046131d4024ab5e2d3675716bf0

*DOCKER_HOST*
  Daemon socket to connect to.

*DOCKER_CERT_PATH*
  The location of your authentication keys.

*DOCKER_TLS_VERIFY*
  When set Docker uses TLS and verifies the remote.


The following environment variables are used by the Amazon `s3` SDK to
authenticate, and should be set by the user. See also
http://docs.aws.amazon.com/sdkforruby/api/index.html.

*S3_USER*
  The Amazon s3 username

*S3 PASS*
  The amazon s3 pass

CONTRIBUTE
----------

Bug reports, contributions and forks are welcome.

All bugs or other forms of discussion happen on
<http://github.com/zimbatm/cide/issues>

There is also a wiki available where you can share your usage patterns or
other tips and tricks <https://github.com/zimbatm/cide/wiki>

COPYRIGHT
---------

Copyright (C) 2015 zimbatm and contributors under the MIT licence.

SEE ALSO
--------

cide.yml(1)
