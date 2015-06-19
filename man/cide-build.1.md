CIDE-BUILD 1 "JUNE 2015" cide-build "Cide Manual"
=================================================

NAME
----

cide-build - Builds an image and executes the run script

SYNOPSIS
--------

*cide build* [`-n`|`--name`=<*name*>] [`--export`|`--no-export`]
             [`-o`|`--export-dir`=<*path*>] [`-r`|`--run`=<*command*>]
             [`--pull`|`--no-pull`] [`-s`|`--ssh-key`=<*path*>]

DESCRIPTION
-----------

This is the default command that does most of the work and is invoked 99% of
the command.

When invoked it will read the `cide.yml` file, generate a temporary
`Dockerfile.cide` file, build it with *docker build*, start all the linked
containers and finally run the selected script. When that script exits all the
running containers are torn down. This loop guarantees a blank slate for every
CI run with isolated dependencies.

OPTIONS
-------

`-n`, `--name`=<*name*>
  Name of the built image. This can be used to differentiate between branches
  or others in a CI environment. By default the name is the basename(1) of the
  current directory.

`--export`, `--no-export`
  Whenever to export artifacts. If `--export` is passed and the `export:`
  directive is set in the `cide.yml` file, artifacts from that directory are
  transfered back outside of the container and into the current directory.

`-o`, `--export-dir`=<*path*>
  By default the export directory is the same as the one given in the
  `export:` directive in the `cide.yml` file. It's possible to change that
  target folder outside of the container.

`-r`, `--run`=<*command*>
  Override the script to run. Usually the `run:` directive in the `cide.yml`
  is defining what to run but this could be used for a quick one-off.

`--pull`, `--no-pull`
  Whenever to pull for new images on build. If set it will try to fetch new
  versions of the `from:` directive. It can be useful to get new versions of
  an image but also invalidates all the cache.

`-s`, `--ssh-key`=<*path*>
  When the `use_ssh:` directive is set to true, where to get the ssh key to
  inject into the container. Defaults to `~/.ssh/id_rsa`.

SEE ALSO
--------

cide(1), cide.yml(1)
