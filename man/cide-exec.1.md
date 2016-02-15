CIDE-EXEC 1 "FEBRUARY 2016" cide-exec "Cide Manual"
===================================================

NAME
----

cide-exec - Builds an image and executes the run script

SYNOPSIS
--------

*cide exec*  [`--export`|`--no-export`] [`-o`|`--export-dir`=<*path*>]
             [`--guest_export_dir`=<*path*>] [`-r`|`--run`=<*command*>]
             [`--pull`|`--no-pull`] [`-s`|`--ssh-key`=<*path*>]

DESCRIPTION
-----------

This is the default command that does most of the work and is invoked most of
the time.

When invoked it will read the `cide.yml` file, generate a temporary
`Dockerfile.cide` file, build it with *docker build*, start all the linked
containers and finally run the selected script. When that script exits all the
running containers are torn down. This loop guarantees a blank slate for every
CI run with isolated dependencies.

OPTIONS
-------

`-n`, `-t`, `--name`=<*name*>
  Name of the built image. This can be used to differentiate between branches
  or others in a CI environment. By default the name is the basename(1) of the
  current directory.

`--export`, `--no-export`
  Whether to export artifacts. If `--export` is passed and the directory to
  be exported is set (either with `export_dir:` in the `cide.yml` file, or by
  specifying `--guest-export-dir` on the command line) artifacts from that
  directory are transfered back outside of the container (see `--export-dir`
  and `--guest-export-dir`)

`-o`, `--export-dir`=<*path*>
  Where the directory extracted from the container should be placed on the
  host.  By default the export directory is the same as the one given in the
  `export_dir:` directive in the `cide.yml` file. It is possible to change that
  target folder outside of the container.

`--guest-export-dir`=<*path*>
  Which directory from the container should be extracted.  By default the
  export directory is the same as the one specified with `--export-dir` if any,
  or in the `export_dir:` directive in the `cide.yml` file otherwise.

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
