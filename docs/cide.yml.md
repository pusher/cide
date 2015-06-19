# Documentation for the `cide.yml` file

The `cide.yml` file is encoded in the [YAML markup language](http://yaml.org/)
and describes mostly how the Dockerfile will be generated.

It has the following semantic:

## Root document

```yaml
---
# Image to base the build on
#
# required
# type: string
from: 'ubuntu'

# A step executed as root. See the Step definition.
as_root: {}

# If set to true, inject the invoker's SSH key into the image.
# This is used for shared github access for example.
#
# type: boolean
use_ssh: no

# A step executed as the "cide" user. See the Step definition.
before: {}

# Environment variables set at runtime. See the ENV definition.
env: {}

# Selects a file or directory to export. When set and cide is invoked with
# --export the same directory will be copied back into the project's root.
#
# type: string
export_dir:

# When defined, executes and attaches the defined linked containers to the
# CI runtime. See the Link definition.
links: []

# Determines what script to run to execute the tests. This is the main command
# that is used to run the CI.
#
# The script has to exit with an exit-status of zero to succeed.
#
# default: "script/ci"
# type: string
run: rake
```

## Step definition

Here is the format for a step.

```yaml
# An array or hash of files to add
#
# In the hash form, the target is on the left-hand side and source on the
# right-hand side. If multiple values are passed on to the right then they are
# all added to the same folder.
#
# If a URL is provided in the source it is fetched during the build.
#
# Note that source files should either come from a URL or from a file within
# the project's directory.
add:
  /etc/cacert.pem: http://curl.se/cacert.pem
  /src:
    - Gemfile
    - Gemfile.lock
# or
add:
  - one
  - two

# Sets environment variables in this step (and next ones). See the ENV
# definition.
env: {}

# A list of commands to run in that stage
#
# type: string or array of string
run:
  - bundle exec
  - npm install
# or
run: go get ./...
```

If the step is defined as a string or array, those are interpreted as commands
to add to that step.

## ENV definition

An array or hash of environment variables to load

In the hash for, the left-hand side is the key and the right-hand side the
value.

If a hash value is nil or the array form is used, the environment variables
are loaded from the cide invoker environment.

```yaml
env:
  HOME: /cide
  AWS_ACCESS_KEY_ID:
# or
env:
  - AWS_ACCESS_SECRET_KEY
```

## Link definition

A hash that describes a linked containers. Linked containers are automatically
started before executing the main command and destroyed after that.

```yaml
# Gives a name to the container to execute. This also determines the hostname
# the main container will be able to address this container with.
#
# If the name is missing, the name is extracted from the `from` key. For
# example if the `from` value is "foobar/redis:2.6" the name will be "redis"
#
# type: string
name: 'mysql'

# Name of the image to pull and base the container on.
#
# type: string
name: 'redis:2.6'

# Environment variables for that container. See the ENV definition.
env: {}

# Single command to execute the container with.
#
# type: string
run: 'redis-server'
```
