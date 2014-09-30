require "erb"
require "json"
require "optparse"
require "shellwords"
require "time"
require "yaml"

require "thor"

module CIDE
  extend self

  DOCKERFILE = 'Dockerfile'
  TEMPLATE = File.read(File.expand_path('../cide_template.erb', __FILE__))

  def docker_id(str)
    # Replaces invalid docker tag characters by underscores
    "#{str}".downcase.gsub(/[^a-z0-9\-_.]/, '_')
  end

  def self.struct(opts={}, &block)
    Class.new(Struct.new(*opts.keys), &block).new(*opts.values)
  end

  DefaultConfig = struct(
    image: "ubuntu",
    as_root: [],
    command: 'script/ci',
  ) do
    def set(name)
      method "#{name}="
    end

    def to_dockerfile
      ERB.new(TEMPLATE).result(binding)
    end

    def merge!(opts={})
      opts.each_pair{|k,v| self[k] = v }
      self
    end

    def merge(opts={})
      dup.merge!(opts)
    end
  end

  class CLI < Thor
    include CIDE
    include Thor::Actions

    default_command "build"

    desc "build", "Builds an image and executes the tests"
    method_option "name",
      desc: "Name of the build",
      aliases: ['n', 't'],
      default: CIDE.docker_id(File.basename(Dir.pwd))
    def build
      config = DefaultConfig.merge YAML.load_file('.cide.yml')
      tag = "cide/#{docker_id(options[:name])}"

      say_status :config, config.to_h

      if !File.exist?(DOCKERFILE)
        say_status :Dockerfile, "Creating temporary Dockerfile"
        File.write(DOCKERFILE, config.to_dockerfile)
        at_exit do
          File.unlink(DOCKERFILE)
        end
      else
        say_status :Dockerfile, "Using existing Dockerfile"
      end

      if `uname`.strip == "Darwin" && !ENV['DOCKER_HOST']
        if !run("which boot2docker >/dev/null 2>&1")
          puts "make sure boot2docker is installed and running"
          puts
          puts "> brew install boot2docker"
          exit 1
        end
        ENV['DOCKER_HOST'] = run("boot2docker socket 2>/dev/null", capture: true).strip
      end

      docker :build, '-t', tag, '.'
      docker :run, '--rm', '-t', tag, "sh", "-c", config.command
    end

    protected

    def docker(*args)
      #sh "docker", *args
      run Shellwords.join(["docker"] + args)
    end

  end
end
