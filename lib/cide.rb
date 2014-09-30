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
  CONFIG_FILE = ".cide.yml"

  def docker_id(str)
    # Replaces invalid docker tag characters by underscores
    "#{str}".downcase.gsub(/[^a-z0-9\-_.]/, '_')
  end

  def self.struct(opts={}, &block)
    Class.new(Struct.new(*opts.keys), &block).new(*opts.values)
  end

  DefaultConfig = struct(
    from: "ubuntu",
    as_root: [],
    command: 'script/ci',
  ) do

    alias_method :image=, :from=

    def set(name)
      method "#{name}="
    end

    def to_dockerfile
      ERB.new(TEMPLATE).result(binding)
    end

    def merge!(opts={})
      opts.each_pair(&method(:set))
      self
    end

    def merge(opts={})
      dup.merge!(opts)
    end

    def to_yaml
      members.each_with_object({}) do |k, obj|
        obj[k.to_s] = self[k]
      end.to_yaml
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
      setup_docker

      config = DefaultConfig.merge YAML.load_file(CONFIG_FILE)
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

      docker :build, '-t', tag, '.'
      docker :run, '--rm', '-t', tag, "sh", "-c", config.command
    end


    desc "clean", "Removes old containers"
    method_option "days",
      desc: "Number of days to keep the images",
      default: 7,
      type: :numeric
    method_option "count",
      desc: "Maximum number of images to keep",
      default: 10,
      type: :numeric
    def clean
      setup_docker

      days_to_keep = options[:days]
      max_images = options[:count]

      x = run('docker images --no-trunc', capture: true)
      iter = x.lines.each
      iter.next
      cide_image_ids = iter
        .map{ |line| line.split(/\s+/) }
        .select { |line| line[0] =~ %r{^cide/} || line[0] == "<none>" }
        .map{ |line| line[2] }

      if cide_image_ids.empty?
        puts "No images found to be cleaned"
        return
      end

      x = run("docker inspect #{cide_image_ids.join(' ')}", capture: true)
      cide_images = JSON.parse(x.strip)
        .each {|image| image["Created"] = Time.iso8601(image["Created"]) }
        .sort {|a, b| a["Created"] <=> b["Created"] }

      if cide_images.size > max_images
        old_cide_images = cide_images[0..-max_images]
          .map{ |image| image["Id"] }
      else
        old_times = Time.now - (days_to_keep * 24 * 60 * 60)
        old_cide_images = cide_images
          .select{ |image| image["Created"] < old_times}
          .map{ |image| image["Id"] }
      end

      if old_cide_images.empty?
        puts "No images found to be cleaned"
        return
      end

      run("docker rmi #{old_cide_images.join(' ')}")
    end

    desc "init", "Creates a blank #{CONFIG_FILE} into the project"
    def init
      if File.exists?(CONFIG_FILE)
        puts "#{CONFIG_FILE} already exists"
        return
      end
      puts "Creating #{CONFIG_FILE} with default values"
      create_file CONFIG_FILE, DefaultConfig.to_yaml
    end

    protected

    def setup_docker
      @setup_docker ||= (
        if `uname`.strip == "Darwin" && !ENV['DOCKER_HOST']
          if !system("which boot2docker >/dev/null 2>&1")
            puts "make sure boot2docker is installed and running"
            puts
            puts "> brew install boot2docker"
            exit 1
          end
          ENV['DOCKER_HOST'] = `boot2docker socket 2>/dev/null`.strip
        end
        true
      )
    end

    def docker(*args)
      #sh "docker", *args
      run Shellwords.join(["docker"] + args)
    end

  end
end
