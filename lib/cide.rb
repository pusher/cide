require 'erb'
require 'json'
require 'optparse'
require 'shellwords'
require 'time'
require 'yaml'

require 'thor'

# CIDE is a Continuous Integration Docker Environment runner
#
# The juicy bits are defined in CIDE::CLI
module CIDE
  DOCKERFILE = 'Dockerfile'
  SSH_CONFIG_FILE = 'ssh_config'
  TEMP_SSH_KEY = 'id_rsa.tmp'
  DOCKERFILE_TEMPLATE = File.read(
    File.expand_path('../cide_template.erb', __FILE__),
  )
  SSH_CONFIG_CONTENTS = File.read(File.expand_path('../ssh_config', __FILE__))
  CONFIG_FILE = '.cide.yml'

  CIDE_DIR = '/cide'
  CIDE_SRC_DIR = File.join(CIDE_DIR, '/src')
  CIDE_SSH_DIR = File.join(CIDE_DIR, '/.ssh')

  module_function

  def docker_id(str)
    # Replaces invalid docker tag characters by underscores
    "#{str}".downcase.gsub(/[^a-z0-9\-_.]/, '_')
  end

  def self.struct(opts = {}, &block)
    Class.new(Struct.new(*opts.keys), &block).new(*opts.values)
  end

  DefaultConfig = struct(
    name: nil,
    from: 'ubuntu',
    as_root: [],
    forward_env: [],
    before: {},
    export: false,
    export_dir: './artifacts',
    host_export_dir: nil,
    run: 'script/ci',
    ssh_key: nil,
  ) do

    alias_method :image=, :from=
    alias_method :command=, :run=

    def name=(str)
      super CIDE.docker_id(str)
    end

    def to_dockerfile
      ERB.new(DOCKERFILE_TEMPLATE, nil, '<>-').result(binding)
    end

    def merge!(opts = {})
      opts.each_pair { |k, v| public_send("#{k}=", v) }
      self
    end

    def merge(opts = {})
      dup.merge!(opts)
    end

    def to_yaml
      members.each_with_object({}) do |k, obj|
        v = self[k]
        obj[k.to_s] = v unless v.nil?
      end.to_yaml
    end
  end

  # Command-line option-parsing and execution for cide
  class CLI < Thor
    include CIDE
    include Thor::Actions

    default_command 'build'

    desc 'build', 'Builds an image and executes the run script'

    method_option 'name',
      desc: 'Name of the build',
      aliases: %w(n t),
      default: nil

    method_option 'host_export_dir',
      desc: 'Output directory on host to put build artefacts in',
      aliases: ['o'],
      default: nil

    method_option 'export',
      desc: 'Are we expecting to export artifacts',
      type: :boolean,
      default: nil

    method_option 'run',
      desc: 'The script to run',
      aliases: ['r'],
      default: nil

    method_option 'ssh_key',
      desc: 'The ssh key to put into the docker image',
      aliases: ['s'],
      default: nil

    def build
      setup_docker

      config = DefaultConfig.merge YAML.load_file(CONFIG_FILE)
      options.each_pair do |k, v|
        config[k] = v unless v.nil?
      end
      config.name ||= File.basename(Dir.pwd)
      config.host_export_dir ||= config.export_dir

      tag = "cide/#{config.name}"

      if config.ssh_key && File.exist?(config.ssh_key)
        say_status :SSHkey, 'Creating temp ssh key file within directory'
        ssh_key_contents = File.read(config.ssh_key)
        File.write(TEMP_SSH_KEY, ssh_key_contents)
        config.ssh_key = TEMP_SSH_KEY
        at_exit do
          File.unlink(TEMP_SSH_KEY)
        end
      else
        say_status :SSHKey, 'No SSH key specified'
      end

      say_status :config, config.to_h

      # FIXME: Move Dockerfile out of the way if it exists
      if !File.exist?(DOCKERFILE)
        say_status :Dockerfile, 'Creating temporary Dockerfile'
        File.write(DOCKERFILE, config.to_dockerfile)
        at_exit do
          File.unlink(DOCKERFILE)
        end
      else
        say_status :Dockerfile, 'Using existing Dockerfile'
      end

      if !File.exist?(SSH_CONFIG_FILE)
        say_status :ssh_config, 'Creating temporary ssh config'
        File.write(SSH_CONFIG_FILE, SSH_CONFIG_CONTENTS)
        at_exit do
          File.unlink(SSH_CONFIG_FILE)
        end
      else
        say_status :ssh_config, 'Using existing ssh config'
      end

      docker :build, '-t', tag, '.'

      return unless config.export

      unless config.export_dir
        fail 'Fail: export flag set but no export dir given'
      end

      id = docker(:run, '-d', tag, true, capture: true).strip
      begin
        guest_export_dir = File.expand_path(config.export_dir, CIDE_SRC_DIR)

        host_export_dir = File.expand_path(
          config.host_export_dir || config.export_dir,
          Dir.pwd)

        docker :cp, [id, guest_export_dir].join(':'), host_export_dir

      ensure
        docker :rm, '-f', id
      end
    end

    desc 'clean', 'Removes old containers'
    method_option 'days',
      desc: 'Number of days to keep the images',
      default: 7,
      type: :numeric
    method_option 'count',
      desc: 'Maximum number of images to keep',
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
        .map { |line| line.split(/\s+/) }
        .select { |line| line[0] =~ /^cide\// || line[0] == '<none>' }
        .map { |line| line[2] }

      if cide_image_ids.empty?
        puts 'No images found to be cleaned'
        return
      end

      x = run("docker inspect #{cide_image_ids.join(' ')}", capture: true)
      cide_images = JSON.parse(x.strip)
        .each { |image| image['Created'] = Time.iso8601(image['Created']) }
        .sort { |a, b| a['Created'] <=> b['Created'] }

      if cide_images.size > max_images
        old_cide_images = cide_images[0..-max_images]
          .map { |image| image['Id'] }
      else
        old_times = Time.now - (days_to_keep * 24 * 60 * 60)
        old_cide_images = cide_images
          .select { |image| image['Created'] < old_times }
          .map { |image| image['Id'] }
      end

      if old_cide_images.empty?
        puts 'No images found to be cleaned'
        return
      end

      run("docker rmi #{old_cide_images.join(' ')}")
    end

    desc 'init', "Creates a blank #{CONFIG_FILE} into the project"
    def init
      if File.exist?(CONFIG_FILE)
        puts "#{CONFIG_FILE} already exists"
        return
      end
      puts "Creating #{CONFIG_FILE} with default values"
      create_file CONFIG_FILE, DefaultConfig.to_yaml
    end

    protected

    def setup_docker
      @setup_docker ||= (
        if `uname`.strip == 'Darwin' && !ENV['DOCKER_HOST']
          unless system('which boot2docker >/dev/null 2>&1')
            puts 'make sure boot2docker is installed and running'
            puts
            puts '> brew install boot2docker'
            exit 1
          end

          `boot2docker shellinit 2>/dev/null`
            .lines
            .grep(/export (\w+)=(.*)/) { ENV[$1] = $2.strip }
        end
        true
      )
    end

    def docker(*args)
      opts = args.last.is_a?(Hash) ? args.pop : {}
      ret = run Shellwords.join(['docker'] + args), opts
      fail 'Command failed' if $?.exitstatus > 0
      ret
    end
  end
end
