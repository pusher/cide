require 'erb'
require 'json'
require 'optparse'
require 'time'
require 'yaml'

require 'thor'

require 'cide/docker'

# CIDE is a Continuous Integration Docker Environment runner
#
# The juicy bits are defined in CIDE::CLI
module CIDE
  DIR = File.expand_path('..', __FILE__)
  DOCKERFILE = 'Dockerfile'
  TEMP_SSH_KEY = 'id_rsa.tmp'
  DOCKERFILE_TEMPLATE = File.join(DIR, 'cide_template.erb')
  SSH_CONFIG_FILE = 'ssh_config'
  SSH_CONFIG_PATH = File.join(DIR, SSH_CONFIG_FILE)
  CONFIG_FILE = '.cide.yml'

  CIDE_DIR = '/cide'
  CIDE_SRC_DIR = File.join(CIDE_DIR, '/src')
  CIDE_SSH_DIR = File.join(CIDE_DIR, '/.ssh')

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
    use_ssh: false,
    ssh_key: '~/.ssh/id_rsa',
  ) do

    alias_method :image=, :from=
    alias_method :command=, :run=

    def name=(str)
      super CIDE::Docker.id(str)
    end

    def ssh_key_path
      File.expand_path(ssh_key)
    end

    def to_dockerfile
      ERB.new(File.read(DOCKERFILE_TEMPLATE), nil, '<>-').result(binding)
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
    include CIDE::Docker
    include Thor::Actions
    add_runtime_options!

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
      desc: 'Path to a ssh key to import into the docker image',
      aliases: ['s'],
      default: '~/.ssh/id_rsa'

    def build
      setup_docker

      config = DefaultConfig.merge YAML.load_file(CONFIG_FILE)
      options.each_pair do |k, v|
        config[k] = v unless v.nil?
      end
      config.name ||= File.basename(Dir.pwd)
      config.host_export_dir ||= config.export_dir

      tag = "cide/#{config.name}"

      if config.use_ssh
        unless File.exist?(config.ssh_key_path)
          fail MalformattedArgumentError, "SSH key #{config.ssh_key} not found"
        end

        create_tmp_file SSH_CONFIG_FILE, File.read(SSH_CONFIG_PATH)
        create_tmp_file TEMP_SSH_KEY, File.read(config.ssh_key_path)
      end

      say_status :config, config.to_h

      create_tmp_file DOCKERFILE, config.to_dockerfile

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
          Dir.pwd,
        )

        docker :cp, [id, guest_export_dir].join(':'), host_export_dir

      ensure
        docker :rm, '-f', id
      end
    rescue Docker::Error => ex
      exit ex.exitstatus
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

      x = docker('images', '--no-trunc', capture: true)
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

      x = docker('inspect', *cide_image_ids, capture: true)
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

      docker('rmi', *old_cide_images)
    end

    desc 'init', "Creates a blank #{CONFIG_FILE} into the project"
    def init
      puts "Creating #{CONFIG_FILE} with default values"
      create_file CONFIG_FILE, DefaultConfig.to_yaml
    end

    private

    def create_tmp_file(destination, *args, &block)
      create_file(destination, *args, &block)
      at_exit do
        remove_file(destination, verbose: false)
      end
    end
  end
end
