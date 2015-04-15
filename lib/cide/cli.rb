require 'cide/constants'
require 'cide/docker'
require 'cide/build'

require 'thor'

require 'json'
require 'time'

module CIDE
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
      default: File.basename(Dir.pwd)

    method_option 'export',
      desc: 'Whenever to export artifacts',
      type: :boolean,
      default: nil

    method_option 'export_dir',
      desc: 'Change the ouput directory on the host',
      aliases: %w(o host_export_dir),
      default: nil

    method_option 'run',
      desc: 'Override the script to run',
      aliases: ['r'],
      default: nil

    method_option 'ssh_key',
      desc: 'Path to a ssh key to import into the docker image',
      aliases: ['s'],
      default: '~/.ssh/id_rsa'

    def build
      containers = []

      setup_docker

      ## Config ##
      build = Build::Config.load_file CONFIG_FILE
      exit 1 if build.nil?
      options.export_dir ||= build.export_dir
      build.run = options.run if options.run
      name = CIDE::Docker.id options.name
      tag = "cide/#{name}"

      ## Build ##
      if build.use_ssh
        unless File.exist?(options.ssh_key)
          fail ArgumentError, "SSH key #{options.ssh_key} not found"
        end

        create_tmp_file SSH_CONFIG_FILE, File.read(SSH_CONFIG_PATH)
        create_tmp_file TEMP_SSH_KEY, File.read(build.ssh_key)
      end
      create_tmp_file DOCKERFILE, build.to_dockerfile
      docker :build, '--force-rm', '--pull', '-f', DOCKERFILE, '-t', tag, '.'

      ## CI ##
      build.links.each do |link|
        args = ['--detach']
        link.env.each_pair do |key, value|
          args.push('--env', [key, value].join('='))
        end
        args << link.image
        args << link.run if link.run
        link.id = docker(:run, *args, capture: true).strip
        containers << link.id
      end

      run_options = ['--detach']

      build.forward_env.each do |env|
        run_options.push '--env', [env, ENV[env]].join('=')
      end

      build.links.each do |link|
        run_options.push '--link', [link.id, link.name].join(':')
      end

      run_options.push tag
      run_options.push build.run

      id = docker(:run, *run_options, capture: true).strip
      containers << id
      docker(:attach, id)

      ## Export ##
      return unless options.export
      fail 'export flag set but no export_dir given' if build.export_dir.nil?

      guest_export_dir = File.expand_path(build.export_dir, CIDE_SRC_DIR)
      host_export_dir  = File.expand_path(options.export_dir, Dir.pwd)
      docker :cp, [id, guest_export_dir].join(':'), host_export_dir
    rescue Docker::Error => ex
      exit ex.exitstatus
    ensure
      # Shutdown old containers
      unless containers.empty?
        docker :rm, '--force', *containers.reverse,
          verbose: false,
          capture: true
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

      x = docker('images', '--no-trunc', capture: true)
      iter = x.lines.each
      iter.next
      cide_image_ids = iter
        .map { |line| line.split(/\s+/) }
        .select { |line| line[0] =~ %r{^cide/} || line[0] == '<none>' }
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
      create_file CONFIG_FILE, File.read(DEFAULT_CIDEFILE)
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
