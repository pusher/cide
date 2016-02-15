require 'cide/builder'
require 'cide/config_file'
require 'cide/constants'
require 'cide/docker'
require 'cide/runner'

require 'thor'

require 'json'
require 'securerandom'
require 'time'

module CIDE
  # Command-line option-parsing and execution for cide
  class CLI < Thor
    include CIDE::Docker
    include Thor::Actions

    default_command 'exec'

    desc 'exec', 'Builds an image and executes the run script'

    method_option 'name',
      desc: 'Name of the image',
      aliases: %w(-n -t),
      default: File.basename(Dir.pwd)

    method_option 'export',
      desc: 'Whenever to export artifacts',
      type: :boolean,
      default: nil

    method_option 'export_dir',
      desc: 'Change the ouput directory on the host',
      aliases: %w(-o --host_export_dir),
      default: nil

    method_option 'guest_export_dir',
      desc: 'Change the ouput directory on the host',
      aliases: %w(-i),
      default: nil

    method_option 'run',
      desc: 'Override the script to run',
      type: :string,
      aliases: ['-r'],
      default: nil

    method_option 'pull',
      desc: 'Whenever to pull for new images on build',
      type: :boolean,
      default: true

    method_option 'ssh_key',
      desc: 'Path to a ssh key to import into the docker image',
      aliases: ['-s'],
      default: '~/.ssh/id_rsa'

    def exec
      setup_docker

      tag = name_to_tag options.name

      banner 'Config'
      config = ConfigFile.load(Dir.pwd)
      say_status :config, config.inspect

      ## Build ##
      banner 'Build'
      builder = Builder.new(config)
      builder.build(
        pull: options.pull,
        ssh_key: File.expand_path(options.ssh_key),
        tag: tag,
      )

      ## Run ##
      banner 'Run'

      command = options.run ? ['sh', '-e', '-c', options.run] : config.run

      runner = Runner.new(
        command: command,
        env: config.env,
        links: config.links,
        tag: tag,
      )
      runner.run!

      ## Export ##
      return unless options.export
      banner 'Export'
      runner.export!(
        guest_dir: options.guest_export_dir || config.export_dir,
        host_dir: options.export_dir || config.export_dir,
      )
    rescue Docker::Error => ex
      exit ex.exitstatus
    rescue RuntimeError => ex
      $stderr.puts ex.to_s
      exit 1
    ensure
      runner.cleanup! if runner
    end

    desc 'package', 'Builds a package from the container script/build'
    method_option 'name',
      desc: 'Name of the image',
      aliases: %w(-n -t),
      default: File.basename(Dir.pwd)

    method_option 'ssh_key',
      desc: 'Path to a ssh key to import into the docker image',
      aliases: ['-s'],
      default: '~/.ssh/id_rsa'

    method_option 'package',
      desc: 'Name of the package',
      default: File.basename(Dir.pwd)

    method_option 'upload',
      desc: 'Whenever to upload the result to S3',
      type: :boolean,
      default: true

    method_option 'set-version',
      desc: 'Tells cide the package version, otherwise extracted from git',
      default: nil

    # AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY need to be passed trough the
    # env or ~/.aws/credentials file
    method_option 'aws_bucket',
      desc: 'AWS_BUCKET',
      default: ENV['AWS_BUCKET']

    def package
      fail 'missing AWS_BUCKET' if options.upload && !options.aws_bucket

      tag = name_to_tag options.name

      build_root = File.expand_path(".packager-#{Time.now.to_i}")
      guest_export_dir = '/cide/package'
      host_export_dir  = File.join(build_root, 'package')

      version = options.version || (
        git_branch = `git symbolic-ref --short -q HEAD || echo unknown`.strip
        git_rev = `git rev-parse --short HEAD`.strip
        "#{git_branch}-#{git_rev}"
      )

      timestamp = Time.now.strftime('%Y-%m-%d_%H%M%S')
      tar_name = "#{options.package}.#{timestamp}.#{version}.tar.gz"
      tar_path = File.join(build_root, tar_name)

      banner 'Config'
      config = ConfigFile.load(Dir.pwd)
      say_status :config, config.inspect

      ## Build ##
      banner 'Build'
      builder = Builder.new(config)
      builder.build(
        pull: options.pull,
        ssh_key: File.expand_path(options.ssh_key),
        tag: tag,
      )

      ## Run ##
      banner 'Run'
      runner = Runner.new(
        command: ['script/build', guest_export_dir],
        env: config.env,
        links: config.links,
        tag: tag,
      )
      runner.run!

      ## Export ##
      banner 'Export'
      runner.export!(
        guest_dir: guest_export_dir,
        host_dir: host_export_dir,
      )

      # Create archive
      system('tar', '-czf', tar_path, '-C', host_export_dir, '.')

      ## Upload ##

      return unless options.upload
      banner 'Upload'

      require 'aws-sdk'
      s3 = Aws::S3::Client.new
      s3.bucket(options.aws_bucket).object(tar_name).upload_file(tar_path)

    rescue Docker::Error => ex
      exit ex.exitstatus
    rescue RuntimeError => ex
      $stderr.puts ex.to_s
      exit 1
    ensure
      FileUtils.rm_rf(host_export_dir) if options.upload

      runner.cleanup! if runner
    end

    desc 'debug', 'Opens a debug console in the last project image'
    method_option 'name',
      desc: 'Name of the build',
      aliases: %w(-n -t),
      default: File.basename(Dir.pwd)
    method_option 'user',
      desc: 'User to run under',
      default: 'cide'
    def debug
      setup_docker

      tag = name_to_tag options.name

      ## Config ##
      banner 'Config'
      config = ConfigFile.load(Dir.pwd)
      say_status :config, config.inspect

      ## Run ##
      banner 'Run'
      runner = Runner.new(
        command: ['bash'],
        env: config.env,
        links: config.links,
        tag: tag,
        user: options.user,
      )
      runner.run!(interactive: true)

    rescue Docker::Error => ex
      exit ex.exitstatus
    rescue RuntimeError => ex
      $stderr.puts ex.to_s
      exit 1
    ensure
      runner.cleanup! if runner
    end

    desc 'clean', 'Removes old containers'
    method_option 'days',
      desc: 'Number of days to keep the images',
      default: 7,
      type: :numeric
    method_option 'count',
      desc: 'Maximum number of images to keep',
      default: 20,
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
        .select { |line| line[0] =~ %r{^cide[/-]} || line[0] == '<none>' }
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

      docker('rmi', '--force', *old_cide_images)
    end

    desc 'init', "Creates a blank #{CONFIG_FILES.first} in the project"
    def init
      puts "Creating #{CONFIG_FILES.first} with default values"
      create_file CONFIG_FILES.first, File.read(DEFAULT_CIDEFILE)
    end

    private

    # Prefixes the tag to make it recognizable by the cleaner
    # Makes sure it's a valid tag
    def name_to_tag(name)
      "cide/#{CIDE::Docker.id name}"
    end

    LINE_WIDTH = 78.0
    def banner(text)
      pad = (LINE_WIDTH - text.size - 4) / 2
      pad = 0 if pad < 0
      puts '=' * pad.floor + "[ #{text} ]" + '=' * pad.ceil
    end
  end
end
