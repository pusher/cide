require 'fileutils'

require 'cide/docker'

module CIDE
  class Builder
    include CIDE::Docker

    attr_reader :config

    def initialize(config)
      @config = config
      @tmp_files = []
    end

    def build(tag: nil, pull: nil, ssh_key: nil)
      raise ArgumentError, 'tag missing' unless tag

      if config.use_ssh
        raise ArgumentError, 'ssh_key missing' unless ssh_key
        unless File.exist?(ssh_key)
          raise ArgumentError, "ssh_key #{ssh_key} not found"
        end
        create_tmp_file! TEMP_SSH_KEY, File.read(ssh_key)
      end
      create_tmp_file! DOCKERFILE, config.to_dockerfile

      build_options = ['--force-rm']
      build_options << '--pull' if pull
      build_options.push '-f', DOCKERFILE
      build_options.push '-t', tag
      build_options << '.'
      docker :build, *build_options
    ensure
      release_tmp_files!
    end

    protected

    def create_tmp_file!(destination, content)
      FileUtils.mkdir_p(File.dirname(destination))
      @tmp_files << destination
      File.write(destination, content)
      # Dockerfile ADD compares content and mtime, we don't want that
      File.utime(1_286_701_800, 1_286_701_800, destination)
    end

    def release_tmp_files!
      @tmp_files.each do |file|
        FileUtils.rm_f(file)
      end
    end
  end
end
