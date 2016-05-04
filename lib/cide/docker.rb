require 'thor'

module CIDE
  # Simple docker client helper
  module Docker
    # Generates a valid id for docker from any string
    def self.id(str)
      str.to_s.downcase.gsub(/[^a-z0-9\-_.]/, '_')
    end

    # Raised when a docker command exits with a status higher than zero
    class Error < StandardError
      attr_reader :exitstatus
      def initialize(exitstatus)
        @exitstatus = exitstatus
        super("Failed with exitstatus #{exitstatus}")
      end
    end

    class VersionError < StandardError; end

    def docker_image_ids(filter_by: false)
      args = ['--no-trunc']
      args << ['--filter', filter_by] if filter_by
      lines = docker('images', *args, capture: true).lines[1..-1]
      lines
        .map { |line| line.split(/\s+/) }
        .map { |line| line[2] }
    end

    def docker(*args, verbose: false, capture: false)
      cmd = (['docker'] + args).map(&:to_s)
      p cmd if verbose

      if capture
        r, w = IO.pipe
        pid = Process.spawn(*cmd, out: w)
        w.close
        return r.read
      else
        pid = Process.spawn(*cmd)

        return 0
      end
    ensure
      Process.wait(pid)
      exitstatus = $?.exitstatus
      raise Error, exitstatus if exitstatus > 0
    end

    protected

    def setup_docker
      @setup_docker ||= (
        # Check docker version
        x = `docker version 2>/dev/null`
        docker_version =
          case x
          when /Client version: ([^\s]+)/
            $1
          when /\s+Version:\s+([^\s]+)/
            $1
          else
            raise VersionError, 'Unknown docker version'
          end

        if Gem::Version.new(docker_version) < Gem::Version.new('1.5.0')
          raise VersionError, "Docker version #{$1} too old"
        end

        true
      )
    end
  end
end
