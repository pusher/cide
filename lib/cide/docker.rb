require 'thor'

module CIDE
  # Simple docker client helper
  module Docker
    # Generates a valid id for docker from any string
    def self.id(str)
      "#{str}".downcase.gsub(/[^a-z0-9\-_.]/, '_')
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
      fail Error, exitstatus if exitstatus > 0
    end

    protected

    def setup_docker
      @setup_docker ||= (
        # Check docker version
        docker_version = nil
        case `docker version 2>/dev/null`
        when /Client version: ([^\s]+)/
          docker_version = $1
        when /\s+Version:\s+([^\s]+)/
          docker_version = $1
        else
          fail VersionError, 'Unknown docker version'
        end

        if Gem::Version.new(docker_version) < Gem::Version.new('1.5.0')
          fail VersionError, "Docker version #{$1} too old"
        end

        true
      )
    end
  end
end
