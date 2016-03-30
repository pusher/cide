require 'virtus'

require 'erb'
require 'yaml'

require 'cide/config_file_loader'

module CIDE
  class ConfigFile
    DOCKERFILE_TEMPLATE =
      File.expand_path('../dockerfile_template.erb', __FILE__)

    module NiceInspect
      def inspect
        out = []
        attributes.each_pair do |key, value|
          out << "#{key}=#{value.inspect}"
        end
        "<#{out.join(' ')}>"
      end
    end

    class FileAdd
      include Virtus.model(strict: true)
      include NiceInspect
      attribute :src, Array[String], default: []
      attribute :dest, String
    end

    class Step
      include Virtus.model(strict: true)
      include NiceInspect
      attribute :add, Array[FileAdd], default: []
      attribute :env, Hash[String, String], default: {}
      attribute :run, Array[String], default: []
    end

    class Link
      include Virtus.model
      include NiceInspect
      attribute :name, String
      attribute :image, String
      attribute :env, Hash[String, String], default: {}
      attribute :run, String
      # Container ID added after the fact
      attr_accessor :id
    end

    class PackageConfig
      include Virtus.model
      include NiceInspect
      attribute :add_version, String, required: false
    end

    include Virtus.model(strict: true)
    include NiceInspect
    attribute :from, String, default: 'ubuntu'
    attribute :as_root, Step, required: false
    attribute :use_ssh, Boolean, default: false
    attribute :before, Step, required: false
    attribute :env, Hash[String, String], default: {}
    attribute :export_dir, String, required: false
    attribute :links, Array[Link], default: []
    attribute :run, Array[String], default: ['script/ci']
    attribute :package, PackageConfig, required: false

    attr_reader :warnings, :errors

    def initialize(*)
      super
      @warnings = []
      @errors = []
    end

    def to_dockerfile
      ERB.new(File.read(DOCKERFILE_TEMPLATE), nil, '<>-').result(binding)
    end

    def self.load(dir, output = $stderr)
      file_path = find_config(dir)
      load_file(file_path, output)
    end

    def self.load_file(file_path, output = $stderr)
      obj = new
      loader = ConfigFileLoader.new(obj)
      loader.load YAML.load_file(file_path)

      obj.warnings.each do |warn|
        output.puts "WARN: #{warn}"
      end

      obj.errors.each do |error|
        output.puts "ERROR: #{error}"
      end

      return obj if obj.errors.empty?
      nil
    end

    def self.find_config(dir)
      paths = CONFIG_FILES.map { |name| File.expand_path(name, dir) }
      paths
        .find { |path| File.exist?(path) } ||
        raise("Config not found among these paths: #{paths.inspect}")
    end
  end
end
