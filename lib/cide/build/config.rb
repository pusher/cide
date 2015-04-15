require 'virtus'

require 'erb'
require 'yaml'

require 'cide/build/config_loader'

module CIDE
  module Build
    class Config
      DOCKERFILE_TEMPLATE =
        File.expand_path('../../dockerfile_template.erb', __FILE__)

      class Step
        include Virtus.model(strict: true)
        attribute :add, Array[String], default: []
        attribute :forward_env, Array[String], default: []
        attribute :run, Array[String], default: []
      end

      include Virtus.model(strict: true)
      attribute :from, String, default: 'ubuntu'
      attribute :as_root, Array[String], default: []
      attribute :use_ssh, Boolean, default: false
      attribute :before, Step, required: false
      attribute :forward_env, Array[String], default: []
      attribute :export_dir, String, required: false
      attribute :run, String, default: 'script/ci'

      attribute :infos, Array[String], default: []
      attribute :errors, Array[String], default: []

      def to_dockerfile
        ERB.new(File.read(DOCKERFILE_TEMPLATE), nil, '<>-').result(binding)
      end

      def self.load_file(file_path, output = $stderr)
        obj = new
        loader = ConfigLoader.new(obj)
        loader.load YAML.load_file(file_path)

        obj.infos.each do |info|
          output.puts info
        end

        obj.errors.each do |error|
          output.puts "ERROR: #{error}"
        end

        return obj if obj.errors.empty?
        nil
      end
    end
  end
end
