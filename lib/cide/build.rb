require 'virtus'

require 'erb'
require 'yaml'

module CIDE
  module Build
    DOCKERFILE_TEMPLATE =
      File.expand_path('../dockerfile_template.erb', __FILE__)

    class Step
      include Virtus.model(strict: true)

      attribute :forward_env, Array[String], default: []
      attribute :add, Array[String], default: []
      attribute :run, String
    end

    class Config
      include Virtus.model(strict: true)

      attribute :from, String, default: 'ubuntu'
      attribute :as_root, Array[String], default: []
      attribute :forward_env, Array[String], default: []
      attribute :before, Step, required: false
      attribute :use_ssh, Boolean, default: false
      attribute :export_dir, String, required: false
      attribute :run, String, default: 'script/ci'

      def to_dockerfile
        ERB.new(File.read(DOCKERFILE_TEMPLATE), nil, '<>-').result(binding)
      end

      def self.load_file(file_path)
        new(YAML.load_file(file_path))
      end
    end
  end
end
