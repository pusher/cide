require 'virtus'

require 'erb'
require 'yaml'

require 'cide/build/config_loader'

module CIDE
  module Build
    class Config
      DOCKERFILE_TEMPLATE =
        File.expand_path('../../dockerfile_template.erb', __FILE__)

      module NiceInspect
        def inspect
          out = []
          attributes.each_pair do |key, value|
            out << "#{key}=#{value.inspect}"
          end
          "<#{out.join(' ')}>"
        end
      end

      class Step
        include Virtus.model(strict: true)
        include NiceInspect
        attribute :add, Array[String], default: []
        attribute :forward_env, Array[String], default: []
        attribute :run, Array[String], default: []
      end

      class Link
        include Virtus.model
        include NiceInspect
        attribute :name, String
        attribute :image, String
        attribute :env, Hash[String, String]
        attribute :run, String
        # Container ID added after the fact
        attr_accessor :id
      end

      include Virtus.model(strict: true)
      include NiceInspect
      attribute :from, String, default: 'ubuntu'
      attribute :as_root, Array[String], default: []
      attribute :use_ssh, Boolean, default: false
      attribute :before, Step, required: false
      attribute :forward_env, Array[String], default: []
      attribute :export_dir, String, required: false
      attribute :links, Array[Link], default: []
      attribute :run, String, default: 'script/ci'

      attr_reader :warnings, :errors

      def initialize(*)
        super
        @warnings = []
        @errors = []
      end

      def to_dockerfile
        ERB.new(File.read(DOCKERFILE_TEMPLATE), nil, '<>-').result(binding)
      end

      def self.load_file(file_path, output = $stderr)
        obj = new
        loader = ConfigLoader.new(obj)
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
    end
  end
end
