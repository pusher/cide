require 'cide/docker'

require 'erb'
require 'yaml'

module CIDE
  def self.struct(opts = {}, &block)
    Class.new(Struct.new(*opts.keys), &block).new(*opts.values)
  end

  DOCKERFILE_TEMPLATE = File.expand_path('../dockerfile_template.erb', __FILE__)

  BuildConfig = struct(
    from: 'ubuntu',
    as_root: [],
    forward_env: [],
    before: {},
    export: false,
    export_dir: nil,
    use_ssh: false,
    run: 'script/ci',
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

    def load_file(file_path)
      merge YAML.load_file(file_path)
    end

    def to_yaml
      members.each_with_object({}) do |k, obj|
        v = self[k]
        obj[k.to_s] = v unless v.nil?
      end.to_yaml
    end
  end
end
