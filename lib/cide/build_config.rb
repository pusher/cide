require "cide/docker"

require "erb"

module CIDE
	def self.struct(opts = {}, &block)
    Class.new(Struct.new(*opts.keys), &block).new(*opts.values)
  end

  DOCKERFILE_TEMPLATE = File.expand_path('../dockerfile_template.erb', __FILE__)

  BuildConfig = struct(
    name: nil,
    from: 'ubuntu',
    as_root: [],
    forward_env: [],
    before: {},
    export: false,
    export_dir: './artifacts',
    host_export_dir: nil,
    run: 'script/ci',
    use_ssh: false,
    ssh_key: '~/.ssh/id_rsa',
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

    def to_yaml
      members.each_with_object({}) do |k, obj|
        v = self[k]
        obj[k.to_s] = v unless v.nil?
      end.to_yaml
    end
  end
end