module CIDE
  module Build
    class ConfigLoader
      class Path
        attr_reader :to_s
        def initialize(str)
          @to_s = str.to_s
        end

        def append(value)
          self.class.new(
            @to_s + (value.is_a?(Integer) ? "[#{value}]" : ".#{value}"),
          )
        end
      end

      def initialize(config)
        @config = config
      end

      def load(data)
        data.each_pair do |key, value|
          key = key.to_s
          path = Path.new(key)
          case key
          when 'from', 'image' then
            wanted_key(path, 'from', key)
            @config.from = expect_string(path, value)
          when 'as_root' then
            @config.as_root = maybe_step(path, value)
          when 'use_ssh' then
            @config.use_ssh = expect_boolean(path, value)
          when 'before' then
            @config.before = maybe_step(path, value)
          when 'env', 'forward_env' then
            wanted_key(path, 'env', key)
            @config.env = expect_env_hash(path, value)
          when 'export_dir' then
            @config.export_dir = maybe_string(path, value)
          when 'link', 'links' then
            @config.links = expect_links(path, value)
          when 'run', 'command' then
            wanted_key(path, 'run', key)
            @config.run = expect_string(path, value)
          else
            unknown_key(path)
          end
        end
        @config
      end

      protected

      def warn(message)
        @config.warnings << message
      end

      def error(message)
        @config.errors << message
      end

      def wanted_key(path, wanted_key, key)
        return if key == wanted_key
        warn "#{path} is deprecated. use '#{wanted_key}' instead."
      end

      def unknown_key(path)
        warn "Unknown key #{path}"
      end

      def type_error(path, wanted_type, value)
        error "expected #{path} to be a #{wanted_type} but got a #{value.class}"
      end

      def expect_string(path, value)
        case value
        when String, Symbol, Integer
          value.to_s
        else
          type_error(path, 'string', value)
          ''
        end
      end

      def maybe_string(path, value)
        case value
        when String, Symbol, Integer
          value.to_s
        when nil
          nil
        else
          type_error(path, 'string or nil', value)
          nil
        end
      end

      def maybe_step(path, value)
        case value
        when String, Symbol, Integer, Array then
          load_step(path, run: value)
        when Hash then
          load_step(path, value)
        when nil then
          nil
        else
          type_error(path, 'string, array, hash or nil', value)
          nil
        end
      end

      def load_step(path, data)
        step = Config::Step.new
        data.each_pair do |key, value|
          key = key.to_s
          path_ = path.append(key)
          case key
          when 'run' then
            step.run = expect_array(path_, value)
          when 'env', 'forward_env' then
            wanted_key(path_, 'env', key)
            step.env = expect_env_hash(path_, value)
          when 'add' then
            step.add = expect_adds(path_, value)
          else
            unknown_key(path_)
          end
        end
        step
      end

      def expect_links(path, value)
        array = []
        case value
        when String, Symbol, Integer, Hash then
          array << expect_link(path, value)
        when Array then
          value.compact.each_with_index do |value_, i|
            array << expect_link(path.append(i), value_)
          end
        when nil then
          # nothing to do
        else
          type_error(path, 'string, array of links, hash or nil', value)
        end
        array.compact
      end

      def expect_link(path, value)
        case value
        when String, Symbol, Integer then
          load_link(path, name: value, image: value)
        when Hash
          load_link(path, value)
        else
          type_error(path, 'string or hash expected', value)
        end
      end

      def load_link(path, data)
        link = Config::Link.new
        data.each_pair do |key, value|
          key = key.to_s
          path_ = path.append(key)
          case key
          when 'name' then
            link.name = expect_string(path_, value)
          when 'image', 'from' then
            wanted_key(path_, 'image', key)
            link.image = expect_string(path_, value)
          when 'env' then
            link.env = expect_env_hash(path_, value)
          when 'run' then
            link.run = maybe_string(path_, value)
          else
            unknown_key(path_)
          end
        end
        link.name ||= link.image
        link.image ||= link.name
        if link.name.nil?
          type_error(
            path,
            'expected hash to either declare the name or image',
            data,
          )
          return nil
        end
        link
      end

      def expect_boolean(path, value)
        case value
        when true then
          true
        when false then
          false
        else
          type_error(path, 'boolean', value)
          false
        end
      end

      def expect_array(path, value)
        array = []
        case value
        when Array then
          value.compact.each_with_index do |value_, i|
            array << expect_string(path.append(i), value_)
          end
        when String, Symbol, Integer then
          array << value.to_s
        when nil then
          # nothing to do
        else
          type_error(path, 'array of string, string or nil', value)
        end
        array.compact
      end

      def expect_adds(path, value)
        array = []
        case value
        when Array then
          value.compact.each_with_index do |value_, i|
            str = expect_string(path.append(i), value_)
            array << load_add_str(str)
          end
        when Hash then
          value.each_pair do |key_, value_|
            src = expect_array(path.append(key_), value_)
            array << Config::FileAdd.new(src: src, dest: key_.to_s)
          end
        when String, Symbol, Integer then
          array << load_add_str(value.to_s)
        when nil then
          # nothing to do
        else
          type_error(path, 'arrays of string, hash, string or nil', value)
        end
        array.compact
      end

      def load_add_str(str)
        Config::FileAdd.new(
          src: [str],
          dest: str,
        )
      end

      def expect_env(path, key)
        str = expect_string(path, key)
        return nil if str == ''
        value = ENV[str]
        error "Missing environment variable #{key} in #{path}" if value.nil?
        value
      end

      def expect_env_hash(path, value)
        hash = {}
        case value
        when String, Symbol, Integer
          key1 = value
          value1 = expect_env(path, key1)
          hash[key1] = value1 if value1
        when Array then
          value.compact.each_with_index do |key, i|
            value_ = expect_env(path.append(i), key)
            hash[key.to_s] = value_ if value_
          end
        when Hash then
          value.each_pair do |key, value_|
            key = key.to_s
            path_ = path.append(key)
            if value_.nil?
              value_ = expect_env(path_, key)
            else
              value_ = expect_string(path_, value_)
            end
            hash[key.to_s] = value_ if value_
          end
        else
          type_error(path, 'hash or array of keys or just a string', value)
        end
        hash
      end
    end
  end
end
