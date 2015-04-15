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
            @config.as_root = expect_array(path, value)
          when 'use_ssh' then
            @config.use_ssh = expect_boolean(path, value)
          when 'before' then
            @config.before = maybe_step(path, value)
          when 'forward_env' then
            @config.forward_env = expect_array(path, value)
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

      def wanted_key(path, wanted_key, key)
        return if key == wanted_key
        @config.warnings <<
          "#{path} is deprecated. use '#{wanted_key}' instead."
      end

      def unknown_key(path)
        @config.warnings << "Unknown key #{path}"
      end

      def type_error(path, wanted_type, value)
        @config.errors <<
          "expected #{path} to be a #{wanted_type} but got a #{value.class}"
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
        when String, Symbol
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
        when String, Symbol, Array then
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
          when 'forward_env' then
            step.forward_env = expect_array(path_, value)
          when 'add' then
            step.add = expect_array(path_, value)
          else
            unknown_key(path_)
          end
        end
        step
      end

      def expect_links(path, value)
        array = []
        case value
        when String, Symbol, Hash then
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
        when String, Symbol then
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
            link.env = expect_env(path_, value)
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
        when String, Symbol then
          array << value.to_s
        when nil then
          # nothing to do
        else
          type_error(path, 'array of string, string or nil', value)
        end
        array.compact
      end

      def expect_env(path, value)
        case value
        when Hash then
          hash = {}
          value.each_pair do |key, value_|
            hash[key.to_s] = expect_string(path.append(key.to_s), value_)
          end
          hash
        else
          type_error(path, 'hash', value)
          {}
        end
      end
    end
  end
end
