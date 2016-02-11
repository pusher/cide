require 'json'

require 'cide/docker'

module CIDE
  class Runner
    include CIDE::Docker

    def initialize(
      command: [],
      env: {},
      links: [],
      tag: nil,
      user: nil
    )
      fail ArgumentError, 'tag missing' unless tag

      @containers = []
      @id = SecureRandom.hex

      @tag = tag
      @env = env
      @links = links
      @command = command
      @user = user
    end

    # !!!! Don't call run twice !!!!
    def run!(interactive: false)
      start_links!

      run_options = ['--detach']

      @env.each_pair do |key, value|
        run_options.push '--env', [key, value].join('=')
      end

      @links.each do |link|
        run_options.push '--link', [link.id, link.name].join(':')
      end

      run_options.push '--name', @id

      run_options.push '--user', @user if @user

      run_options.push('--interactive', '--tty') if interactive

      run_options.push @tag
      run_options.push(*@command)

      id = docker(:run, *run_options, capture: true).strip
      @containers << id
      docker(:attach, id)
    end

    def export!(guest_dir: nil, host_dir: nil)
      fail ArgumentError, 'guest export_dir missing' unless guest_dir
      fail ArgumentError, 'host export_dir missing' unless host_dir

      # FIXME: strip trailing slashes ?
      guest_dir = File.expand_path(guest_dir, CIDE_SRC_DIR)
      host_dir = File.expand_path(host_dir, Dir.pwd)

      FileUtils.mkdir_p(File.dirname(host_dir))

      docker :cp, [@id, guest_dir].join(':'), host_dir
    end

    def cleanup!(output: $stderr)
      return if @containers.empty?
      report_containers!(output)
    ensure
      # Shutdown old containers
      docker :rm, '--force', '--volumes', *@containers.reverse, capture: true
    end

    protected

    def start_links!
      @links.each do |link|
        args = ['--detach']
        link.env.each_pair do |key, value|
          args.push('--env', [key, value].join('='))
        end
        args << link.image
        args << link.run if link.run
        link.id = docker(:run, *args, capture: true).strip
        @containers << link.id
      end
    end

    def report_containers!(output)
      infos = docker(:inspect, *@containers, capture: true)

      JSON.parse(infos).each do |info|
        config = info['Config']
        state = info['State']

        next unless state['Dead'] || state['ExitCode'] > 0

        output.puts "=== Failed container #{info['Id']} ==="
        output.puts "Image: #{config['Image']}"
        output.puts "State: #{state.inspect}"
        docker(:logs, '--tail', 20, info['Id'])
      end
    end
  end
end
