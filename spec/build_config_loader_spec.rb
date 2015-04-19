require "cide/build"
require "stringio"
require "ostruct"
require "active_support/json"

describe "CIDE::Build::Config::Loader" do
  before do
    @config = CIDE::Build::Config.new
    @loader = CIDE::Build::ConfigLoader.new(@config)
    ENV['TEST1'] = 'test1'
    ENV['TEST2'] = 'test2'
  end

  default_config = {
    "from" => "ubuntu",
    "as_root" => nil,
    "use_ssh" => false,
    "before" => nil,
    "env" => {},
    "export_dir" => nil,
    "links" => [],
    "run" => ["script/ci"],
  }

  it "works - empty config" do
    @loader.load({})
    expect(@config.as_json).to eq(default_config)
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([])
  end

  it "works2 - full config" do
    full_config = {
      "from" => "god",
      "as_root" => {
        #"add" => [["http://df.ru", "zzz"], "yyy" => ["."]],
        "add" => [],
        "env" => {"HOME" => "/"},
        "run" => ["one", "two"],
      },
      "use_ssh" => true,
      "before" => {
        #"add" => [["http://df.ru", "zzz"], "yyy" => ["."]],
        "add" => [],
        "env" => {"HOME" => "/"},
        "run" => ["a", "b"],
      },
      "env" => {"HOME" => "/"},
      "links" => [
        {"name" => "redis", "image" => "redis2:foo", "env" => {"HOME" => "/"}, "run" => "redis-server"}
      ],
      "export_dir" => "./artifacts",
      "run" => ["do/something"],
    }

    @loader.load(full_config)

    expect(@config.as_json).to eq(default_config.merge(full_config))
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([])
  end

  it "coerces things around" do
    ENV['LOL'] = 'zzz'
    ENV['555'] = '666'
    @loader.load(
      as_root: "xxxxx",
      before: {
        add: {bin: 555}
      },
      links: ["mysql:5.6", {image: "redis", env: {PATH: "/bin", TEST1: nil}}, nil],
      env: ["LOL", nil, 555],
      run: "hello world",
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => {"add" => [], "env" => {}, "run" => ["xxxxx"]},
      "before" => {
        "add" => [{"src" => ["555"], "dest" => "bin"}],
        "env" => {},
        "run" => [],
      },
      "links" => [
        {"name" => "mysql", "image" => "mysql:5.6", "env" => {}, "run" => nil},
        {"name" => "redis", "image" => "redis", "env" => {"PATH" => "/bin", "TEST1" => "test1"}, "run" => nil},
      ],
      "env" => {"LOL" => "zzz", "555" => "666"},
      "run" => ["sh", "-e", "-c", "hello world"],
    ))
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([])
  end

  it "notifies deprecations" do
    @loader.load(
      link: { from: "hoho" },
      image: "foo",
      command: "lol",
      zzz: 4,
    )
    expect(@config.as_json).to eq(default_config.merge(
      "links" => [{"name" => "hoho", "image" => "hoho", "env" => {}, "run" => nil}],
      "from" => "foo",
      "run" => ["sh", "-e", "-c", "lol"],
    ))
    expect(@config.warnings).to eq([
      "link.from is deprecated. use 'image' instead.",
      "image is deprecated. use 'from' instead.",
      "command is deprecated. use 'run' instead.",
      "Unknown key zzz",
    ])
    expect(@config.errors).to eq([])
  end

  it "reports type errors" do
    @loader.load(
      as_root: ["aaa", Time.now],
      env: Time.now,
      links: {},
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => {"add" => [], "env" => {}, "run" => ["aaa", ""]}
    ))
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([
      "expected as_root.run[1] to be a string but got a Time",
      "expected env to be a hash or array of keys or just a string but got a Time",
      "expected links to be a expected hash to either declare the name or image but got a Hash",
    ])
  end

end
