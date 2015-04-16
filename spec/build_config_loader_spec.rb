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
    "as_root" => [],
    "use_ssh" => false,
    "before" => nil,
    "env" => {},
    "export_dir" => nil,
    "links" => [],
    "run" => "script/ci",
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
      "as_root" => ["one", "two"],
      "use_ssh" => true,
      "before" => {
        "add" => ["zzz", "yyy"],
        "env" => {"HOME" => "/"},
        "run" => ["a", "b"],
      },
      "env" => {"HOME" => "/"},
      "links" => [
        {"name" => "redis", "image" => "redis2:foo", "env" => {"HOME" => "/"}, "run" => "redis-server"}
      ],
      "export_dir" => "./artifacts",
      "run" => "do/something",
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
      before: :zzzzz,
      links: ["mysql", {image: "redis", env: {PATH: "/bin", TEST1: nil}}, nil],
      env: ["LOL", nil, 555]
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => ["xxxxx"],
      "before" => {
        "add" => [],
        "env" => {},
        "run" => ["zzzzz"],
      },
      "links" => [
        {"name" => "mysql", "image" => "mysql", "env" => {}, "run" => nil},
        {"name" => "redis", "image" => "redis", "env" => {"PATH" => "/bin", "TEST1" => "test1"}, "run" => nil},
      ],
      "env" => {"LOL" => "zzz", "555" => "666"},
    ))
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([])
  end

  it "notifies deprecations" do
    @loader.load(
      image: "foo",
      command: "lol",
      zzz: 4,
    )
    expect(@config.as_json).to eq(default_config.merge(
      "from" => "foo",
      "run" => "lol",
    ))
    expect(@config.warnings).to eq([
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
      "as_root" => ["aaa", ""],
    ))
    expect(@config.warnings).to eq([])
    expect(@config.errors).to eq([
      "expected as_root[1] to be a string but got a Time",
      "expected env to be a hash or array of keys or just a string but got a Time",
      "expected links to be a expected hash to either declare the name or image but got a Hash",
    ])
  end

end
