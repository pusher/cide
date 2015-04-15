require "cide/build"
require "stringio"
require "ostruct"
require "active_support/json"

describe "CIDE::Build::Config::Loader" do
  before do
    @config = CIDE::Build::Config.new
    @loader = CIDE::Build::ConfigLoader.new(@config)
  end

  default_config = {
    "from" => "ubuntu",
    "as_root" => [],
    "use_ssh" => false,
    "before" => nil,
    "forward_env" => [],
    "export_dir" => nil,
    "links" => [],
    "run" => "script/ci",
    "infos" => [],
    "errors" => [],
  }

  it "works - empty config" do
    @loader.load({})
    expect(@config.as_json).to eq(default_config)
  end

  it "works2 - full config" do
    full_config = {
      "from" => "god",
      "as_root" => ["one", "two"],
      "use_ssh" => true,
      "before" => {
        "add" => ["zzz", "yyy"],
        "forward_env" => ["HOME"],
        "run" => ["a", "b"],
      },
      "forward_env" => ["PWD"],
      "links" => [
        {"name" => "redis", "image" => "redis2:foo", "env" => {"HOME" => "/"}, "run" => "redis-server"}
      ],
      "export_dir" => "./artifacts",
      "run" => "do/something",
    }

    @loader.load(full_config)

    expect(@config.as_json).to eq(default_config.merge(full_config))
  end

  it "coerces things around" do
    @loader.load(
      as_root: "xxxxx",
      before: :zzzzz,
      links: ["mysql", {image: "redis", env: {PATH: "/bin"}}, nil],
      forward_env: ["HOME", nil, 555]
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => ["xxxxx"],
      "before" => {
        "add" => [],
        "forward_env" => [],
        "run" => ["zzzzz"],
      },
      "links" => [
        {"name" => "mysql", "image" => "mysql", "run" => nil, "env" => {}},
        {"name" => "redis", "image" => "redis", "run" => nil, "env" => {"PATH" => "/bin"}},
      ],
      "forward_env" => ["HOME", "555"],
    ))
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
      "infos" => [
        "image is deprecated. use 'from' instead.",
        "command is deprecated. use 'run' instead.",
        "Unknown key zzz",
      ]
    ))
  end

  it "reports type errors" do
    @loader.load(
      as_root: ["aaa", Time.now],
      forward_env: {},
      links: {},
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => ["aaa", ""],
      "errors" => [
        "expected as_root[1] to be a string but got a Time",
        "expected forward_env to be a array of string, string or nil but got a Hash",
        "expected links to be a expected hash to either declare the name or image but got a Hash",
      ]
    ))
  end

end
