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
      forward_env: ["HOME", nil, "PWD"]
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => ["xxxxx"],
      "before" => {
        "add" => [],
        "forward_env" => [],
        "run" => ["zzzzz"],
      },
      "forward_env" => ["HOME", "PWD"],
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
      as_root: ["aaa", 555],
      forward_env: {},
    )

    expect(@config.to_h.as_json).to eq(default_config.merge(
      "as_root" => ["aaa", ""],
      "errors" => [
        "expected as_root[1] to be a string but got a Fixnum",
        "expected forward_env to be a array of string, string or nil but got a Hash",
      ]
    ))
  end

end
