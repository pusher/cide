require 'md2man/rakefile'
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

task default: [:spec, :rubocop, 'md2man:man']

RuboCop::RakeTask.new
RSpec::Core::RakeTask.new
