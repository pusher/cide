# coding: utf-8

Gem::Specification.new do |s|
  s.name          = 'cide'
  s.version       = '0.5.0'
  s.authors       = ['zimbatm']
  s.email         = ['zimbatm@zimbatm.com']
  s.summary       = 'Isolated test runner with Docker'
  s.description   = <<DESC
cide is a command-line tool that runs tests in an isolated (docker)
environment. It allows to run the same command on the developer and CI
machines.
DESC
  s.homepage      = 'https://zimbatm.github.io/cide'
  s.license       = 'MIT'

  s.executables   = ['cide']
  s.files         = `git ls-files`.split($INPUT_RECORD_SEPARATOR)
  s.test_files    = `git ls-files spec`.split($INPUT_RECORD_SEPARATOR)
  s.require_paths = ['lib']

  s.required_ruby_version = '>= 1.9.3'

  s.add_runtime_dependency 'thor', '~> 0.19'
  s.add_runtime_dependency 'virtus', '~> 1.0'
  s.add_development_dependency 'activesupport'
  s.add_development_dependency 'md2man'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'rspec'
  s.add_development_dependency 'rubocop'
end
