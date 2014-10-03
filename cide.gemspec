# coding: utf-8

Gem::Specification.new do |s|
  s.name          = 'cide'
  s.version       = '0.0.5'
  s.authors       = ['zimbatm']
  s.email         = ['zimbatm@zimbatm.com']
  s.summary       = 'CI docker runner'
  s.description   = <<DESC
cide makes it easy to reproduce CI builds on the developer computer by
providing the same docker environment.
DESC
  s.homepage      = 'https://github.com/zimbatm/cide'
  s.license       = 'MIT'

  s.executables  << 'cide'
  s.files         = `git ls-files`.split($INPUT_RECORD_SEPARATOR)
  s.test_files    = `git ls-files spec`.split($INPUT_RECORD_SEPARATOR)
  s.require_paths = ['lib']

  s.required_ruby_version = '>= 1.9.3'

  s.add_runtime_dependency 'thor'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'rubocop'
end
