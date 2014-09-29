# coding: utf-8

Gem::Specification.new do |s|
  s.name          = 'cide'
  s.version       = '0.0.3'
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
  s.files         = `git ls-files`.split($/)
  s.test_files    = `git ls-files spec`.split($/)
  s.require_paths = ['lib']
end
