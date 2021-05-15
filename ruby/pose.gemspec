# coding: utf-8
lib = File.expand_path('.', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)

Gem::Specification.new do |spec|
  spec.name          = "pose"
  spec.version       = "0.0.1"
  spec.authors       = ["Lassi Kortela"]
  spec.email         = ["lassi@lassi.io"]
  spec.summary       = "Portable S-expressions (POSE)"
  spec.homepage      = "https://github.com/s-expressions/pose"
  spec.license       = "ISC"

  spec.files         = `git ls-files`.split($/)
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end