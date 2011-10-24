Gem::Specification.new do |s|
  s.name              = "heist"
  s.version           = "0.3.3"
  s.summary           = "Scheme in as little Ruby as possible"
  s.author            = "James Coglan"
  s.email             = "jcoglan@gmail.com"
  s.homepage          = "http://github.com/jcoglan/heist"

  s.extra_rdoc_files  = %w[README.rdoc]
  s.rdoc_options      = %w[--main README.rdoc]

  s.files             = %w[History.txt README.rdoc] +
                        Dir.glob("{bin,lib,test}/**/*")
  
  s.executables       = Dir.glob("bin/**").map { |f| File.basename(f) }
  s.require_paths     = %w[lib]

  s.add_dependency "oyster", ">= 0.9"
  s.add_dependency "treetop", ">= 1.2"
  
  s.add_development_dependency "rspec"
end
