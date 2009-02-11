require 'rubygems'
require 'oyster'

Heist::BIN_SPEC = Oyster.spec do
  name "heist -- Ruby-powered Scheme interpreter, v. #{Heist::VERSION}"
  author 'James Coglan <jcoglan@googlemail.com>'
  
  synopsis <<-EOS
    heist -i [OPTIONS]
    heist FILE_NAME [OPTIONS]
  EOS
  
  flag :interactive, :desc =>
        'Start an interactive Scheme session'
  
  flag :lazy, :default => false, :desc =>
        'Use lazy evaluation order'
  
  flag :continuations, :default => false, :desc =>
        'Enable first-class continuations and (call/cc)'
end

