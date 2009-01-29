# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/heist.rb'

Hoe.new('heist', Heist::VERSION) do |p|
  p.developer('James Coglan', 'jcoglan@googlemail.com')
  p.extra_deps = %w(oyster treetop)
end

task :run do
  puts "\nRunning 'test/test.scm':\n\n"
  Heist.run('test/test.scm')
  puts "\n"
end

# vim: syntax=Ruby
