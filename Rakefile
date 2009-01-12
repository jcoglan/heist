# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/heist.rb'

Hoe.new('heist', Heist::VERSION) do |p|
  # p.rubyforge_name = 'heistx' # if different than lowercase project name
  p.developer('James Coglan', 'jcoglan@googlemail.com')
end

task :run do
  puts "\nRunning 'test/test.scm':\n\n"
  Heist.run('test/test.scm')
  puts "\n"
end

# vim: syntax=Ruby
