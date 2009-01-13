require 'rubygems'
require 'treetop'

%w(scheme scheme/nodes runtime).each do |path|
  require File.dirname(__FILE__) + '/heist/' + path
end

module Heist
  VERSION = '0.1.0'
  
  EVAL_MODE = "normal"
  
  def self.run(file, scope = nil)
    eval(File.read(file), scope)
  end
  
  def self.eval(source, scope = nil)
    (scope || env).eval(source)
  end
  
  def self.env
    @env ||= Runtime.new
  end
  
  def self.parse(source)
    @parser ||= SchemeParser.new
    @parser.parse(source)
  end
end

