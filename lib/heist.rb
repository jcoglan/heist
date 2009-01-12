require 'rubygems'
require 'treetop'

%w(scheme scheme/nodes runtime/function runtime/scope runtime/builtins).each do |path|
  require File.dirname(__FILE__) + '/heist/' + path
end

module Heist
  VERSION = '0.1.0'
  
  def self.run(file, scope = nil)
    ast = parse(File.read(file))
    ast.eval(scope || env)
  end
  
  def self.env
    @scope ||= Runtime::Scope.new
  end
  
  def self.eval(source, scope = nil)
    parse(source).eval(scope || Runtime::Scope.new)
  end
  
  def self.parse(source)
    @parser ||= SchemeParser.new
    @parser.parse(source)
  end
end

