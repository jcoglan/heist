require 'rubygems'
require 'treetop'
require 'readline'

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
  
  def self.repl
    puts "Heist Scheme interpreter, v. #{ VERSION }"
    puts "Application mode: #{ EVAL_MODE.upcase }\n\n"
    
    runtime, buffer = Runtime.new, ""
    
    loop do
      inset = buffer.scan(/\(/).size - buffer.scan(/\)/).size
      input = Readline.readline(buffer.empty? ? "> " : "  " + "   " * inset)
      exit if input.nil? or input.strip == "quit"
      Readline::HISTORY.push(input)
      buffer << input + "\n"
      tree = parse(buffer)
      next if tree.nil?
      buffer = ""
      begin
        puts runtime.eval(tree)
      rescue
        puts "[error]"
      end
    end
  end
  
end

