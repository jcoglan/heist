require 'rubygems'
require 'treetop'
require 'readline'

%w(scheme scheme/nodes runtime).each do |path|
  require File.dirname(__FILE__) + '/heist/' + path
end

module Heist
  VERSION = '0.1.0'
  
  NORMAL_ORDER      = LAZY  = 0
  APPLICATIVE_ORDER = EAGER = 1
  
  ORDERS = %w(normal applicative)
  
  LOAD_PATH = [File.dirname(__FILE__) + '/heist/stdlib/']
  FILE_EXT  = ".scm"
  
  def self.run(file, scope = nil)
    file = file + FILE_EXT unless File.file?(file)
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
  
  def self.repl(options = {})
    runtime, buffer = Runtime.new(options), ""
    
    puts "Heist Scheme interpreter, v. #{ VERSION }"
    puts "Evaluation strategy: #{ runtime.lazy? ? 'LAZY' : 'EAGER' }\n\n"
    
    loop do
      inset  = buffer.scan(/\(/).size - buffer.scan(/\)/).size
      prompt = buffer.empty? ? "> " : "  " + "   " * inset
      input  = Readline.readline()
      exit if input.nil?
      
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

