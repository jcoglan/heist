require 'rubygems'
require 'treetop'
require 'readline'

module Heist
  VERSION = '0.1.0'
  
  NORMAL_ORDER      = LAZY  = 0
  APPLICATIVE_ORDER = EAGER = 1
  
  ORDERS = %w(normal applicative)
  
  ROOT_PATH    = File.expand_path(File.dirname(__FILE__))
  PARSER_PATH  = ROOT_PATH + '/interpreter/'
  RUNTIME_PATH = ROOT_PATH + '/runtime/'
  BUILTIN_PATH = ROOT_PATH + '/builtin/'
  LIB_PATH     = ROOT_PATH + '/stdlib/'
  
  require PARSER_PATH + 'scheme'
  require PARSER_PATH + 'nodes'
  require RUNTIME_PATH + 'runtime'
  
  LOAD_PATH = [LIB_PATH]
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
      prompt = buffer == "" ? "> " : "  " + "   " * inset
      input  = Readline.readline(prompt)
      exit if input.nil?
      
      Readline::HISTORY.push(input)
      buffer << input + "\n"
      tree = parse(buffer)
      next if tree.nil?
      
      buffer = ""
      puts "=>  #{ runtime.eval(tree) rescue '[error]' }\n\n"
    end
  end
  
end

