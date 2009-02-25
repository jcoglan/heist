require 'rubygems'
require 'treetop'

module Heist
  VERSION = '0.1.0'
  
  ROOT_PATH    = File.expand_path(File.dirname(__FILE__))
  PARSER_PATH  = ROOT_PATH + '/parser/'
  RUNTIME_PATH = ROOT_PATH + '/runtime/'
  BUILTIN_PATH = ROOT_PATH + '/builtin/'
  LIB_PATH     = ROOT_PATH + '/stdlib/'
  
  require PARSER_PATH + 'scheme'
  require PARSER_PATH + 'nodes'
  require RUNTIME_PATH + 'runtime'
  require ROOT_PATH + '/repl'
  
  LOAD_PATH = [LIB_PATH]
  FILE_EXT  = ".scm"
  
  class HeistError            < StandardError; end
  class RuntimeError          < HeistError; end
  class UndefinedVariable     < RuntimeError; end
  class SyntaxError           < RuntimeError; end
  class MacroError            < SyntaxError; end
  class MacroTemplateMismatch < MacroError; end
  class TypeError             < RuntimeError; end
  
  class << self
    def parse(source)
      @parser ||= SchemeParser.new
      @parser.parse(source)
    end
    
    def evaluate(expression, scope)
      Runtime::Expression === expression ?
          expression.eval(scope) :
          expression
    end
    
    def quote(arg)
      case arg
      when Runtime::Cons then
        Runtime::Cons.construct(arg) { |cell| quote(cell) }
      when Runtime::Identifier then
        arg.to_s.to_sym
      else
        arg
      end
    end
    
    def quasiquote(arg, scope)
      case arg
      when Runtime::Cons
        if Runtime::Identifier === arg.car
          name = arg.car.to_s
          return evaluate(arg.cdr.car, scope) if name == 'unquote'
          return Runtime::Cons.splice(arg.cdr.car, scope) if name == 'unquote-splicing'
        end
        Runtime::Cons.construct(arg) { |cell| quasiquote(cell, scope) }
      else
        quote(arg)
      end
    end
    
    %w[list? pair? improper? null?].each do |symbol|
      define_method(symbol) do |object|
        Runtime::Cons === object and object.__send__(symbol)
      end
    end
    
    def info(runtime)
      puts "Heist Scheme interpreter v. #{ VERSION }"
      puts "Evaluation mode: #{ runtime.lazy? ? 'LAZY' : 'EAGER' }"
      puts "Continuations enabled? #{ runtime.stackless? ? 'NO' : 'YES' }"
      puts "Macros: #{ runtime.hygienic? ? 'HYGIENIC' : 'UNHYGIENIC' }\n\n"
    end
  end
  
end

