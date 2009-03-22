require 'forwardable'
require 'rational'
require 'complex'

require 'rubygems'
require 'treetop'

module Heist
  VERSION = '0.1.0'
  
  ROOT_PATH    = File.expand_path(File.dirname(__FILE__))
  PARSER_PATH  = ROOT_PATH + '/parser/'
  RUNTIME_PATH = ROOT_PATH + '/runtime/'
  BUILTIN_PATH = ROOT_PATH + '/builtin/'
  LIB_PATH     = ROOT_PATH + '/stdlib/'
  
  require PARSER_PATH + 'ruby'
  require PARSER_PATH + 'scheme'
  require PARSER_PATH + 'nodes'
  require RUNTIME_PATH + 'runtime'
  require ROOT_PATH + '/repl'
  
  LOAD_PATH = [LIB_PATH]
  FILE_EXT  = ".scm"
  
  QUOTE = Runtime::Identifier.new('quote')
  ELSE  = Runtime::Identifier.new('else')
  PIPE  = Runtime::Identifier.new('=>')
  UNQ   = Runtime::Identifier.new('unquote')
  UNQS  = Runtime::Identifier.new('unquote-splicing')
  
  class HeistError            < StandardError; end
  class RuntimeError          < HeistError; end
  class UndefinedVariable     < RuntimeError; end
  class SyntaxError           < RuntimeError; end
  class MacroError            < SyntaxError; end
  class MacroTemplateMismatch < MacroError; end
  class TypeError             < RuntimeError; end
  
  class << self
    def parse(source)
      @scheme ||= SchemeParser.new
      @ruby   ||= RubyParser.new
      parser = (String === source) ? @scheme : @ruby
      parser.parse(source)
    end
    
    def evaluate(expression, scope)
      Runtime::Expression === expression ?
          expression.eval(scope) :
          expression
    end
    
    def complex(real, imaginary)
      Complex.new(real, imaginary)
    end
    
    def rational(numerator, denominator)
      Rational(numerator, denominator)
    end
    
    def quasiquote(arg, scope)
      case arg
        when Runtime::Cons
          if Runtime::Identifier === arg.car
            name = arg.car
            return evaluate(arg.cdr.car, scope) if name == UNQ
            return Runtime::Cons.splice(arg.cdr.car, scope) if name == UNQS
          end
          Runtime::Cons.construct(arg) { |cell| quasiquote(cell, scope) }
        else
          arg
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

