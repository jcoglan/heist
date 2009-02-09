require 'readline'

module Heist
  class REPL
    
    def initialize(options = {})
      @runtime = Runtime.new(options)
      reset!
    end
    
    def run
      puts "Heist Scheme interpreter, v. #{ VERSION }"
      puts "Evaluation strategy: #{ @runtime.lazy? ? 'LAZY' : 'EAGER' }\n\n"
      
      loop do
        input  = Readline.readline(prompt)
        exit if input.nil?
        
        push(input)
        tree = Heist.parse(@buffer * '')
        next if tree.nil?
        
        reset!
        begin
          result = @runtime.eval(tree)
          puts "=>  #{ result }\n\n" unless result.nil?
        rescue Exception => ex
          puts "[error] #{ ex.message }\n\n"
          puts "backtrace: " + ex.backtrace.join("\n           ") +
            "\n\n" unless Heist::RuntimeError === ex
        end
      end
    end
    
  private
    
    def reset!
      @buffer = []
    end
    
    def push(line)
      @buffer << line
      Readline::HISTORY.push(line)
    end
    
    def prompt
      @buffer.empty? ? "> " : "  " + "   " * depth
    end
    
    def depth
      source = @buffer * ''
      [/[\(\[]/, /[\)\]]/].map { |p| source.scan(p).size }.
                           inject { |a,b| a - b }
    end
    
  end
end

