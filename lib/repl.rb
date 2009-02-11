require 'readline'

module Heist
  class REPL
    
    def initialize(options = {})
      @runtime, @buffer = Runtime.new(options), ""
    end
    
    def run
      puts "Heist Scheme interpreter, v. #{ VERSION }"
      puts "Evaluation strategy: #{ @runtime.lazy? ? 'LAZY' : 'EAGER' }"
      puts "Continuations enabled: #{ @runtime.stackless? ? 'no' : 'yes' }\n\n"
      
      loop do
        inset  = @buffer.scan(/[\(\[]/).size - @buffer.scan(/[\)\]]/).size
        prompt = @buffer == "" ? "> " : "  " + "   " * inset
        input  = Readline.readline(prompt)
        exit if input.nil?
        
        Readline::HISTORY.push(input)
        @buffer << input + "\n"
        tree = Heist.parse(@buffer)
        next if tree.nil?
        
        @buffer = ""
        puts "=>  #{ @runtime.eval(tree) }\n\n"
      end
    end
    
  end
end

