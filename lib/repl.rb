require 'readline'

module Heist
  class REPL
    
    def initialize(options = {})
      @runtime, @buffer = Runtime.new(options), ""
    end
    
    def run
      puts "Heist Scheme interpreter, v. #{ VERSION }"
      puts "Evaluation strategy: #{ @runtime.lazy? ? 'LAZY' : 'EAGER' }\n\n"
      
      loop do
        inset  = @buffer.scan(/\(/).size - @buffer.scan(/\)/).size
        prompt = @buffer == "" ? "> " : "  " + "   " * inset
        input  = Readline.readline(prompt)
        exit if input.nil?
        
        Readline::HISTORY.push(input)
        @buffer << input + "\n"
        tree = Heist.parse(@buffer)
        next if tree.nil?
        
        @buffer = ""
        puts "=>  #{ stringify(@runtime.eval(tree)) rescue '[error]' }\n\n"
      end
    end
    
    def stringify(object)
      return object.to_s unless Runtime::Function === object
      return "[native code]" if object.primitive?
      
      string = "lambda (" + (object.names * " ") + ")\n"
      indent, last = 1, ""
      object.body.map do |part|
        part.text_value.split(/\n/).each do |line|
          string << ("   " * indent) + line.strip + "\n"
          indent += line.scan(/\(/).size - line.scan(/\)/).size
        end
      end
      string.strip
    end
    
  end
end

