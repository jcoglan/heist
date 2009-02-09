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
        tree = Heist.parse(@buffer * ' ')
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
  
    INDENT  = 2
    SPECIAL = %w[define lambda]
    
    def reset!
      @buffer = []
      @indents = []
    end
    
    def push(line)
      old_depth = depth
      @buffer << line
      Readline::HISTORY.push(line)
      new_depth = depth
      
      return if new_depth == old_depth
      return @indents.slice!(new_depth..-1) if new_depth < old_depth
      
      calls  = line.scan(/[\(\[]([^\(\)\[\]\s]+|.)/).flatten
      name   = calls[new_depth - old_depth - 1]
      index  = line.rindex(name)
      name   = name.gsub(/[\(\)\[\]\s]/, '')
      offset = index + name.length
      @indents[new_depth - 1] = (offset == line.length) ?
                                index + INDENT :
                                indent_for(name, index)
    end
    
    def indent_for(name, index)
      return index if name.empty?
      return index + INDENT if SPECIAL.include?(name)
      index + name.length + 1
    end
    
    def prompt
      @buffer.empty? ? "> " :
      "  " + @indents.map { |x| " " * (x || 0) }.join('')
    end
    
    def depth
      source = @buffer * ' '
      [/[\(\[]/, /[\)\]]/].map { |p| source.scan(p).size }.
                           inject { |a,b| a - b }
    end
    
  end
end

