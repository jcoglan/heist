require 'readline'

module Heist
  class REPL
    
    def initialize(options = {})
      @runtime = Runtime.new(options)
      @scope = Runtime::FileScope.new(@runtime.top_level, File.expand_path('.'))
      @results = []
      
      @runtime.define('~') { |x| @results[-x] }
      
      reset!
    end
    
    def run
      puts @runtime.info
      
      Readline.completion_append_character = nil
      Readline.completion_proc = @runtime.top_level.method(:longest_prefix)
      
      loop do
        begin
          input  = Readline.readline(prompt)
          exit if input.nil?
          
          push(input)
          tree = Heist.parse(@buffer * "\n")
          next if tree.nil?
          
          reset!
          result = @scope.eval(tree)
          unless result.nil?
            puts "; => #{ Heist.stringify(result) }\n\n"
            @results << result
            @scope['^'] = result
          end
          
        rescue Exception => ex
          return if SystemExit === ex
          if Interrupt === ex
            reset! and puts "\n\n"
          else
            puts "; [error] #{ ex.message }\n\n"
          end
          
        # debugging
        #  puts "; backtrace: " + ex.backtrace.join("\n;            ") +
        #    "\n\n" unless Heist::RuntimeError === ex
        end
      end
    end
    
  private
  
    INDENT  = 2
    SPECIAL = %w[define lambda let let* letrec
                 define-syntax syntax-rules
                 let-syntax letrec-syntax]
    
    def reset!
      @buffer = []
      @open   = []
      @indent = 0
    end
    
    def push(line)
      old_depth = depth
      @buffer << line
      Readline::HISTORY.push(line)
      new_depth = depth
      
      return if new_depth == old_depth
      return @open.slice!(new_depth..-1) if new_depth < old_depth
      
      code = line.dup
      while code == code.gsub!(/\([^\(\)\[\]]*\)|\[[^\(\)\[\]]*\]/, ''); end
      calls = code.scan(/[\(\[](?:[^\(\)\[\]\s]*\s*)/).flatten
      
      offsets  = calls.inject([]) do |list, call|
        index  = list.empty? ? 0 : list.last.last - @indent
        index  = @indent + (line.index(call, index) || 0)
        rindex = index + call.length
        eol    = (rindex == line.length + @indent)
        list << [call[1..-1], eol, index, rindex]
        list
      end
      
      @open = @open + offsets
    end
    
    def indent
      return 0 if @buffer.empty? or @open.empty?
      open = @open.last
      @indent = (SPECIAL.include?(open[0].strip) or open[1]) ?
                open[2] + INDENT :
                open[3]
    end
    
    def prompt
      " " * indent
    end
    
    def depth
      source = @buffer * ' '
      [/[\(\[]/, /[\)\]]/].map { |p| source.scan(p).size }.
                           inject { |a,b| a - b }
    end
    
  end
end

