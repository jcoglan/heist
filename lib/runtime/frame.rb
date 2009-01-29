module Heist
  class Runtime
    
    class Frame
      def initialize(list, scope)
        @current = Binding.new(list, scope)
        @stack = scope.runtime.stack
      end
      
      def evaluate
        @stack << self
        expand! while Binding === @current
        @stack.pop
        @current
      end
      
    private
      
      def expand!
        list, scope = @current.expression, @current.scope
        case list
        
        when Identifier then
          @current = @current.extract
        
        when List then
          first = Heist.value_of(list.first, scope)
          unless Function === first
            rest = list.rest.map { |cell| Heist.value_of(cell, scope) }
            return @current = List.new([first] + rest)
          end
          # puts ". " * @stack.size + "(#{list.first})" if Identifier === list.first
          @current = first.call(scope, list.rest)
          
          return unless Macro::Expansion === @current
          log = list.first.to_s == 'rotate'
          puts "\n\nBEFORE: #{list.parent}" if log
          puts "MACRO: #{list}, #{list.index}" if log
          puts "REPLACEMENT: #{@current.expression}" if log
          list.replace(@current.expression)
          puts "AFTER: #{list.parent}\n\n" if log
          @current = Binding.new(@current.expression, scope)
        
        else
          @current = list
        end
      end
    end
    
  end
end

