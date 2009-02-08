module Heist
  class Runtime
    
    class Frame
      attr_reader :expression
      
      def initialize(expression, scope)
        reset!(expression)
        @scope = scope
      end
      
      def complete?
        @complete
      end
      
      def process!
        puts "PROCESS: #{@expression} :: #{@values * ', '}"
        case @expression
        
          when List then
            if MetaFunction === @values.first or @values.size == @expression.size
              @complete = true
              func = @values.first
              
              merge!
              
              return List.new(@data) unless Function === func
              
              result = @data.first.call(@scope, @data[1..-1])
              return result unless Macro::Expansion === result
              reset!(result.expression, true)
              return process!
            end
            
            stack = @scope.runtime.stack
            stack << Frame.new(@expression[@values.size], @scope)
        
          when Identifier then
            @complete = true
            @scope[@expression]
        
          else
            @complete = true
            Heist.value_of(@expression, @scope)
        end
      end
      
      def dup
        copy, values = super, @values.dup
        copy.instance_eval { @values = values }
        copy
      end
      
      def fill!(value)
        puts "FILL! #{value}"
        @values << value
      end
      
    private
      
      def merge!
        return @data = @values unless MetaFunction === @values.first
        @data = @expression.dup
        holes = @values.first.holes
        @values.each_with_index do |value, i|
          index = holes[i] || i
          @data[index] = value
        end
        puts "MERGED: #{@data}"
      end
      
      def reset!(expression, replace = false)
        @expression.replace(expression) if replace
        @expression = expression
        @values     = []
        @complete   = false
      end
    end
    
  end
end

