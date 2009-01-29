require 'forwardable'

module Heist
  class Runtime
    
    class List < Array
      def exists_at!(parent, index)
        @parent, @index = parent, index
      end
      
      def <<(element)
        element.exists_at!(self, self.size) if List === element
        super
      end
      
      def eval(scope)
        Frame.new(self, scope).evaluate
      end
      
      def replace(expression)
        return unless @parent
        @parent[@index] = expression
      end
      
      def rest
        self[1..-1]
      end
      
      def to_s
        '(' + collect { |cell| cell.to_s } * ' ' + ')'
      end
      
      alias :inspect :to_s
    end
    
  end
end

