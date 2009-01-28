require 'forwardable'

module Heist
  class Runtime
    
    class List < Array
      attr_reader :parent, :index
      
      def exists_at!(parent, index)
        @parent, @index = parent, index
      end
      
      def <<(element)
        element.exists_at!(self, self.size) if List === element
        super
      end
      
      def eval(scope)
        value = Frame.new(self, scope).evaluate
        return value unless Macro::Expansion === value
        parent[index] = value.expression if parent
        Heist.value_of(value.expression, scope)
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

