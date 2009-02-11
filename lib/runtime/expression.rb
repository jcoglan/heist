module Heist
  class Runtime
    
    module Expression
      attr_reader :parent, :index
      
      def exists_at!(parent, index)
        @parent, @index = parent, index
      end
      
      def replace(expression)
        return unless @parent
        @parent[@index] = expression
      end
      
      def eval(scope)
        scope.runtime.stack << Frame.new(self, scope)
      end
    end
    
  end
end

