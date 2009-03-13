module Heist
  class Runtime
    
    module Expression
      attr_accessor :parent
      
      def replace(expression)
        return unless @parent
        @parent.car = expression
        @parent.hosts(expression)
      end
      
      def eval(scope)
        scope.runtime.stack << Frame.new(self, scope)
      end
    end
    
  end
end

