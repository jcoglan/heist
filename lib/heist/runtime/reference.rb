module Heist
  class Runtime
    
    class Reference
      def initialize(expression, scope)
        @expression, @scope = expression, scope
      end
      
      def eval
        @value ||= @expression.eval(@scope)
      end
    end
    
  end
end

