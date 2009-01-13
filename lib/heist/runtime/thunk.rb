module Heist
  class Runtime
    
    class Thunk
      def initialize(expression, scope)
        @expression, @scope = expression, scope
      end
      
      def eval
        @value ||= @expression.eval(@scope)
      end
    end
    
  end
end

