module Heist
  class Runtime
    
    class Binding
      attr_reader :expression
      
      def initialize(expression, scope)
        @expression, @scope = expression, scope
      end
      
      def eval(scope = nil)
        @value ||= Heist.value_of(@expression, @scope)
      end
    end
    
  end
end

