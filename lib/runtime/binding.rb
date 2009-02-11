module Heist
  class Runtime
    
    class Binding
      include Expression
      
      attr_reader :expression, :scope
      
      def initialize(expression, scope)
        @expression = expression
        @scope      = scope
      end
      
      def extract
        @value ||= Heist.value_of(@expression, @scope)
      end
      
      def eval(scope)
        extract
      end
      
      def to_s
        @expression.to_s
      end
    end
    
  end
end

