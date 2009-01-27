module Heist
  class Runtime
    
    class Binding
      attr_reader :expression, :scope
      
      def initialize(expression, scope)
        @expression = expression
        @scope      = scope
      end
      
      def extract
        @value ||= Heist.value_of(@expression, @scope)
      end
    end
    
  end
end

