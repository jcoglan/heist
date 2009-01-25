module Heist
  class Runtime
    
    class Binding
      attr_reader :expression, :scope
      
      def initialize(expression, scope, memoized = true)
        @expression = expression
        @scope      = scope
        @memoized   = !!memoized
      end
      
      def extract
        return @value if defined?(@value)
        value = Heist.value_of(@expression, @scope)
        @value = value if @memoized
        value
      end
    end
    
  end
end

