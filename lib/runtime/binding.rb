module Heist
  class Runtime
    
    class Binding
      attr_reader :expression
      
      def self.create(expression, scope)
        return new(expression, scope) unless Scheme::Identifier === expression
        value = expression.eval(scope)
        Binding === value ? value : new(expression, scope)
      end
      
      def initialize(expression, scope)
        @expression, @scope = expression, scope
      end
      
      def eval(scope = nil)
        @value ||= @expression.eval(@scope)
      end
    end
    
  end
end

