module Heist
  class Runtime
    
    class Function
      attr_reader :body
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope   = scope
        @body    = body || block
        @formals = formals
      end
      
      def call(scope, cells)
        params, closure = [], Scope.new(@scope)
        cells.each_with_index do |arg, i|
          params[i] = closure[@formals[i]] = lazy? ?
              Binding.new(arg, scope) :
              Heist.value_of(arg, scope)
        end
        return @body.call(*params) if primitive?
        @body.map { |part| Heist.value_of(part, closure) }.last
      end
      
      def primitive?
        Proc === @body
      end
      
      def lazy?
        @scope.runtime.lazy? && !primitive?
      end
      
      def to_s
        "#<procedure>"
      end
    end
    
    class MetaFunction < Function
      def holes
        @formals
      end
      
      def call(scope, cells)
        @body.call(scope, *cells)
      end
    end
    
  end
end

