module Heist
  class Runtime
    
    class Function
      attr_reader :body
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope   = scope
        @body    = body || block
        @formals = formals.map { |f| f.to_s }
      end
      
      def call(scope, cells)
        params, closure = [], Scope.new(@scope)
        cells.each_with_index do |arg, i|
          params[i] = closure[@formals[i]] = lazy? ?
              Binding.new(arg, scope) :
              Heist.value_of(arg, scope)
        end
        return @body.call(*params) if primitive?
        @body[0...-1].each { |part| Heist.value_of(part, closure) }
        Binding.new(@body.last, closure)
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
      def call(scope, cells)
        @body.call(scope, *cells)
      end
    end
    
  end
end

