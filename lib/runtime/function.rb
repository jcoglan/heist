module Heist
  class Runtime
    
    class Function
      attr_reader :body
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope   = scope
        @body    = body || block
        @formals = formals.map { |f| f.to_s }
      end
      
      def call(scope, bindings)
        params, closure = [], Scope.new(@scope)
        bindings.each_with_index do |arg, i|
          params[i] = closure[@formals[i]] = lazy? ? arg : arg.extract
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
    end
    
    class MetaFunction < Function
      def call(scope, bindings)
        cells = bindings.map { |b| b.expression }
        @body.call(scope, *cells)
      end
    end
    
  end
end

