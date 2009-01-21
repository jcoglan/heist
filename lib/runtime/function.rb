module Heist
  class Runtime
    
    class Function
      attr_reader :body
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope   = scope
        @body    = body || block
        @formals = formals.map { |f| f.to_s }
      end
      
      def call(frame, scope, bindings)
        params, closure = [], Scope.new(@scope)
        bindings.each_with_index do |arg, i|
          params[i] = closure[@formals[i]] = lazy? ? arg : arg.extract
        end
        return frame.push(@body.call(*params)) if primitive?
        @body[0...-1].each { |part| Heist.value_of(part, closure) }
        frame.push(@body.last, closure)
      end
      
      def primitive?
        Proc === @body
      end
      
      def lazy?
        @scope.runtime.lazy? && !primitive?
      end
    end
    
    class MetaFunction < Function
      def call(frame, scope, bindings)
        cells = bindings.map { |b| b.expression }
        frame.push(@body.call(frame, scope, *cells))
      end
    end
    
  end
end

