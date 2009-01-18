module Heist
  class Runtime
    
    class Function
      attr_reader :names, :body
      
      def initialize(scope, names = [], body = nil, &block)
        @scope = scope
        @body  = body || block
        @names = names.dup
      end
      
      def call(frame, scope, bindings)
        params, closure = [], Scope.new(@scope)
        bindings.each_with_index do |arg, i|
          params[i] = closure[@names[i]] = lazy? ? arg : arg.eval
        end
        return frame.push(@body.call(*params), closure) if primitive?
        @body[0...-1].each { |part| part.eval(closure) }
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
        frame.push(@body.call(frame, scope, *cells), scope)
      end
    end
    
  end
end

