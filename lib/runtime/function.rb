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
          arg = arg.eval if primitive? or not lazy?
          params[i] = closure[@names[i]] = arg
        end
        frame.push(primitive? ? @body.call(*params) : @body, closure)
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

