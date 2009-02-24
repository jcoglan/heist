module Heist
  class Runtime
    
    class Function
      attr_reader :body, :name
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope   = scope
        @body    = body || block
        @formals = formals.map { |id| id.to_s }
      end
      
      def name=(name)
        @name ||= name.to_s
      end
      
      def call(scope, cells)
        params, closure = [], Scope.new(@scope)
        cells.each_with_index do |arg, i|
          params[i] = closure[@formals[i]] = lazy? ?
              Binding.new(arg, scope) :
              Heist.evaluate(arg, scope)
        end
        return @body.call(*params) if primitive?
        Body.new(@body, closure)
      end
      
      def primitive?
        Proc === @body
      end
      
      def lazy?
        @scope.runtime.lazy? && !primitive?
      end
      
      def to_s
        "#<procedure:#{ @name }>"
      end
    end
    
    class Syntax < Function
      def call(scope, cells)
        @body.call(scope, cells)
      end
      
      def to_s
        "#<syntax:#{ @name }>"
      end
    end
    
  end
end

