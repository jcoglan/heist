module Heist
  class Runtime
    
    class Function
      attr_reader :body, :name
      
      def initialize(scope, formals = [], body = nil, &block)
        @scope, @formals = scope, formals
        @formals = @formals.map { |id| id.to_s } if Enumerable === @formals
        @body    = body || block
        @lazy    = scope.runtime.lazy?
        @eager   = !scope.runtime.stackless?
      end
      
      def name=(name)
        @name ||= name.to_s
      end
      
      def call(scope, cells)
        closure = Scope.new(@scope)
        params = cells.map do |arg|
          lazy? ? Binding.new(arg, scope) :
                  (@eager ? arg : Heist.evaluate(arg, scope))
        end
        if Array === @formals
          params.each_with_index do |param, i|
            closure[@formals[i]] = param
          end
        else
          closure[@formals] = Cons.construct(params)
        end
        return @body.call(*params) if primitive?
        Body.new(@body, closure)
      end
      
      def primitive?
        Proc === @body
      end
      
      def lazy?
        @lazy and not primitive?
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

