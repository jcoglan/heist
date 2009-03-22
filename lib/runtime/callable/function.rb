module Heist
  class Runtime
    
    class Function
      attr_reader :body, :name
      
      def initialize(scope, formals = nil, body = nil, &block)
        @formals, @rest = [], formals
        if Cons === formals
          tail     = formals.tail.cdr
          @formals = formals.map { |id| id.to_s }
          @rest    = (Identifier === tail) ? tail : nil
        end
        @scope = scope
        @body  = body || block
        @lazy  = scope.runtime.lazy?
        @eager = !scope.runtime.stackless?
      end
      
      def name=(name)
        @name ||= name.to_s
      end
      
      def call(scope, cells)
        params = cells.map do |arg|
          lazy? ? Binding.new(arg, scope) :
                  (@eager ? arg : Heist.evaluate(arg, scope))
        end
        call_with_values(params)
      end
      
      def apply(scope, cells)
        params = Heist.evaluate(cells.car, scope).to_a
        call_with_values(params)
      end
      
      def call_with_values(params)
        return @body.call(*params) if primitive?
        closure = Scope.new(@scope)
        idx = 0
        @formals.each do |name|
          closure[name] = params[idx]
          idx += 1
        end
        closure[@rest] = Cons.construct(params[idx..-1]) if @rest
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
      alias :inspect :to_s
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

