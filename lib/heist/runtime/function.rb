module Heist
  class Runtime
    
    class Function
      def initialize(scope, names = [], body = nil, &block)
        @scope = scope
        @body  = body || block
        @names = names.dup
      end
      
      def call(scope, *args)
        params, closure = [], Scope.new(@scope)
        args.each_with_index do |arg, i|
          params[i] = closure[@names[i]] = Reference.new(arg, scope)
        end
        Proc === @body ?
            @body.call(*params) :
            @body.eval(closure)
      end
    end
    
    class MetaFunction < Function
      def call(scope, *args)
        @body.call(scope, *args)
      end
    end
    
  end
end

