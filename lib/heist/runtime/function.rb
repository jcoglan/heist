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
          params[i] = closure[@names[i]] = Thunk.new(arg, scope)
        end
        Proc === @body ?
            @body.call(*params.map { |p| p.eval }) :
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

