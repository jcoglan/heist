module Heist
  class Runtime
    
    class Function
      def initialize(scope, names = [], body = nil, &block)
        @scope = Scope.new(scope)
        @body  = body || block
        @names = names.dup
      end
      
      def call(scope, *args)
        params = []
        args.each_with_index do |arg, i|
          params[i] = @scope[@names[i]] = arg.eval(scope)
        end
        Proc === @body ? @body.call(*params) : @body.eval(@scope)
      end
    end
    
    class MetaFunction < Function
      def call(scope, *args)
        @body.call(scope, *args)
      end
    end
    
  end
end

