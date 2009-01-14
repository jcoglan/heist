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
        
          params[i] = closure[@names[i]] =
              arg.respond_to?(:eval) ?
                  (lazy? ? Thunk.new(arg, scope) : arg.eval(scope)) :
                  arg
        end
        primitive? ?
            @body.call(*params) :
            @body.eval(closure)
      end
      
      def primitive?
        Proc === @body
      end
      
      def lazy?
        @scope.runtime.lazy? && !primitive?
      end
      
      def to_s
        return @string if @string
        return "[native code]" if primitive?
        
        string = "lambda (" + (@names * " ") + ")\n"
        indent, last = 1, ""
        @body.text_value.split(/\n/).each do |line|
          string << ("   " * indent) + line.strip + "\n"
          indent += line.scan(/\(/).size - line.scan(/\)/).size
        end
        @string = string.strip
      end
    end
    
    class MetaFunction < Function
      def call(scope, *args)
        @body.call(scope, *args)
      end
    end
    
  end
end

