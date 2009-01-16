module Heist
  class Runtime
    
    class Function
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
        if primitive?
          frame.send(@body.call(*params))
        else
          bindings = @body.arguments.map { |arg| Binding.new(arg, closure) }
          frame.push(@body.function(closure), closure, bindings)
        end
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
      def call(frame, scope, bindings)
        cells = bindings.map { |b| b.expression }
        frame.send(@body.call(frame, scope, *cells))
      end
    end
    
  end
end

