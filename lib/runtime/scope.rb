module Heist
  class Runtime
    
    class Scope
      def initialize(parent)
        @symbols = {}
        return @parent = parent unless Runtime === parent
        @runtime = parent
        @parent  = {}
      end
      
      def runtime
        @runtime || @parent.runtime
      end
      
      def [](name)
        value = @symbols.has_key?(name) ?
                @symbols[name] :
                @parent[name]
        value = value.eval if Thunk === value
        value
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
      
      def eval(source)
        source = Heist.parse(source) if String === source
        source.eval(self)
      end
    end
    
  end
end

