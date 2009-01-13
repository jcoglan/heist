module Heist
  class Runtime
    
    class Scope
      def initialize(parent = nil)
        @parent = parent || {}
        @symbols = {}
      end
      
      def [](name)
        value = @symbols[name] || @parent[name]
        value = value.eval if Reference === value
        value
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
      
      def eval(source)
        Heist.parse(source).eval(self)
      end
    end
    
  end
end

