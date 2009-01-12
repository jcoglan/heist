module Heist
  module Runtime
    
    class Scope
      def initialize(parent = nil)
        @parent = parent || TopLevel.new
        @symbols = {}
      end
      
      def [](name)
        @symbols[name] || @parent[name]
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
    end
    
    class TopLevel < Scope
      def initialize(*args)
        @symbols = {}
        @parent  = {}
        Builtins.add(self)
      end
    end
    
  end
end

