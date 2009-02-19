module Heist
  class Runtime
    
    class Scope
      def initialize(parent = nil)
        @parent = parent || {}
        @symbols = {}
        @start_time = Time.now.to_f
      end
      
      def [](name)
        @symbols[name] || @parent[name]
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
      
      def eval(source)
        Heist.parse(source).eval(self)
      end
      
      def elapsed_time
        (Time.now.to_f - @start_time) * 1000000
      end
    end
    
  end
end

