module Heist
  class Runtime
    
    class Identifier
      def initialize(name)
        @name = name.to_s
      end
      
      def eval(scope)
        scope[@name]
      end
      
      def to_s
        @name.to_s
      end
    end
    
  end
end

