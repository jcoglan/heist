module Heist
  class Runtime
    class Macro
      
      class Expansion
        attr_reader :expression
        def initialize(expression)
          @expression = expression
        end
      end
      
    end
  end
end

