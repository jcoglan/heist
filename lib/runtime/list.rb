require 'forwardable'

module Heist
  class Runtime
    
    class List < Array
      def eval(scope)
        Frame.new(self, scope).evaluate
      end
      
      def rest
        self[1..-1]
      end
      
      def to_s
        '(' + collect { |cell| cell.to_s } * ' ' + ')'
      end
      
      alias :inspect :to_s
    end
    
  end
end

