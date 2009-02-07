module Heist
  class Runtime
    
    class List < Array
      include Expression
      
      def self.from(array)
        list = new
        array.each { |item| list << item }
        list
      end
      
      def <<(element)
        element.exists_at!(self, self.size) if Expression === element
        super
      end
      
      def replace(expression)
        return unless @parent
        @parent[@index] = expression
      end
      
      def []=(index, value)
        super
        value.exists_at!(self, index) if Expression === value
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

