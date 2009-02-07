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
      
      def eval(scope)
        func = Heist.value_of(first, scope)
        
        unless Function === func
          others = rest.map { |cell| Heist.value_of(cell, scope) }
          return List.new([func] + others)
        end
        
        result = func.call(scope, rest)
        return result unless Macro::Expansion === result
        
        replace(result.expression)
        result = Heist.value_of(result.expression, scope)
        result
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

